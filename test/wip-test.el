;;; wip-test.el --- Tests for wip.el -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for wip.el.  Run with `eldev test'.

;;; Code:

(require 'ert)
(require 'dired)
(require 'wip)

(defmacro wip-test--fixture (&rest body)
  "Run BODY with isolated wip state, cleaning up buffers afterwards."
  (declare (indent 0) (debug t))
  `(let ((wip-directory (make-temp-file "wip-test-" t))
         (wip--wips nil)
         (wip--current nil)
         (wip--known-buffers (make-hash-table :test #'eq :weakness 'key))
         (wip--global-window-config nil)
         (wip--global-tabs nil)
         (switch-to-prev-buffer-skip switch-to-prev-buffer-skip)
         (wip--saved-prev-buffer-skip nil)
         (wip-test--initial-buffers (buffer-list)))
     (unwind-protect
         (progn ,@body)
       (remove-hook 'buffer-list-update-hook #'wip--on-buffer-list-update)
       (dolist (buffer (buffer-list))
         (unless (memq buffer wip-test--initial-buffers)
           (when (buffer-live-p buffer)
             (kill-buffer buffer))))
       (delete-directory wip-directory t))))

(defun wip-test--wip (name)
  "Return the active wip struct called NAME, or nil."
  (cdr (assoc name wip--wips)))

(ert-deftest wip-test-create-and-switch ()
  (wip-test--fixture
    (wip "alpha")
    (should wip--current)
    (should (equal (wip--wip-name wip--current) "alpha"))
    (should (wip-test--wip "alpha"))
    (wip "beta")
    (should (equal (wip--wip-name wip--current) "beta"))
    ;; Most recently used first.
    (should (equal (mapcar #'car wip--wips) '("beta" "alpha")))
    (wip "alpha")
    (should (equal (mapcar #'car wip--wips) '("alpha" "beta")))))

(ert-deftest wip-test-stale-struct-upgraded-on-entry ()
  "Wip records from an older wip.el layout are upgraded, not crashed on.
Reloading wip.el after a struct change leaves stale records in
`wip--wips'; creating or entering a wip used to signal
args-out-of-range on them.  The name and live buffers survive the
upgrade; the rest of the state is rebuilt lazily."
  (wip-test--fixture
    (let* ((buffer (generate-new-buffer "wip-test-old-member"))
           ;; The original 7-slot layout:
           ;; [wip--wip name buffers window-config tabs pad pad-name]
           (stale (record 'wip--wip "legacy" (list buffer)
                          nil nil nil nil)))
      (push (cons "legacy" stale) wip--wips)
      ;; Creating a NEW wip must not crash on the stale record either.
      (wip "fresh")
      (wip "legacy")
      (should (wip--wip-compatible-p wip--current))
      (should (equal (wip--wip-name wip--current) "legacy"))
      (should (memq buffer (wip--wip-buffers wip--current))))))

(ert-deftest wip-test-stale-current-upgraded-by-tracking-hook ()
  "The tracking hook upgrades a stale `wip--current' instead of erroring.
After a reload mid-wip, the very next buffer event goes through
`wip--on-buffer-list-update'; it must survive the old record."
  (wip-test--fixture
    (wip "alpha")
    ;; Simulate a reload: swap the live struct for an old-layout one.
    (let ((stale (record 'wip--wip "alpha" nil nil nil nil nil)))
      (setcdr (assoc "alpha" wip--wips) stale)
      (setq wip--current stale)
      (let ((buffer (generate-new-buffer "wip-test-post-reload")))
        (should (wip--wip-compatible-p wip--current))
        (should (memq buffer (wip--wip-buffers wip--current)))))))

(ert-deftest wip-test-blank-name-rejected ()
  (wip-test--fixture
    (should-error (wip "  ") :type 'user-error)))

(ert-deftest wip-test-new-buffer-auto-added ()
  (wip-test--fixture
    (wip "alpha")
    (let ((buffer (generate-new-buffer "wip-test-new")))
      (should (memq buffer (wip--wip-buffers wip--current))))))

(ert-deftest wip-test-internal-buffer-not-added ()
  (wip-test--fixture
    (wip "alpha")
    (let ((buffer (generate-new-buffer " wip-test-internal")))
      (should-not (memq buffer (wip--wip-buffers wip--current))))))

(ert-deftest wip-test-preexisting-buffer-not-added ()
  "A pre-existing buffer that is never displayed is not adopted at wip entry.
A displayed pre-existing buffer IS adopted; see
`wip-test-selected-preexisting-buffer-joins-wip'."
  (wip-test--fixture
    (let ((buffer (generate-new-buffer "wip-test-preexisting")))
      (wip "alpha")
      (should-not (memq buffer (wip--wip-buffers wip--current))))))

(ert-deftest wip-test-buffers-scoped-per-wip ()
  (wip-test--fixture
    (wip "alpha")
    (let ((in-alpha (generate-new-buffer "wip-test-alpha")))
      (wip "beta")
      (let ((in-beta (generate-new-buffer "wip-test-beta")))
        (should (memq in-beta (wip--wip-buffers (wip-test--wip "beta"))))
        (should-not (memq in-alpha (wip--wip-buffers (wip-test--wip "beta"))))
        (should (memq in-alpha (wip--wip-buffers (wip-test--wip "alpha"))))))))

(ert-deftest wip-test-dired-fresh-buffer-joins-wip ()
  "A dired buffer created from within a wip joins the wip.
The buffer is genuinely new, so creation tracking picks it up
directly, independent of selection adoption."
  (wip-test--fixture
    (wip "alpha")
    (dired temporary-file-directory)
    (should (eq major-mode 'dired-mode))
    (should (memq (current-buffer) (wip--wip-buffers wip--current)))))

(ert-deftest wip-test-dired-reused-buffer-joins-wip ()
  "Opening dired from within a wip adds its buffer even when reused.
When the directory was already visited before entering the wip,
`dired' reuses that pre-existing buffer instead of creating one.
The user still \"opened dired from within the wip\", so the
display-adoption rule brings the reused buffer into the wip."
  (wip-test--fixture
    ;; Visit the directory once, outside any wip.
    (let ((buffer (dired-noselect temporary-file-directory)))
      (wip "alpha")
      (dired temporary-file-directory)
      ;; dired reused the pre-existing buffer rather than creating one.
      (should (eq (current-buffer) buffer))
      (should (memq buffer (wip--wip-buffers wip--current))))))

(ert-deftest wip-test-find-file-fresh-buffer-joins-wip ()
  "A file buffer freshly created from within a wip joins the wip.
Contrast case for `wip-test-find-file-reused-buffer-joins-wip': the
buffer is genuinely new, so creation tracking picks it up directly."
  (wip-test--fixture
    (let ((file (make-temp-file "wip-test-file-")))
      (unwind-protect
          (progn
            (wip "alpha")
            (find-file file)
            (should (memq (current-buffer)
                          (wip--wip-buffers wip--current))))
        (delete-file file)))))

(ert-deftest wip-test-find-file-reused-buffer-joins-wip ()
  "Opening a file from within a wip adds its buffer even when reused.
When the file was already visited before entering the wip, `find-file'
reuses the pre-existing buffer instead of creating one, so creation
tracking never sees it; the display-adoption rule brings it in
instead.  (Regression: previously such a buffer was absent from
`C-x b' entirely.)"
  (wip-test--fixture
    (let ((file (make-temp-file "wip-test-file-")))
      (unwind-protect
          ;; Visit the file once, outside any wip.
          (let ((buffer (find-file-noselect file)))
            (wip "alpha")
            (find-file file)
            ;; find-file reused the pre-existing buffer.
            (should (eq (current-buffer) buffer))
            (should (memq buffer (wip--wip-buffers wip--current))))
        (delete-file file)))))

(ert-deftest wip-test-selected-preexisting-buffer-joins-wip ()
  "A pre-existing buffer selected while in a wip joins the wip.
User scenario: the vterm buffer you are working in existed before the
wip; being in it makes it part of the wip via the display-adoption
rule, so `C-x b' from the next buffer offers it."
  (wip-test--fixture
    (let ((term (generate-new-buffer "wip-test-term")))
      (wip "alpha")
      (switch-to-buffer term)
      (should (eq (current-buffer) term))
      (should (memq term (wip--wip-buffers wip--current))))))

(ert-deftest wip-test-switch-buffer-offers-previous-buffer-first ()
  "`wip-ido-switch-buffer' offers candidates in most-recently-used order.
Like `ido-switch-buffer', the buffer you were just in is the first
(default) candidate, even when other wip buffers were created more
recently."
  (wip-test--fixture
    (wip "alpha")
    (let ((prev (generate-new-buffer "wip-test-prev")))
      ;; Two buffers created after PREV, so PREV is not the newest.
      (generate-new-buffer "wip-test-mid1")
      (generate-new-buffer "wip-test-mid2")
      ;; Work in PREV, then open a new buffer from there.
      (switch-to-buffer prev)
      (switch-to-buffer (generate-new-buffer "wip-test-new"))
      (let (captured)
        (let ((stub (lambda (_prompt choices &rest _)
                      (setq captured choices)
                      (car choices))))
          (advice-add 'ido-completing-read :override stub)
          (unwind-protect
              (wip-ido-switch-buffer)
            (advice-remove 'ido-completing-read stub)))
        ;; The previously used buffer must be the first candidate.
        (should (equal (car captured) "wip-test-prev"))))))

(ert-deftest wip-test-evict-removes-buffer-from-windows ()
  "Evicting a displayed buffer removes it from the wip's windows.
The buffer survives globally (eviction never kills), but the window
showing it switches to something else, like `kill-buffer' does for
windows."
  (wip-test--fixture
    (wip "alpha")
    (let ((buffer (generate-new-buffer "wip-test-evictee")))
      (switch-to-buffer buffer)
      (should (eq (window-buffer) buffer))
      (let ((stub (lambda (&rest _) (buffer-name buffer))))
        (advice-add 'ido-completing-read :override stub)
        (unwind-protect
            (wip-ido-kill-buffer)
          (advice-remove 'ido-completing-read stub)))
      ;; Evicted and still alive...
      (should-not (memq buffer (wip--wip-buffers wip--current)))
      (should (buffer-live-p buffer))
      ;; ...but no longer shown in any window.
      (should-not (get-buffer-window buffer)))))

(ert-deftest wip-test-prefixed-kill-runs-normal-kill-buffer ()
  "A prefix argument makes `wip-ido-kill-buffer' run `kill-buffer'."
  (wip-test--fixture
    (wip "alpha")
    (let ((buffer (generate-new-buffer "wip-test-kill")))
      (let ((read-buffer-function
             (lambda (&rest _args) (buffer-name buffer))))
        (wip-ido-kill-buffer '(4)))
      (should-not (buffer-live-p buffer)))))

(ert-deftest wip-test-evict-keeps-buffer-alive ()
  (wip-test--fixture
    (wip "alpha")
    (let ((buffer (generate-new-buffer "wip-test-evictee")))
      (wip--remove-buffer buffer wip--current)
      (should-not (memq buffer (wip--wip-buffers wip--current)))
      (should (buffer-live-p buffer)))))

(ert-deftest wip-test-evicted-buffer-not-retracked ()
  (wip-test--fixture
    (wip "alpha")
    (let ((buffer (generate-new-buffer "wip-test-evictee")))
      (wip--remove-buffer buffer wip--current)
      ;; Creating another buffer re-runs the tracking scan; the
      ;; evicted buffer is already known (defeating creation
      ;; tracking) and not shown in any window, so it must not come
      ;; back.  (Even when displayed, eviction is sticky; see
      ;; `wip-test-evicted-buffer-not-readopted-when-redisplayed'.)
      (generate-new-buffer "wip-test-other")
      (should-not (memq buffer (wip--wip-buffers wip--current))))))

(ert-deftest wip-test-displayed-in-other-window-joins-wip ()
  "Any buffer displayed in one of the wip's windows joins the wip.
The membership rule is generic — display in any window counts, not
just selection in the selected window."
  (wip-test--fixture
    (let ((popup (generate-new-buffer "wip-test-popup")))
      (wip "alpha")
      (display-buffer popup)
      (generate-new-buffer "wip-test-event")
      (should (memq popup (wip--wip-buffers wip--current))))))

(ert-deftest wip-test-kill-wip-spares-adopted-buffers ()
  "`wip-kill' never kills buffers merely adopted from outside.
Only buffers the wip created (owns) are candidates for the
exclusive-buffer sweep."
  (wip-test--fixture
    (let ((adopted (generate-new-buffer "wip-test-adopted")))
      (wip "alpha")
      (switch-to-buffer adopted)
      (should (memq adopted (wip--wip-buffers wip--current)))
      (wip-kill "alpha")
      (should (buffer-live-p adopted)))))

(ert-deftest wip-test-empty-wip-restore-lands-on-pad ()
  "Restoring a wip whose members all died lands on a fresh pad.
The dead-buffer substitute Emacs picks (possibly another wip's pad)
must not stay in the window nor join the wip."
  (wip-test--fixture
    (wip "alpha")
    (let ((alpha-pad (wip--get-pad wip--current)))
      (wip "beta")
      (let ((beta-pad (wip--get-pad wip--current)))
        (kill-buffer alpha-pad)
        (wip "alpha")
        (generate-new-buffer "wip-test-event")
        (should (memq (window-buffer) (wip--wip-buffers wip--current)))
        (should-not (memq beta-pad (wip--wip-buffers wip--current)))))))

(ert-deftest wip-test-evict-sole-member-lands-on-pad ()
  "Evicting the only remaining member gives the window a fresh pad."
  (wip-test--fixture
    (wip "alpha")
    (let ((pad (wip--get-pad wip--current))
          (member (generate-new-buffer "wip-test-member")))
      (switch-to-buffer member)
      (kill-buffer pad)
      (wip--evict-buffer member wip--current)
      (should (memq (window-buffer) (wip--wip-buffers wip--current)))
      (should-not (eq (window-buffer) member)))))

(ert-deftest wip-test-kill-sole-member-adopts-fallback ()
  "Killing the last wip buffer adopts whatever Emacs shows next.
Membership is generic: the buffer now displayed joined by being
displayed.  It was not created in the wip, so `wip-kill' will not
kill it (see `wip-test-kill-wip-spares-adopted-buffers')."
  (wip-test--fixture
    (let ((global (generate-new-buffer "wip-test-global")))
      (switch-to-buffer global)
      (wip "alpha")
      (let ((pad (wip--get-pad wip--current)))
        (kill-buffer pad)
        (generate-new-buffer "wip-test-event")
        (should (memq (window-buffer) (wip--wip-buffers wip--current)))))))

(ert-deftest wip-test-pad-created-for-noncurrent-wip-is-owned-by-it ()
  "A pad created for a non-current wip belongs to that wip alone.
Reachable via a wip ibuffer that outlives its wip being current: the
pad must be registered with and owned by its own wip, not adopted or
owned by whichever wip happens to be current."
  (wip-test--fixture
    (wip "alpha")
    (let ((alpha wip--current))
      (wip "beta")
      (kill-buffer (wip--wip-pad-buffer alpha))
      (let ((pad (wip--get-pad alpha)))
        (generate-new-buffer "wip-test-event")
        (should (memq pad (wip--wip-buffers alpha)))
        (should (memq pad (wip--wip-created alpha)))
        (should-not (memq pad (wip--wip-buffers wip--current)))
        (should-not (memq pad (wip--wip-created wip--current)))))))

(ert-deftest wip-test-kill-fallback-stays-in-wip ()
  "Killing a displayed wip buffer must not pull outside buffers in.
Emacs picks a fallback for the window; `wip--prev-buffer-skip-p'
steers it to a wip member, and the pre-wip buffer in the window's
history must not be adopted."
  (wip-test--fixture
    (let ((global (generate-new-buffer "wip-test-global")))
      (switch-to-buffer global)         ; in window history, pre-wip
      (wip "alpha")
      (let ((member (generate-new-buffer "wip-test-member")))
        (switch-to-buffer member)
        (kill-buffer member)
        (should (memq (window-buffer) (wip--wip-buffers wip--current)))
        ;; A later, unrelated buffer-list event must not adopt the
        ;; pre-wip buffer either.
        (generate-new-buffer "wip-test-event")
        (should-not (memq global (wip--wip-buffers wip--current)))))))

(ert-deftest wip-test-evict-fallback-stays-in-wip ()
  "Evicting a displayed wip buffer must not pull outside buffers in.
Same as `wip-test-kill-fallback-stays-in-wip' but through eviction."
  (wip-test--fixture
    (let ((global (generate-new-buffer "wip-test-global")))
      (switch-to-buffer global)
      (wip "alpha")
      (let ((member (generate-new-buffer "wip-test-member")))
        (switch-to-buffer member)
        (wip--evict-buffer member wip--current)
        (should (memq (window-buffer) (wip--wip-buffers wip--current)))
        (generate-new-buffer "wip-test-event")
        (should-not (memq global (wip--wip-buffers wip--current)))))))

(ert-deftest wip-test-evicted-buffer-not-readopted-when-redisplayed ()
  "Eviction is sticky: redisplaying an evicted buffer does not re-add it.
A restored tab's window configuration (or any other mechanism) can
put an evicted buffer back on screen; that must not silently undo
the eviction."
  (wip-test--fixture
    (wip "alpha")
    (let ((buffer (generate-new-buffer "wip-test-evictee")))
      (wip--evict-buffer buffer wip--current)
      (switch-to-buffer buffer)
      (generate-new-buffer "wip-test-event")
      (should-not (memq buffer (wip--wip-buffers wip--current))))))

(ert-deftest wip-test-explicit-add-unblocks-evicted-buffer ()
  "An explicit add (as C-u C-x b does) brings back an evicted buffer."
  (wip-test--fixture
    (wip "alpha")
    (let ((buffer (generate-new-buffer "wip-test-evictee")))
      (wip--evict-buffer buffer wip--current)
      (wip--add-buffer buffer wip--current)
      (should (memq buffer (wip--wip-buffers wip--current)))
      ;; And selection adoption works for it again afterwards.
      (should-not (memq buffer (wip--wip-evicted wip--current))))))

(ert-deftest wip-test-switch-substitute-not-adopted ()
  "A buffer substituted into a restored wip config is not adopted.
If the saved selected window's buffer died while another wip was
current, `set-window-configuration' substitutes some live buffer
\(possibly another wip's pad); the restore must switch to a member
instead, and the substitute must not join the wip."
  (wip-test--fixture
    (wip "alpha")
    (let ((doomed (generate-new-buffer "wip-test-doomed")))
      (switch-to-buffer doomed)
      (wip "beta")
      (let ((beta-pad (wip--get-pad wip--current)))
        (kill-buffer doomed)
        (wip "alpha")
        (should (memq (window-buffer) (wip--wip-buffers wip--current)))
        (generate-new-buffer "wip-test-event")
        (should-not (memq beta-pad (wip--wip-buffers wip--current)))))))

(ert-deftest wip-test-kill-wip-kills-exclusive-buffers ()
  (wip-test--fixture
    (wip "alpha")
    (let ((buffer (generate-new-buffer "wip-test-exclusive")))
      (wip-kill "alpha")
      (should-not wip--current)
      (should-not (wip-test--wip "alpha"))
      (should-not (buffer-live-p buffer)))))

(ert-deftest wip-test-kill-wip-spares-shared-buffers ()
  (wip-test--fixture
    (wip "alpha")
    (let ((shared (generate-new-buffer "wip-test-shared")))
      (wip "beta")
      (wip--add-buffer shared wip--current)
      (wip-kill "alpha")
      (should (buffer-live-p shared))
      (should (memq shared (wip--wip-buffers (wip-test--wip "beta")))))))

(ert-deftest wip-test-kill-wip-snapshots-promoted-pad ()
  (wip-test--fixture
    (wip "alpha")
    (let ((pad (wip--get-pad wip--current))
          (wip wip--current))
      (with-current-buffer pad
        (insert "promoted then killed")
        (rename-buffer "promoted-notes"))
      (wip-kill "alpha")
      ;; The promoted buffer was exclusive to the wip, so it was
      ;; killed -- but since no persist had noticed the promotion
      ;; yet, its content was snapshotted to the pad file first
      ;; instead of being forgotten.
      (should-not (buffer-live-p pad))
      (should (file-exists-p (wip--pad-file wip)))
      (should (equal (with-temp-buffer
                       (insert-file-contents (wip--pad-file wip))
                       (buffer-string))
                     "promoted then killed")))))

(ert-deftest wip-test-kill-respects-custom-flag ()
  (wip-test--fixture
    (let ((wip-kill-exclusive-buffers nil))
      (wip "alpha")
      (let ((buffer (generate-new-buffer "wip-test-survivor")))
        (wip-kill "alpha")
        (should (buffer-live-p buffer))))))

(ert-deftest wip-test-exit-keeps-wip-active ()
  (wip-test--fixture
    (wip "alpha")
    (wip-exit)
    (should-not wip--current)
    (should (wip-test--wip "alpha"))
    ;; Buffers created outside a wip belong to nothing.
    (let ((buffer (generate-new-buffer "wip-test-outside")))
      (wip "alpha")
      (should-not (memq buffer (wip--wip-buffers wip--current))))))

(ert-deftest wip-test-exit-outside-wip-errors ()
  (wip-test--fixture
    (should-error (wip-exit) :type 'user-error)))

(ert-deftest wip-test-switch-saves-window-config ()
  (wip-test--fixture
    (wip "alpha")
    (wip "beta")
    (should (window-configuration-p
             (nth 1 (wip--frame-state (wip-test--wip "alpha")))))))

(ert-deftest wip-test-frame-states-do-not-overwrite-one-another ()
  "Each frame retains a distinct saved view for the same wip."
  (wip-test--fixture
    (let ((wip (wip--wip-create :name "alpha"))
          (first-frame 'first-frame)
          (second-frame 'second-frame)
          (first-config 'first-config)
          (second-config 'second-config)
          (first-tabs 'first-tabs)
          (second-tabs 'second-tabs))
      (wip--set-frame-state wip first-frame first-config first-tabs)
      (wip--set-frame-state wip second-frame second-config second-tabs)
      (should (equal (wip--frame-state wip first-frame)
                     (list first-frame first-config first-tabs)))
      (should (equal (wip--frame-state wip second-frame)
                     (list second-frame second-config second-tabs))))))

(ert-deftest wip-test-reload-upgrades-frame-session ()
  "Reload compatibility also repairs current wips saved in frame state."
  (wip-test--fixture
    (let* ((frame (selected-frame))
           (saved (frame-parameter frame 'wip--current))
           (stale (record 'wip--wip "legacy" nil nil nil nil nil)))
      (unwind-protect
          (progn
            (push (cons "legacy" stale) wip--wips)
            (set-frame-parameter frame 'wip--current stale)
            (wip--upgrade-wips)
            (should (wip--wip-compatible-p
                     (frame-parameter frame 'wip--current))))
        (set-frame-parameter frame 'wip--current saved)))))

(ert-deftest wip-test-active-frame-session-keeps-fallback-filter ()
  "Loading an active frame keeps its wip fallback-buffer predicate."
  (wip-test--fixture
    (let* ((frame (selected-frame))
           (saved-current (frame-parameter frame 'wip--current))
           (saved-skip (frame-parameter frame 'wip--saved-prev-buffer-skip))
           (wip (wip--wip-create :name "alpha")))
      (unwind-protect
          (progn
            (set-frame-parameter frame 'wip--current wip)
            (set-frame-parameter frame 'wip--saved-prev-buffer-skip nil)
            (wip--load-frame-session frame)
            (should (eq switch-to-prev-buffer-skip
                        #'wip--prev-buffer-skip-p)))
        (set-frame-parameter frame 'wip--current saved-current)
        (set-frame-parameter frame 'wip--saved-prev-buffer-skip saved-skip)))))

(ert-deftest wip-test-pad-created-with-modes ()
  (wip-test--fixture
    (wip "alpha")
    (let ((pad (wip--get-pad wip--current)))
      (should (buffer-live-p pad))
      (should (string-prefix-p wip-pad-buffer-name (buffer-name pad)))
      (should (eq (buffer-local-value 'major-mode pad) 'org-mode))
      (should (memq pad (wip--wip-buffers wip--current)))
      ;; Idempotent while the pad is live.
      (should (eq pad (wip--get-pad wip--current))))))

(ert-deftest wip-test-terminal-falls-back-to-eshell ()
  "A wip terminal uses its wip-specific name when vterm is unavailable."
  (wip-test--fixture
    (wip "alpha")
    (let ((original-require (symbol-function 'require)))
      (cl-letf (((symbol-function 'require)
                 (lambda (feature &optional filename noerror)
                   (if (eq feature 'vterm)
                       nil
                     (funcall original-require feature filename noerror)))))
        (wip-terminal)))
    (let ((buffer (get-buffer "*wip terminal: alpha*")))
      (should buffer)
      (with-current-buffer buffer
        (should (derived-mode-p 'eshell-mode)))
      (wip-terminal)
      (should (eq buffer (get-buffer "*wip terminal: alpha*"))))))

(ert-deftest wip-test-terminal-ignores-unrelated-name-collision ()
  "A non-terminal buffer with the terminal name is not reused."
  (wip-test--fixture
    (wip "alpha")
    (let ((collision (generate-new-buffer "*wip terminal: alpha*")))
      (let ((original-require (symbol-function 'require)))
        (cl-letf (((symbol-function 'require)
                   (lambda (feature &optional filename noerror)
                     (if (eq feature 'vterm)
                         nil
                       (funcall original-require feature filename noerror)))))
          (wip-terminal)))
      (should-not (eq collision (wip--terminal-buffer wip--current))))))

(ert-deftest wip-test-terminal-ignores-dead-vterm ()
  "A terminated vterm is recreated rather than redisplayed."
  (wip-test--fixture
    (wip "alpha")
    (let ((buffer (generate-new-buffer "wip-test-dead-vterm"))
          (wip wip--current))
      (with-current-buffer buffer
        (setq-local wip--terminal-wip wip)
        (setq major-mode 'vterm-mode))
      (should-not (wip--terminal-buffer wip)))))

(ert-deftest wip-test-terminal-survives-wip-upgrade ()
  "A terminal remains associated with its wip after its record is replaced."
  (wip-test--fixture
    (wip "alpha")
    (let* ((buffer (generate-new-buffer "wip-test-terminal-reload"))
           ;; The original 7-slot layout, as would remain after reload.
           (stale (record 'wip--wip "alpha" nil nil nil nil nil)))
      (with-current-buffer buffer
        (setq-local wip--terminal-wip stale))
      (setcdr (assoc "alpha" wip--wips) stale)
      (setq wip--current stale)
      (wip--upgrade-wips)
      (should (eq buffer (wip--terminal-buffer wip--current))))))

(ert-deftest wip-test-terminal-does-not-survive-kill-and-recreate ()
  "A terminal belongs to one wip lifetime, not just its name."
  (wip-test--fixture
    (let ((wip-kill-exclusive-buffers nil))
      (wip "alpha")
      (let ((buffer (generate-new-buffer "wip-test-old-terminal")))
        (with-current-buffer buffer
          (setq-local wip--terminal-wip wip--current))
        (wip-kill "alpha")
        (wip "alpha")
        (should-not (eq buffer (wip--terminal-buffer wip--current)))))))

(ert-deftest wip-test-terminal-invalid-directory-falls-back-to-local-home ()
  "A terminal never inherits an invalid or remote working directory."
  (wip-test--fixture
    (wip "alpha")
    (let ((default-directory "/nonexistent-wip-test-directory/")
          (original-require (symbol-function 'require)))
      (cl-letf (((symbol-function 'require)
                 (lambda (feature &optional filename noerror)
                   (if (eq feature 'vterm)
                       nil
                     (funcall original-require feature filename noerror)))))
        (wip-terminal)))
    (with-current-buffer (wip--terminal-buffer wip--current)
      (should (equal default-directory
                     (file-name-as-directory
                      (expand-file-name (or (getenv "HOME") "~"))))))))

(ert-deftest wip-test-terminal-key-survives-reload ()
  "Reloading wip.el installs the terminal binding in an existing keymap."
  (let* ((key (kbd "C-c w t"))
         (saved (lookup-key wip-mode-map key))
         (source (expand-file-name "wip.el"
                                   (file-name-directory (locate-library "wip")))))
    (unwind-protect
        (progn
          (define-key wip-mode-map key nil)
          (load source nil nil t)
          (should (eq (lookup-key wip-mode-map key) #'wip-terminal)))
      (define-key wip-mode-map key saved))))

(ert-deftest wip-test-tile-buffers-matches-substring-in-a-balanced-grid ()
  "Tiling shows each matching buffer in an equally sized grid cell."
  (wip-test--fixture
    (let ((first (generate-new-buffer "wip-test-tile-one"))
          (second (generate-new-buffer "wip-test-tile-two"))
          (third (generate-new-buffer "wip-test-tile-three"))
          (unmatched (generate-new-buffer "wip-test-unmatched")))
      (wip-tile-buffers "tile")
      (let ((windows (window-list nil 'never)))
        (should (= (length windows) 4))
        (dolist (buffer (list first second third))
          (should (memq buffer (mapcar #'window-buffer windows))))
        (should-not (memq unmatched (mapcar #'window-buffer windows)))
        (should (apply #'= (mapcar #'window-total-width windows)))
        (should (<= (- (apply #'max (mapcar #'window-total-height windows))
                       (apply #'min (mapcar #'window-total-height windows)))
                    1))))))

(ert-deftest wip-test-tile-buffers-no-match-keeps-layout ()
  "A failed tile request leaves the selected frame unchanged."
  (wip-test--fixture
    (let ((buffer (window-buffer)))
      (should-error (wip-tile-buffers "wip-test-no-match") :type 'user-error)
      (should (= (length (window-list nil 'never)) 1))
      (should (eq (window-buffer) buffer)))))

(ert-deftest wip-test-tile-buffers-quit-keeps-layout ()
  "Quitting a tile request restores the previous layout."
  (wip-test--fixture
    (let ((right (generate-new-buffer "wip-test-tile-right")))
      (set-window-buffer (split-window-right) right)
      (let ((before (mapcar #'window-buffer (window-list nil 'never))))
        (dolist (name '("wip-test-tile-one" "wip-test-tile-two"
                        "wip-test-tile-three"))
          (generate-new-buffer name))
        (cl-letf (((symbol-function 'split-window-below)
                   (lambda (&rest _) (signal 'quit nil))))
          (should (eq (condition-case nil
                          (progn (wip-tile-buffers "wip-test-tile") nil)
                        (quit 'quit))
                      'quit)))
        (should (equal (mapcar #'window-buffer (window-list nil 'never))
                       before))))))

(ert-deftest wip-test-pad-persist-and-recover ()
  (wip-test--fixture
    (wip "alpha")
    (let ((pad (wip--get-pad wip--current)))
      (with-current-buffer pad
        (insert "remember this"))
      (wip--persist-pad wip--current)
      (should (file-exists-p (wip--pad-file wip--current)))
      (kill-buffer pad)
      (let ((recovered (wip--get-pad wip--current)))
        (should-not (eq recovered pad))
        (should (equal (with-current-buffer recovered (buffer-string))
                       "remember this"))))))

(ert-deftest wip-test-pad-renamed-creates-fresh ()
  (wip-test--fixture
    (wip "alpha")
    (let ((pad (wip--get-pad wip--current)))
      (with-current-buffer pad
        (insert "promoted content"))
      ;; Persist first so an old pad file really exists on disk.
      (wip--persist-pad wip--current)
      (should (file-exists-p (wip--pad-file wip--current)))
      (with-current-buffer pad
        (rename-buffer "promoted-notes"))
      (let ((fresh (wip--get-pad wip--current)))
        (should-not (eq fresh pad))
        ;; The old pad was renamed away, not lost.
        (should (buffer-live-p pad))
        ;; The fresh pad starts empty even though a pad file existed:
        ;; noticing the promotion deletes the stale file.
        (should (equal (with-current-buffer fresh (buffer-string)) ""))
        (should-not (file-exists-p (wip--pad-file wip--current)))))))

(ert-deftest wip-test-pad-persist-forgets-renamed-pad ()
  (wip-test--fixture
    (wip "alpha")
    (let ((pad (wip--get-pad wip--current)))
      (with-current-buffer pad
        (insert "should not be written")
        (rename-buffer "promoted-notes"))
      ;; Persisting after a promotion forgets the pad instead of
      ;; writing the promoted content.
      (wip--persist-pad wip--current)
      (should-not (file-exists-p (wip--pad-file wip--current)))
      (should-not (wip--wip-pad-buffer wip--current)))))

(ert-deftest wip-test-pad-promoted-then-killed-not-resurrected ()
  (wip-test--fixture
    (wip "alpha")
    (let ((pad (wip--get-pad wip--current)))
      (with-current-buffer pad
        (insert "draft v1"))
      (wip--persist-pad wip--current)
      (with-current-buffer pad
        (rename-buffer "promoted-notes"))
      ;; Kill the promoted buffer with no persist in between; the
      ;; kill hook notices the promotion and forgets the pad file.
      (kill-buffer pad)
      (let ((fresh (wip--get-pad wip--current)))
        (should (equal (with-current-buffer fresh (buffer-string)) ""))))))

(ert-deftest wip-test-pad-killed-as-pad-recovers-latest ()
  (wip-test--fixture
    (wip "alpha")
    (let ((pad (wip--get-pad wip--current)))
      (with-current-buffer pad
        (insert "latest edits"))
      ;; No explicit persist: killing the pad snapshots it to disk.
      (kill-buffer pad)
      (let ((recovered (wip--get-pad wip--current)))
        (should (equal (with-current-buffer recovered (buffer-string))
                       "latest edits"))))))

(ert-deftest wip-test-sanitize-name ()
  ;; Deterministic, readable prefix, no path separators.
  (should (equal (wip--sanitize-name "simple") (wip--sanitize-name "simple")))
  (should (string-prefix-p "with_space-" (wip--sanitize-name "with space")))
  (should-not (string-match-p "/" (wip--sanitize-name "../../etc/passwd")))
  ;; Distinct names never share a directory component.
  (should-not (equal (wip--sanitize-name "a/b") (wip--sanitize-name "a_b")))
  (should-not (equal (wip--sanitize-name ".") (wip--sanitize-name "..")))
  (should-not (equal (wip--sanitize-name "") (wip--sanitize-name ".")))
  ;; Never a plain "." or ".." component.
  (should (string-prefix-p "unnamed-" (wip--sanitize-name "")))
  ;; Long names stay well below filesystem component limits, counted
  ;; in bytes even for multibyte names.
  (should (< (string-bytes (wip--sanitize-name (make-string 300 ?x))) 100))
  (should (< (string-bytes (wip--sanitize-name (make-string 300 ?𝔸))) 100)))

(ert-deftest wip-test-tracking-survives-hook-inhibited-buffers ()
  (wip-test--fixture
    (wip "alpha")
    ;; A buffer created with buffer hooks inhibited (as
    ;; `with-temp-buffer' does) never runs the tracking hook itself;
    ;; the hook's full scan on the next tracked creation must still
    ;; pick that creation up.  Regression test for the removed
    ;; buffer-count short-circuit, which this sequence defeated.
    (let ((tmp (generate-new-buffer " *silent*" t)))
      (get-buffer-create "wip-test-a")
      (kill-buffer tmp))
    (let ((buffer (get-buffer-create "wip-test-b")))
      (should (memq buffer (wip--wip-buffers wip--current))))))

(ert-deftest wip-test-persist-error-does-not-wedge-exit ()
  (wip-test--fixture
    (wip "alpha")
    (wip--get-pad wip--current)
    ;; Persisting into an uncreatable directory must not signal, so
    ;; leaving the wip always succeeds.
    (let ((wip-directory "/nonexistent-root-dir/nope"))
      (wip-exit))
    (should-not wip--current)))

(ert-deftest wip-test-state-directory-stays-under-wip-directory ()
  (wip-test--fixture
    (wip "../escape")
    (let ((dir (wip--state-directory wip--current)))
      (should (string-prefix-p (file-name-as-directory
                                (expand-file-name wip-directory))
                               (file-name-as-directory dir))))))

(provide 'wip-test)

;;; wip-test.el ends here
