;;; wip-test.el --- Tests for wip.el -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for wip.el.  Run with `eldev test'.

;;; Code:

(require 'ert)
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
      ;; evicted buffer is already known and must not come back.
      (generate-new-buffer "wip-test-other")
      (should-not (memq buffer (wip--wip-buffers wip--current))))))

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
             (wip--wip-window-config (wip-test--wip "alpha"))))))

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
