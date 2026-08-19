;;; wip.el --- Focused work-in-progress contexts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pierre Larochelle

;; Author: Pierre Larochelle & Claude
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; wip.el groups buffers, window configurations and tab-bar tabs into
;; named "wips" (works in progress) so that you can focus on one piece
;; of work at a time.
;;
;; wip is modal: you are either in a wip or not.  While in a wip:
;;
;; - Membership is one generic rule: any buffer created while the
;;   wip is current joins the wip, and so does any buffer displayed
;;   in one of its windows — created, reused (`find-file', `dired'),
;;   selected, or popped up.  Only internal (space-prefixed) buffers
;;   and evicted buffers are excepted.  Buffers *created* in the wip
;;   are owned by it: `wip-kill' kills owned buffers that belong to
;;   no other wip, and never buffers merely adopted from outside.
;; - `C-x b' (`wip-ido-switch-buffer') is filtered to the wip's
;;   buffers; with a prefix argument it shows all buffers, and a
;;   buffer selected that way is brought into the wip.
;; - `C-x k' (`wip-ido-kill-buffer') evicts a buffer from the wip
;;   without killing it globally; `C-u C-x k' runs the normal
;;   `kill-buffer' command.  Eviction is sticky: the buffer is not
;;   re-adopted just by being displayed again; bring it back with
;;   C-u C-x b.
;; - Each frame keeps its own current wip, window configuration, and
;;   tab-bar tabs.  A wip can therefore be used in several frames
;;   without one frame replacing another's view.
;; - `wip-pad' gives you a per-wip scratchpad, persisted to a file
;;   under `wip-directory' for recovery.
;;
;; Enable `wip-mode' to get the global `C-c w' bindings:
;;
;;   C-c w m   `wip'        select or create a wip
;;   C-c w q   `wip-exit'   leave the current wip (wips survive)
;;   C-c w k   `wip-kill'   kill a wip
;;   C-c w b   `wip-ibuffer'  ibuffer filtered to the wip
;;   C-c w p   `wip-pad'    show the wip scratchpad
;;   C-c w f   `wip-firefox'  show/launch a wip-local Firefox (EXWM)
;;   C-c w t   `wip-terminal' show a wip-local vterm or Eshell
;;   C-c w g   `wip-tile-buffers' tile buffers matching a name substring
;;
;; Current limitations of this first pass:
;;
;; - Winner-mode integration (per-wip `C-c <left>'/`C-c <right>')
;;   is not implemented yet.
;; - The overview and history features from the README are not
;;   implemented yet, nor are project-scoped file selection and
;;   wip-scoped subcommands.
;; - Only the scratchpad is persisted across Emacs sessions; full wip
;;   recovery is not implemented yet.  Scratchpads are written to
;;   disk on wip switches, exits, wip kills, pad kills, and Emacs
;;   exit — not continuously — so a hard crash can lose pad edits
;;   made since entering the wip.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ido)
(require 'ibuffer)
(require 'tab-bar)

(declare-function vterm "vterm" (&optional buffer-name))

;;;; Customization

(defgroup wip nil
  "Focused work-in-progress contexts."
  :group 'convenience
  :prefix "wip-")

(defcustom wip-directory (locate-user-emacs-file "wip")
  "Directory under which per-wip state (such as scratchpads) is stored.
Each wip gets a subdirectory named after a sanitized version of its
name; see `wip--sanitize-name'."
  :type 'directory)

(defcustom wip-scratchpad-modes '(org-mode)
  "Modes enabled in a freshly created wip scratchpad.
Each element is called as a function with no arguments, in order.
Typically the first element is a major mode and any further elements
are minor modes."
  :type '(repeat function))

(defcustom wip-pad-buffer-name "*pad*"
  "Base name for wip scratchpad buffers.
When several wips have scratchpads at once, Emacs uniquifies the
names (\"*pad*<2>\" and so on)."
  :type 'string)

(defcustom wip-kill-exclusive-buffers t
  "Non-nil means `wip-kill' kills buffers the wip created and owns.
Only buffers that were created while the wip was current and belong
to no other wip are killed.  Buffers adopted from outside (merely
displayed or selected while in the wip) and buffers shared with
other wips are always left alone."
  :type 'boolean)

(defcustom wip-firefox-command '("firefox" "--new-window")
  "Command (program and arguments) used by `wip-firefox'."
  :type '(repeat string))

;;;; Internal state

(cl-defstruct (wip--wip (:constructor wip--wip-create)
                        (:copier nil))
  "A single work-in-progress context.
BUFFERS are the members; CREATED tracks the buffers created while
this wip was current, whether or not they are still members (the
wip \"owns\" those — `wip-kill' only ever kills owned buffers);
EVICTED holds buffers explicitly evicted, which stay out until
explicitly re-added."
  name
  (buffers nil)
  (created nil)
  (evicted nil)
  (frame-states nil)
  pad-buffer
  pad-name)

(defvar wip--wips nil
  "Alist of (NAME . WIP) for all active wips, most recently used first.")

(defvar wip--current nil
  "The current `selected-frame' wip, or nil when it is not in a wip.
Also used as the activation variable for `wip--session-map' in
`minor-mode-map-alist'.")

(defvar wip--global-window-config nil
  "Selected-frame configuration saved when entering a wip from global context.")

(defvar wip--global-tabs nil
  "Selected frame's `tabs' parameter saved when entering a wip from global context.")

(defvar wip--known-buffers (make-hash-table :test #'eq :weakness 'key)
  "Selected frame's set of seen buffers, used to detect new buffers.")

(defvar wip--inhibit-adoption nil
  "Non-nil while a wip transition or an eviction is running.
Suppresses display adoption in `wip--on-buffer-list-update' so that
buffers momentarily on display during a transition are not pulled
into the new wip, and so that a buffer being evicted is not
re-adopted while windows still show it.")

(defvar wip--saved-prev-buffer-skip nil
  "Value of `switch-to-prev-buffer-skip' before wip took it over.
Saved by `wip--activate' and restored by `wip--deactivate'.")

(defvar wip--session-frame (selected-frame)
  "Frame whose wip session is currently loaded into the variables above.")

(defun wip--save-frame-session ()
  "Save the loaded wip session in its frame parameters.
`wip--sync-frame-session' calls this before loading the newly selected
frame, keeping wip activation and global-context state independent
between frames."
  (when (frame-live-p wip--session-frame)
    (set-frame-parameter wip--session-frame 'wip--current wip--current)
    (set-frame-parameter wip--session-frame 'wip--global-window-config
                         wip--global-window-config)
    (set-frame-parameter wip--session-frame 'wip--global-tabs wip--global-tabs)
    (set-frame-parameter wip--session-frame 'wip--known-buffers
                         wip--known-buffers)
    (set-frame-parameter wip--session-frame 'wip--saved-prev-buffer-skip
                         wip--saved-prev-buffer-skip)))

(defun wip--load-frame-session (frame)
  "Load FRAME's wip session from its frame parameters.
A frame without saved state starts outside a wip with its own buffer
tracking table and its normal `switch-to-prev-buffer-skip' value."
  (let* ((normal-prev-buffer-skip
          (if wip--current
              wip--saved-prev-buffer-skip
            switch-to-prev-buffer-skip))
         (saved-prev-buffer-skip
          (if (assq 'wip--saved-prev-buffer-skip (frame-parameters frame))
              (frame-parameter frame 'wip--saved-prev-buffer-skip)
            normal-prev-buffer-skip)))
    (setq wip--session-frame frame
          wip--current (frame-parameter frame 'wip--current)
          wip--global-window-config
          (frame-parameter frame 'wip--global-window-config)
          wip--global-tabs (frame-parameter frame 'wip--global-tabs)
          wip--known-buffers
          (or (frame-parameter frame 'wip--known-buffers)
              (make-hash-table :test #'eq :weakness 'key))
          wip--saved-prev-buffer-skip saved-prev-buffer-skip
          switch-to-prev-buffer-skip
          (if wip--current
              #'wip--prev-buffer-skip-p
            saved-prev-buffer-skip))))

(defun wip--sync-frame-session ()
  "Load the selected frame's wip session when it is not already loaded."
  (unless (eq (selected-frame) wip--session-frame)
    (wip--save-frame-session)
    (wip--load-frame-session (selected-frame))))

(defun wip--on-window-selection-change (frame)
  "Swap sessions when FRAME becomes the `selected-frame' active window.
This is the documented Emacs 28+ notification for frame selection as
well as window selection; the current-frame check ignores unrelated
redisplay notifications."
  (when (eq frame (selected-frame))
    (wip--sync-frame-session)))

;;;; Reload compatibility

(defconst wip--wip-length (length (wip--wip-create :name ""))
  "Record length of the current `wip--wip' layout.
Used to detect stale records after wip.el is reloaded with a
changed struct definition.")

(defun wip--wip-compatible-p (object)
  "Return non-nil if OBJECT is a wip record with the current layout."
  (and (recordp object)
       (eq (aref object 0) 'wip--wip)
       (= (length object) wip--wip-length)))

(defun wip--upgrade-wip (object)
  "Return OBJECT, or a compatible replacement rebuilt from it.
A record from an older wip.el layout keeps its name (slot 1) and
live buffers (slot 2) — stable across all layouts so far; the rest
of its state (per-frame view state, pad bookkeeping, eviction and
ownership lists) is dropped and gets rebuilt lazily."
  (if (wip--wip-compatible-p object)
      object
    (let ((new (wip--wip-create :name (aref object 1))))
      (setf (wip--wip-buffers new)
            (seq-filter #'buffer-live-p
                        (and (> (length object) 2) (aref object 2))))
      new)))

(defun wip--upgrade-wips ()
  "Replace stale-layout wip records after a reload of wip.el.
Walks `wip--wips', upgrading each record via `wip--upgrade-wip' and
keeping the current-wip state in every frame pointing at the
upgraded object."
  (let (upgrades)
    (setq wip--wips
          (mapcar (lambda (entry)
                    (let* ((old (cdr entry))
                           (new (wip--upgrade-wip old)))
                      (push (cons old new) upgrades)
                      (when (eq old wip--current)
                        (setq wip--current new))
                      (cons (car entry) new)))
                  wip--wips))
    (dolist (frame (frame-list))
      (let ((current (frame-parameter frame 'wip--current)))
        (when (and current (not (wip--wip-compatible-p current)))
          (let ((new (or (cdr (assq current upgrades))
                         (cdr (assoc (wip--wip-name current) wip--wips))
                         (wip--upgrade-wip current))))
            (set-frame-parameter frame 'wip--current new)
            (when (eq frame wip--session-frame)
              (setq wip--current new))))))
    ;; Terminal buffers refer to their owning record.  Preserve that
    ;; ownership when a reload replaces a stale record.
    (dolist (buffer (buffer-list))
      (let ((replacement (assq (buffer-local-value 'wip--terminal-wip buffer)
                               upgrades)))
        (when replacement
          (with-current-buffer buffer
            (setq-local wip--terminal-wip (cdr replacement))))))))

;;;; Buffer bookkeeping

(defun wip--trackable-buffer-p (buffer)
  "Return non-nil if BUFFER should be tracked in a wip.
Internal buffers (whose names start with a space) are not tracked."
  (not (string-prefix-p " " (buffer-name buffer))))

(defun wip--live-buffers (wip)
  "Return the live buffers of WIP, pruning dead ones from its lists."
  (setf (wip--wip-created wip)
        (seq-filter #'buffer-live-p (wip--wip-created wip)))
  (setf (wip--wip-buffers wip)
        (seq-filter #'buffer-live-p (wip--wip-buffers wip))))

(defun wip--add-buffer (buffer wip)
  "Add BUFFER to WIP unless it is already a member or dead.
Adding also clears BUFFER's evicted status, so an explicit add
brings back a previously evicted buffer."
  (when (buffer-live-p buffer)
    (setf (wip--wip-evicted wip) (delq buffer (wip--wip-evicted wip)))
    (unless (memq buffer (wip--wip-buffers wip))
      (push buffer (wip--wip-buffers wip)))))

(defun wip--remove-buffer (buffer wip)
  "Remove BUFFER from WIP.  The buffer is not killed."
  (setf (wip--wip-buffers wip)
        (remq buffer (wip--wip-buffers wip))))

(defun wip--evict-buffer (buffer wip)
  "Evict BUFFER from WIP and remove it from any window showing it.
The buffer is never killed; windows showing it switch to another wip
buffer (see `wip--prev-buffer-skip-p'), as `kill-buffer' would
arrange, falling back to the wip's pad when the wip has no other
buffer to show (evicting the pad itself just removes it from the
member list).  The buffer is remembered as evicted: merely being
displayed again (say, by a restored tab) will not re-adopt it;
\\[universal-argument] \\[wip-ido-switch-buffer] brings it back.
Adoption is suppressed while the windows change."
  (wip--remove-buffer buffer wip)
  (setf (wip--wip-evicted wip)
        (cons buffer (delq buffer (seq-filter #'buffer-live-p
                                              (wip--wip-evicted wip)))))
  (let ((wip--inhibit-adoption t))
    (replace-buffer-in-windows buffer)
    ;; If no other wip buffer existed to fall back on, windows may
    ;; still show the evictee; give them the pad instead (unless the
    ;; evictee IS the pad).
    (when (get-buffer-window-list buffer nil t)
      (let ((fallback (wip--get-pad wip)))
        (unless (eq fallback buffer)
          (dolist (window (get-buffer-window-list buffer nil t))
            (set-window-buffer window fallback)))))))

(defun wip--mru-buffers (wip)
  "Return WIP's live buffers in most-recently-used order.
Buffers that were never selected come last, in creation order."
  (let ((buffers (wip--live-buffers wip)))
    (seq-filter (lambda (buffer) (memq buffer buffers))
                (buffer-list))))

(defun wip--buffer-owners (buffer)
  "Return the wips that BUFFER belongs to, as a list of structs."
  (seq-filter (lambda (wip) (memq buffer (wip--wip-buffers wip)))
              (mapcar #'cdr wip--wips)))

(defun wip--sync-known-buffers ()
  "Record every existing buffer as known, without adding any to a wip."
  (dolist (buffer (buffer-list))
    (puthash buffer t wip--known-buffers)))

(defun wip--on-buffer-list-update ()
  "Track buffers for the current wip.
Installed on `buffer-list-update-hook' while `wip-mode' is enabled;
does nothing in frames without a current wip.
Membership is deliberately generic — one rule, no per-command cases:
any trackable buffer (name not starting with a space) that is
CREATED while the wip is current joins the wip and is owned by it,
and any trackable buffer DISPLAYED in one of the frame's windows
joins the wip — however it got there (created, reused by
`find-file' or `dired', popped up, or chosen by Emacs as a
fallback).  The only exceptions: the wip's evicted buffers (see
`wip--evict-buffer') stay out, and display adoption pauses during
wip transitions and evictions (see `wip--inhibit-adoption').
Buffers created with buffer hooks inhibited are picked up on the
next run."
  (wip--sync-frame-session)
  (when wip--current
    (unless (wip--wip-compatible-p wip--current)
      (wip--upgrade-wips))
    (dolist (buffer (buffer-list))
      (unless (gethash buffer wip--known-buffers)
        (puthash buffer t wip--known-buffers)
        (when (wip--trackable-buffer-p buffer)
          (wip--add-buffer buffer wip--current)
          (push buffer (wip--wip-created wip--current)))))
    (unless wip--inhibit-adoption
      (dolist (window (window-list))
        (let ((buffer (window-buffer window)))
          (when (and (wip--trackable-buffer-p buffer)
                     (not (memq buffer
                                (wip--wip-evicted wip--current))))
            (wip--add-buffer buffer wip--current)))))))

(defun wip--prev-buffer-skip-p (_window buffer _bury-or-kill)
  "Return non-nil to skip BUFFER as a window fallback while in a wip.
Installed as `switch-to-prev-buffer-skip' while the wip machinery is
active, so that when a displayed buffer is killed or evicted the
window falls back to a buffer of the current wip instead of pulling
an outside buffer into view (which display adoption would then
adopt)."
  (and wip--current
       (not (memq buffer (wip--wip-buffers wip--current)))))

;;;; Entering, leaving and killing wips

(defvar wip-mode)

(defun wip--ensure-current ()
  "Signal a `user-error' unless a wip is current.
Also upgrades stale wip records left behind by a reload of wip.el."
  (wip--sync-frame-session)
  (unless wip--current
    (user-error "Not in a wip (use `wip' / C-c w m to enter one)"))
  (unless (wip--wip-compatible-p wip--current)
    (wip--upgrade-wips)))

(defun wip--read-wip-name (prompt &optional allow-new)
  "Read the name of a wip with PROMPT using ido completion.
When ALLOW-NEW is non-nil, any input is accepted; otherwise the input
must name an active wip."
  (let ((names (mapcar #'car wip--wips)))
    (when (and (null names) (not allow-new))
      (user-error "No active wips"))
    (ido-completing-read prompt names nil (not allow-new))))

(defun wip--activate ()
  "Activate wip behavior in the selected frame.
Takes over this frame's `switch-to-prev-buffer-skip'; its prior value
is restored on exit.  The tracking hook is global, so it is only
removed when `wip-mode' is disabled."
  (add-hook 'buffer-list-update-hook #'wip--on-buffer-list-update)
  (setq wip--saved-prev-buffer-skip switch-to-prev-buffer-skip
        switch-to-prev-buffer-skip #'wip--prev-buffer-skip-p))

(defun wip--deactivate ()
  "Deactivate wip behavior in the selected frame.
Restores that frame's `switch-to-prev-buffer-skip' value."
  (setq switch-to-prev-buffer-skip wip--saved-prev-buffer-skip))

(defun wip--restorable-config-p (config)
  "Return non-nil if CONFIG is a window configuration that can be restored.
A configuration whose frame has been deleted passes
`window-configuration-p' but restores nothing, so it is rejected."
  (and (window-configuration-p config)
       (frame-live-p (window-configuration-frame config))))

(defun wip--set-frame-state (wip frame window-config tabs)
  "Save FRAME's WINDOW-CONFIG and TABS into WIP.
Each entry is keyed by its frame so working in another frame cannot
replace this frame's saved view."
  (setf (wip--wip-frame-states wip)
        (cons (list frame window-config tabs)
              (assq-delete-all frame (wip--wip-frame-states wip)))))

(defun wip--frame-state (wip &optional frame)
  "Return WIP's saved view state for FRAME, or nil.
FRAME defaults to the `selected-frame'.  The returned list holds the
frame, window configuration, and tabs in that order."
  (assq (or frame (selected-frame)) (wip--wip-frame-states wip)))

(defun wip--save-state (wip)
  "Save the selected frame's window configuration and tabs into WIP.
Also persists WIP's scratchpad to disk; persistence errors are
demoted to messages so state transitions always complete."
  (wip--set-frame-state wip (selected-frame)
                        (current-window-configuration)
                        (frame-parameter nil 'tabs))
  (wip--persist-pad wip))

(defun wip--restore-state (wip)
  "Restore WIP's `selected-frame' window configuration and tabs.
A wip that was never displayed before, or whose saved configuration
belongs to a deleted frame, gets a fresh single window showing its
scratchpad."
  (let* ((state (wip--frame-state wip))
         (window-config (nth 1 state))
         (tabs (nth 2 state)))
    (set-frame-parameter nil 'tabs tabs)
    (if (wip--restorable-config-p window-config)
      (progn
        (set-window-configuration window-config)
        ;; If the saved selected window's buffer died meanwhile,
        ;; Emacs substitutes a buffer of its own choosing; switch to
        ;; a wip buffer instead so the substitute is not adopted.
        ;; When no member is left to show, fall back to the pad.
        (unless (memq (window-buffer) (wip--live-buffers wip))
          (switch-to-prev-buffer)
          (unless (memq (window-buffer) (wip--live-buffers wip))
            (switch-to-buffer (wip--get-pad wip)))))
      (delete-other-windows)
      (switch-to-buffer (wip--get-pad wip))))
  (force-mode-line-update t))

(defun wip--save-global-state ()
  "Save the non-wip window configuration and tabs."
  (setq wip--global-window-config (current-window-configuration)
        wip--global-tabs (frame-parameter nil 'tabs)))

(defun wip--restore-global-state ()
  "Restore the window configuration and tabs saved by `wip--save-global-state'."
  (set-frame-parameter nil 'tabs wip--global-tabs)
  (when (wip--restorable-config-p wip--global-window-config)
    (set-window-configuration wip--global-window-config))
  (force-mode-line-update t))

(defun wip--switch-to (wip)
  "Make WIP current, saving the state of wherever we came from."
  (if (eq wip wip--current)
      (message "Already in wip %s" (wip--wip-name wip))
    (let ((wip--inhibit-adoption t))
      (if wip--current
          (wip--save-state wip--current)
        (wip--save-global-state)
        (wip--activate))
      (setq wip--current wip)
      ;; Most recently used first.
      (setq wip--wips (cons (cons (wip--wip-name wip) wip)
                            (rassq-delete-all wip wip--wips)))
      (wip--sync-known-buffers)
      (wip--restore-state wip))
    (message "wip: %s" (wip--wip-name wip))))

;;;###autoload
(defun wip (name)
  "Switch to the wip called NAME, creating it if it does not exist.
Interactively, complete over the active wips; typing a new name
creates a fresh wip."
  (interactive (list (wip--read-wip-name "wip: " t)))
  (wip--sync-frame-session)
  (when (string-blank-p name)
    (user-error "A wip needs a name"))
  (wip--upgrade-wips)
  (unless wip-mode (wip-mode 1))
  (wip--switch-to
   (or (cdr (assoc name wip--wips))
       (let ((wip (wip--wip-create :name name)))
         (push (cons name wip) wip--wips)
         wip))))

(defun wip-exit ()
  "Leave the current wip and restore the global context.
The wip itself stays active and can be re-entered with `wip'."
  (interactive)
  (wip--ensure-current)
  (let ((name (wip--wip-name wip--current)))
    (wip--save-state wip--current)
    (setq wip--current nil)
    (wip--deactivate)
    (wip--restore-global-state)
    (wip--save-frame-session)
    (message "Left wip %s" name)))

(defun wip--leave-wip-everywhere (wip)
  "Leave WIP and restore global context in every frame using it.
This is used before killing WIP, so another frame cannot retain a
current wip record that has been removed from `wip--wips'."
  (dolist (frame (frame-list))
    (with-selected-frame frame
      (wip--sync-frame-session)
      (when (eq wip wip--current)
        (wip--save-state wip)
        (setq wip--current nil)
        (wip--deactivate)
        (wip--restore-global-state)
        (wip--save-frame-session)))))

(defun wip-kill (name)
  "Kill the wip called NAME.
The wip's scratchpad content is snapshotted to disk first, and its
state directory under `wip-directory' is kept, so pad content
remains recoverable after the kill.  When
`wip-kill-exclusive-buffers' is non-nil, buffers that were created
in this wip, still belong to it, and belong to no other wip are
killed globally; buffers adopted from outside are left alive.  Like
`kill-buffer' generally, that sweep discards unsaved non-file
buffers without asking —
including a pad that was renamed away earlier and never saved to a
file; save promoted content if it must survive a wip kill.  If NAME
is current in any frame, the global context is restored in each of
those frames."
  (interactive (list (wip--read-wip-name "Kill wip: ")))
  (wip--upgrade-wips)
  (let* ((entry (assoc name wip--wips))
         (wip (cdr entry)))
    (unless wip
      (user-error "No wip named %s" name))
    (wip--persist-pad-final wip)
    (wip--leave-wip-everywhere wip)
    (setq wip--wips (delq entry wip--wips))
    (when wip-kill-exclusive-buffers
      (dolist (buffer (wip--live-buffers wip))
        (when (and (memq buffer (wip--wip-created wip))
                   (null (wip--buffer-owners buffer)))
          (kill-buffer buffer))))
    (message "Killed wip %s" name)))

;;;; Buffer commands

(defun wip-ido-switch-buffer (&optional arg)
  "Switch to a buffer of the current wip, with ido completion.
With prefix argument ARG, complete over all buffers instead; the
selected buffer is brought into the wip."
  (interactive "P")
  (wip--ensure-current)
  (if arg
      (progn
        (call-interactively #'ido-switch-buffer)
        (wip--add-buffer (current-buffer) wip--current))
    (let ((names (mapcar #'buffer-name
                         (remq (current-buffer)
                               (wip--mru-buffers wip--current)))))
      (if (null names)
          (message "No other buffers in wip %s (C-u C-x b to pull one in)"
                   (wip--wip-name wip--current))
        (switch-to-buffer
         (ido-completing-read "wip buffer: " names nil t))))))

(defun wip-ido-kill-buffer (&optional arg)
  "Evict a buffer from the current wip, with ido completion.
The buffer is only removed from the wip, not killed globally, but
windows showing it switch to another buffer, as they would if it had
been killed.  With prefix argument ARG, run the normal `kill-buffer'
command instead."
  (interactive "P")
  (wip--ensure-current)
  (if arg
      (call-interactively #'kill-buffer)
    (let ((names (mapcar #'buffer-name (wip--mru-buffers wip--current))))
      (if (null names)
          (message "No buffers in wip %s" (wip--wip-name wip--current))
        (let* ((default (car (member (buffer-name) names)))
               (name (ido-completing-read "Evict from wip: " names nil t
                                          nil nil default))
               (buffer (get-buffer name)))
          (if (null buffer)
              (message "No such buffer: %s" name)
            (wip--evict-buffer buffer wip--current)
            (message "Evicted %s from wip %s"
                     name (wip--wip-name wip--current))))))))

;;;; ibuffer integration

(defvar-local wip-ibuffer--wip nil
  "The wip that this ibuffer buffer was created for.")

(defun wip-ibuffer--target-buffers ()
  "Return the buffers marked in any way, or the buffer at point."
  (let (buffers)
    (ibuffer-map-lines-nomodify
     (lambda (buffer mark)
       (unless (eq mark ?\s)
         (push buffer buffers))))
    (or (nreverse buffers)
        (list (ibuffer-current-buffer t)))))

(defun wip-ibuffer-do-evict ()
  "Evict the marked buffers (or the buffer at point) from the wip.
This replaces the kill commands in wip ibuffer buffers: buffers are
removed from the wip (and from windows showing them) but never
killed globally.  Both regular marks and deletion marks count."
  (interactive)
  (let ((wip (or wip-ibuffer--wip wip--current))
        (buffers (wip-ibuffer--target-buffers)))
    (unless wip
      (user-error "This ibuffer is not attached to a wip"))
    (dolist (buffer buffers)
      (wip--evict-buffer buffer wip))
    (ibuffer-update nil t)
    (message "Evicted %d buffer(s) from wip %s"
             (length buffers) (wip--wip-name wip))))

(defvar wip-ibuffer-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap ibuffer-do-delete] #'wip-ibuffer-do-evict)
    (define-key map [remap ibuffer-do-kill-on-deletion-marks]
                #'wip-ibuffer-do-evict)
    map)
  "Keymap for `wip-ibuffer-minor-mode'.")

(define-minor-mode wip-ibuffer-minor-mode
  "Minor mode for ibuffer buffers scoped to a wip.
Remaps `ibuffer-do-delete' and `ibuffer-do-kill-on-deletion-marks' to
`wip-ibuffer-do-evict' so that \"deleting\" a buffer only evicts it
from the wip."
  :lighter " wip-ibuf")

(defun wip-ibuffer ()
  "Show an ibuffer listing only the buffers of the current wip.
Deleting a buffer from this listing evicts it from the wip instead of
killing it."
  (interactive)
  (wip--ensure-current)
  (let ((wip wip--current))
    (ibuffer nil (format "*wip ibuffer: %s*" (wip--wip-name wip))
             `((predicate . (memq (current-buffer)
                                  (wip--live-buffers ,wip)))))
    (setq wip-ibuffer--wip wip)
    (wip-ibuffer-minor-mode 1)))

;;;; The scratchpad

(defun wip--sanitize-name (name)
  "Return NAME transformed into a safe, unique directory component.
Characters other than alphanumerics, `_', `.' and `-' become `_', the
readable part is capped at 80 bytes (so multibyte names stay under
filesystem component limits), and a short hash of the raw NAME is
appended so that distinct names never share a directory (and so the
result is never \".\" or \"..\")."
  (let ((safe (replace-regexp-in-string "[^[:alnum:]_.-]" "_" name)))
    (while (> (string-bytes safe) 80)
      (setq safe (substring safe 0 -1)))
    (when (string-empty-p safe)
      (setq safe "unnamed"))
    (format "%s-%s" safe (substring (sha1 name) 0 8))))

(defun wip--state-directory (wip)
  "Return the directory holding WIP's persisted state."
  (expand-file-name (wip--sanitize-name (wip--wip-name wip))
                    wip-directory))

(defun wip--pad-file (wip)
  "Return the file WIP's scratchpad is persisted to."
  (expand-file-name "pad.org" (wip--state-directory wip)))

(defun wip--pad-live-p (wip)
  "Return WIP's scratchpad buffer if it is still usable, else nil.
A scratchpad stops being the scratchpad once it is killed, renamed,
or starts visiting a file."
  (let ((pad (wip--wip-pad-buffer wip)))
    (and (buffer-live-p pad)
         (null (buffer-file-name pad))
         (equal (buffer-name pad) (wip--wip-pad-name wip))
         pad)))

(defun wip--pad-promoted-p (wip)
  "Return non-nil if WIP's scratchpad buffer is live but no longer the pad.
This happens when the user renames the pad or saves it to a file,
promoting its content to a proper home."
  (let ((pad (wip--wip-pad-buffer wip)))
    (and (buffer-live-p pad)
         (not (wip--pad-live-p wip)))))

(defun wip--forget-pad (wip)
  "Clear WIP's scratchpad bookkeeping and delete its persisted file.
Called when the pad was promoted (renamed or saved to a file): its
content lives on in the promoted buffer, so the next scratchpad must
genuinely start empty."
  (setf (wip--wip-pad-buffer wip) nil
        (wip--wip-pad-name wip) nil)
  (with-demoted-errors "wip: could not delete stale pad file: %S"
    (let ((file (wip--pad-file wip)))
      (when (file-exists-p file)
        (delete-file file)))))

(defun wip--on-pad-killed (wip)
  "React to WIP's scratchpad buffer being killed.
If it dies while still being the pad, snapshot its content to disk so
it can be recovered.  If it was promoted (renamed or saved to a file)
before being killed, forget it instead so the next scratchpad starts
empty.  Runs from a buffer-local `kill-buffer-hook'."
  (when (eq (current-buffer) (wip--wip-pad-buffer wip))
    (if (wip--pad-live-p wip)
        (wip--persist-pad wip)
      (wip--forget-pad wip))))

(defun wip--create-pad (wip)
  "Create a fresh scratchpad buffer for WIP and return it.
Contents are recovered from `wip--pad-file' when it exists; the file
is deleted whenever a pad's promotion (rename or save to a file) is
noticed, so promoted-away content is normally not resurrected.  The
exception is `wip-kill', which snapshots even promoted content, so
re-creating a killed wip can recover it.  The pad is registered with
and owned by WIP itself, even when another wip is current (reachable
via a wip ibuffer that outlived its wip being current)."
  (let ((file (wip--pad-file wip))
        (buffer (let ((wip--current wip)
                      (wip--inhibit-adoption t))
                  (generate-new-buffer wip-pad-buffer-name))))
    (with-current-buffer buffer
      (when (file-exists-p file)
        (insert-file-contents file))
      (dolist (mode wip-scratchpad-modes)
        (with-demoted-errors "wip: error enabling scratchpad mode: %S"
          (funcall mode)))
      (add-hook 'kill-buffer-hook
                (lambda () (wip--on-pad-killed wip))
                nil t))
    (setf (wip--wip-pad-buffer wip) buffer
          (wip--wip-pad-name wip) (buffer-name buffer))
    (puthash buffer t wip--known-buffers)
    (unless (memq buffer (wip--wip-created wip))
      (push buffer (wip--wip-created wip)))
    (wip--add-buffer buffer wip)
    buffer))

(defun wip--get-pad (wip)
  "Return WIP's scratchpad buffer, creating it if necessary."
  (or (wip--pad-live-p wip)
      (progn
        (when (wip--pad-promoted-p wip)
          (wip--forget-pad wip))
        (wip--create-pad wip))))

(defun wip--write-pad-file (wip buffer)
  "Atomically write BUFFER's content to WIP's pad file.
The write goes through a temp file plus rename, the state directory
is created with owner-only permissions, and any I/O error is demoted
to a message so callers always complete."
  (with-demoted-errors "wip: error persisting scratchpad: %S"
    (let* ((file (wip--pad-file wip))
           (dir (file-name-directory file)))
      (with-file-modes #o700
        (make-directory dir t))
      (with-current-buffer buffer
        (let ((tmp (make-temp-file (expand-file-name "pad." dir))))
          (unwind-protect
              (progn
                (write-region nil nil tmp nil 'silent)
                (rename-file tmp file t))
            (when (file-exists-p tmp)
              (delete-file tmp))))))))

(defun wip--persist-pad (wip)
  "Write WIP's scratchpad to `wip--pad-file' if it is still the pad.
A promoted pad (renamed or saved to a file) is forgotten instead; see
`wip--forget-pad'."
  (if (wip--pad-promoted-p wip)
      (wip--forget-pad wip)
    (let ((pad (wip--pad-live-p wip)))
      (when pad
        (wip--write-pad-file wip pad)))))

(defun wip--persist-pad-final (wip)
  "Snapshot WIP's pad content to disk ahead of the wip being killed.
Unlike `wip--persist-pad', a promoted pad is not forgotten here: its
content is still written to `wip--pad-file', because the promoted
buffer itself may be about to be killed along with the wip.  Clears
the pad bookkeeping afterwards so the buffer sweep in `wip-kill'
cannot re-trigger pad handling via the kill hook."
  (let ((pad (wip--wip-pad-buffer wip)))
    (when (buffer-live-p pad)
      (wip--write-pad-file wip pad))
    (setf (wip--wip-pad-buffer wip) nil
          (wip--wip-pad-name wip) nil)))

(defun wip--persist-all-pads ()
  "Persist the scratchpads of all active wips.
Runs from `kill-emacs-hook'.  Stale wip records from a reload are
upgraded first so one incompatible record cannot abort the loop."
  (wip--upgrade-wips)
  (dolist (entry wip--wips)
    (wip--persist-pad (cdr entry))))

(defun wip-pad ()
  "Create or display the scratchpad of the current wip.
The scratchpad is a non-file buffer (named `wip-pad-buffer-name')
whose contents are persisted under `wip-directory'.  If the current
scratchpad has been renamed or saved to a file, a new empty one is
created."
  (interactive)
  (wip--ensure-current)
  (display-buffer (wip--get-pad wip--current)))

;;;; Firefox integration (EXWM)

(defun wip--firefox-buffer (wip)
  "Return the first EXWM Firefox buffer belonging to WIP, or nil."
  (seq-find (lambda (buffer)
              (with-current-buffer buffer
                (and (derived-mode-p 'exwm-mode)
                     (string-match-p
                      "firefox"
                      (downcase (or (bound-and-true-p exwm-class-name) ""))))))
            (wip--live-buffers wip)))

(defun wip-firefox ()
  "Display the current wip's Firefox buffer, launching one if needed.
Requires EXWM.  The launched Firefox window is captured by the
automatic buffer tracking when it appears, so it joins whichever wip
is current at that moment — stay in the wip until the window maps."
  (interactive)
  (wip--ensure-current)
  (unless (featurep 'exwm)
    (user-error "The command `wip-firefox' requires EXWM"))
  (let ((buffer (wip--firefox-buffer wip--current)))
    (if buffer
        (display-buffer buffer)
      (apply #'start-process "wip-firefox" nil wip-firefox-command)
      (message "Launching %s for wip %s"
               (car wip-firefox-command) (wip--wip-name wip--current)))))

;;;; Terminal integration

(defvar-local wip--terminal-wip nil
  "Wip record that owns this terminal buffer, or nil otherwise.")

(defun wip--terminal-buffer-name (wip)
  "Return the terminal buffer name reserved for WIP."
  (format "*wip terminal: %s*" (wip--wip-name wip)))

(defun wip--terminal-buffer (wip)
  "Return WIP's live terminal buffer, or nil."
  (seq-find (lambda (buffer)
              (and (eq (buffer-local-value 'wip--terminal-wip buffer) wip)
                   (or (not (with-current-buffer buffer
                              (derived-mode-p 'vterm-mode)))
                       (process-live-p (get-buffer-process buffer)))))
            (buffer-list)))

(defun wip--terminal-directory ()
  "Return a usable local directory for a new terminal."
  (if (and (not (file-remote-p default-directory))
           (file-directory-p default-directory))
      default-directory
    (file-name-as-directory (expand-file-name (or (getenv "HOME") "~")))))

(defun wip-terminal ()
  "Show the current wip's terminal, creating it if necessary.
Uses vterm when it is installed; otherwise starts Eshell.  The buffer
name is unique to the wip, so repeated calls return to the same
terminal."
  (interactive)
  (wip--ensure-current)
  (let* ((wip wip--current)
         (buffer (wip--terminal-buffer wip)))
    (if buffer
        (display-buffer buffer)
      (let ((name (generate-new-buffer-name (wip--terminal-buffer-name wip))))
        (let ((default-directory (wip--terminal-directory)))
          (setq buffer
                (if (require 'vterm nil t)
                    (progn
                      (vterm name)
                      (get-buffer name))
                  (let ((eshell (eshell t)))
                    (with-current-buffer eshell
                      (rename-buffer name))
                    eshell))))
        (with-current-buffer buffer
          (setq-local wip--terminal-wip wip))
        (display-buffer buffer)))))

;;;; Buffer tiling

(defconst wip--tile-empty-buffer-name " *wip tile empty*"
  "Name of the internal buffer used to complete a tile grid.")

(defun wip--matching-buffers (substring)
  "Return live buffers whose names contain SUBSTRING, ignoring case.
The internal blank tile buffer is excluded."
  (let ((case-fold-search t)
        (pattern (regexp-quote substring)))
    (seq-filter (lambda (buffer)
                  (and (not (equal (buffer-name buffer)
                                   wip--tile-empty-buffer-name))
                       (string-match-p pattern (buffer-name buffer))))
                (buffer-list))))

(defun wip-tile-buffers (substring)
  "Tile buffers whose names contain SUBSTRING in the selected frame.
Matching is case-insensitive and literal.  Empty cells are filled
with an internal blank buffer so every matching buffer gets the same
size."
  (interactive (list (read-string "Tile buffers matching: ")))
  (let ((buffers (wip--matching-buffers substring)))
    (unless buffers
      (user-error "No buffer names contain %S" substring))
    (let* ((count (length buffers))
           (columns (ceiling (sqrt count)))
           (rows (ceiling (/ (float count) columns)))
           (empty (get-buffer-create wip--tile-empty-buffer-name))
           (shown (append buffers
                          (make-list (- (* rows columns) count) empty)))
           (configuration (current-window-configuration)))
      (condition-case err
          (progn
            (delete-other-windows)
            (dotimes (_ (1- rows))
              (split-window-below))
            (dolist (window (window-list nil 'never))
              (with-selected-window window
                (dotimes (_ (1- columns))
                  (split-window-right))))
            (balance-windows)
            (let ((windows (window-list nil 'never)))
              (cl-mapc #'set-window-buffer windows shown)
              (select-window (car windows))))
        (quit
         (set-window-configuration configuration)
         (signal (car err) (cdr err)))
        (error
         (set-window-configuration configuration)
         (user-error "Cannot tile %d buffers: %s"
                     count (error-message-string err)))))))

;;;; Keymaps and mode

(defvar wip--session-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x b") #'wip-ido-switch-buffer)
    (define-key map (kbd "C-x k") #'wip-ido-kill-buffer)
    map)
  "Keymap active only while inside a wip.
Activated through `minor-mode-map-alist', keyed on `wip--current';
the entry is installed when `wip-mode' is first enabled.")

(defvar wip-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c w m") #'wip)
    (define-key map (kbd "C-c w q") #'wip-exit)
    (define-key map (kbd "C-c w k") #'wip-kill)
    (define-key map (kbd "C-c w b") #'wip-ibuffer)
    (define-key map (kbd "C-c w p") #'wip-pad)
    (define-key map (kbd "C-c w f") #'wip-firefox)
    map)
  "Keymap for `wip-mode'.")

;; `wip-mode-map' survives reloads, so install new bindings outside its `defvar'.
(define-key wip-mode-map (kbd "C-c w t") #'wip-terminal)
(define-key wip-mode-map (kbd "C-c w g") #'wip-tile-buffers)

;;;###autoload
(define-minor-mode wip-mode
  "Global minor mode providing the `C-c w' wip commands.
Entering a wip with `wip' enables this mode automatically.  Disabling
it exits wips in every frame (active wips are kept)."
  :global t
  :keymap wip-mode-map
  :lighter (:eval (if wip--current
                      (format " wip[%s]" (wip--wip-name wip--current))
                    " wip"))
  (if wip-mode
      (progn
        (wip--sync-frame-session)
        (add-hook 'buffer-list-update-hook #'wip--on-buffer-list-update)
        (add-hook 'window-selection-change-functions
                  #'wip--on-window-selection-change)
        (add-hook 'kill-emacs-hook #'wip--persist-all-pads)
        (add-to-list 'minor-mode-map-alist
                     (cons 'wip--current wip--session-map)))
    (dolist (frame (frame-list))
      (with-selected-frame frame
        (wip--sync-frame-session)
        (when wip--current
          (wip-exit))))
    (remove-hook 'buffer-list-update-hook #'wip--on-buffer-list-update)
    (remove-hook 'window-selection-change-functions
                 #'wip--on-window-selection-change)
    (setq minor-mode-map-alist
          (assq-delete-all 'wip--current minor-mode-map-alist))))

(provide 'wip)

;;; wip.el ends here
