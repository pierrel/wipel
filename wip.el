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
;; - Any buffer created (except internal, space-prefixed ones) is
;;   automatically added to the wip.
;; - `C-x b' (`wip-ido-switch-buffer') is filtered to the wip's
;;   buffers; with a prefix argument it shows all buffers, and a
;;   buffer selected that way is brought into the wip.
;; - `C-x k' (`wip-ido-kill-buffer') evicts a buffer from the wip
;;   without killing it globally.
;; - The window configuration and the frame's tab-bar tabs are saved
;;   when you switch away and restored when you come back.
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
;;
;; Current limitations of this first pass:
;;
;; - State is per selected frame; multi-frame use is untested.
;; - Winner-mode integration (per-wip `C-c <left>'/`C-c <right>')
;;   is not implemented yet.
;; - The overview and history features from the README are not
;;   implemented yet, nor are project-scoped file selection and
;;   wip-scoped terminals/subcommands.
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
  "Non-nil means `wip-kill' kills buffers that belong to no other wip.
Buffers shared with other wips are always left alone."
  :type 'boolean)

(defcustom wip-firefox-command '("firefox" "--new-window")
  "Command (program and arguments) used by `wip-firefox'."
  :type '(repeat string))

;;;; Internal state

(cl-defstruct (wip--wip (:constructor wip--wip-create)
                        (:copier nil))
  "A single work-in-progress context."
  name
  (buffers nil)
  window-config
  tabs
  pad-buffer
  pad-name)

(defvar wip--wips nil
  "Alist of (NAME . WIP) for all active wips, most recently used first.")

(defvar wip--current nil
  "The current wip (a `wip--wip' struct), or nil when not in a wip.
Also used as the activation variable for `wip--session-map' in
`minor-mode-map-alist'.")

(defvar wip--global-window-config nil
  "Window configuration saved when entering wip from the global context.")

(defvar wip--global-tabs nil
  "Frame `tabs' parameter saved when entering wip from the global context.")

(defvar wip--known-buffers (make-hash-table :test #'eq :weakness 'key)
  "Set of buffers already seen, used to detect newly created buffers.")

;;;; Buffer bookkeeping

(defun wip--trackable-buffer-p (buffer)
  "Return non-nil if BUFFER should be tracked in a wip.
Internal buffers (whose names start with a space) are not tracked."
  (not (string-prefix-p " " (buffer-name buffer))))

(defun wip--live-buffers (wip)
  "Return the live buffers of WIP, pruning dead ones from its list."
  (setf (wip--wip-buffers wip)
        (seq-filter #'buffer-live-p (wip--wip-buffers wip))))

(defun wip--add-buffer (buffer wip)
  "Add BUFFER to WIP unless it is already a member or dead."
  (when (and (buffer-live-p buffer)
             (not (memq buffer (wip--wip-buffers wip))))
    (push buffer (wip--wip-buffers wip))))

(defun wip--remove-buffer (buffer wip)
  "Remove BUFFER from WIP.  The buffer is not killed."
  (setf (wip--wip-buffers wip)
        (remq buffer (wip--wip-buffers wip))))

(defun wip--buffer-owners (buffer)
  "Return the wips that BUFFER belongs to, as a list of structs."
  (seq-filter (lambda (wip) (memq buffer (wip--wip-buffers wip)))
              (mapcar #'cdr wip--wips)))

(defun wip--sync-known-buffers ()
  "Record every existing buffer as known, without adding any to a wip."
  (dolist (buffer (buffer-list))
    (puthash buffer t wip--known-buffers)))

(defun wip--on-buffer-list-update ()
  "Add buffers created while in a wip to the current wip.
Installed on `buffer-list-update-hook' only while a wip is current.
New buffers are those not yet in `wip--known-buffers'; buffers created
with buffer hooks inhibited are picked up on the next run."
  (when wip--current
    (dolist (buffer (buffer-list))
      (unless (gethash buffer wip--known-buffers)
        (puthash buffer t wip--known-buffers)
        (when (wip--trackable-buffer-p buffer)
          (wip--add-buffer buffer wip--current))))))

;;;; Entering, leaving and killing wips

(defvar wip-mode)

(defun wip--ensure-current ()
  "Signal a `user-error' unless a wip is current."
  (unless wip--current
    (user-error "Not in a wip (use `wip' / C-c w m to enter one)")))

(defun wip--read-wip-name (prompt &optional allow-new)
  "Read the name of a wip with PROMPT using ido completion.
When ALLOW-NEW is non-nil, any input is accepted; otherwise the input
must name an active wip."
  (let ((names (mapcar #'car wip--wips)))
    (when (and (null names) (not allow-new))
      (user-error "No active wips"))
    (ido-completing-read prompt names nil (not allow-new))))

(defun wip--activate ()
  "Install the hooks that make the wip machinery live."
  (add-hook 'buffer-list-update-hook #'wip--on-buffer-list-update)
  (add-hook 'kill-emacs-hook #'wip--persist-all-pads))

(defun wip--deactivate ()
  "Remove the buffer-tracking hook installed by `wip--activate'.
The `kill-emacs-hook' entry stays so scratchpads of inactive wips are
still persisted on exit."
  (remove-hook 'buffer-list-update-hook #'wip--on-buffer-list-update))

(defun wip--restorable-config-p (config)
  "Return non-nil if CONFIG is a window configuration that can be restored.
A configuration whose frame has been deleted passes
`window-configuration-p' but restores nothing, so it is rejected."
  (and (window-configuration-p config)
       (frame-live-p (window-configuration-frame config))))

(defun wip--save-state (wip)
  "Save the frame's window configuration and tabs into WIP.
Also persists WIP's scratchpad to disk; persistence errors are
demoted to messages so state transitions always complete."
  (setf (wip--wip-window-config wip) (current-window-configuration)
        (wip--wip-tabs wip) (frame-parameter nil 'tabs))
  (wip--persist-pad wip))

(defun wip--restore-state (wip)
  "Restore WIP's window configuration and tabs in the selected frame.
A wip that was never displayed before, or whose saved configuration
belongs to a deleted frame, gets a fresh single window showing its
scratchpad."
  (set-frame-parameter nil 'tabs (wip--wip-tabs wip))
  (if (wip--restorable-config-p (wip--wip-window-config wip))
      (set-window-configuration (wip--wip-window-config wip))
    (delete-other-windows)
    (switch-to-buffer (wip--get-pad wip)))
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
    (if wip--current
        (wip--save-state wip--current)
      (wip--save-global-state)
      (wip--activate))
    (setq wip--current wip)
    ;; Most recently used first.
    (setq wip--wips (cons (cons (wip--wip-name wip) wip)
                          (rassq-delete-all wip wip--wips)))
    (wip--sync-known-buffers)
    (wip--restore-state wip)
    (message "wip: %s" (wip--wip-name wip))))

;;;###autoload
(defun wip (name)
  "Switch to the wip called NAME, creating it if it does not exist.
Interactively, complete over the active wips; typing a new name
creates a fresh wip."
  (interactive (list (wip--read-wip-name "wip: " t)))
  (when (string-blank-p name)
    (user-error "A wip needs a name"))
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
    (message "Left wip %s" name)))

(defun wip-kill (name)
  "Kill the wip called NAME.
The wip's scratchpad content is snapshotted to disk first, and its
state directory under `wip-directory' is kept, so pad content
remains recoverable after the kill.  When
`wip-kill-exclusive-buffers' is non-nil, buffers belonging to no
other wip are killed globally.  Like `kill-buffer' generally, that
sweep discards unsaved non-file buffers without asking — including a
pad that was renamed away earlier and never saved to a file; save
promoted content if it must survive a wip kill.  If NAME is the
current wip, the global context is restored."
  (interactive (list (wip--read-wip-name "Kill wip: ")))
  (let* ((entry (assoc name wip--wips))
         (wip (cdr entry)))
    (unless wip
      (user-error "No wip named %s" name))
    (wip--persist-pad-final wip)
    (when (eq wip wip--current)
      (setq wip--current nil)
      (wip--deactivate)
      (wip--restore-global-state))
    (setq wip--wips (delq entry wip--wips))
    (when wip-kill-exclusive-buffers
      (dolist (buffer (wip--live-buffers wip))
        (unless (wip--buffer-owners buffer)
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
                               (wip--live-buffers wip--current)))))
      (if (null names)
          (message "No other buffers in wip %s (C-u C-x b to pull one in)"
                   (wip--wip-name wip--current))
        (switch-to-buffer
         (ido-completing-read "wip buffer: " names nil t))))))

(defun wip-ido-kill-buffer ()
  "Evict a buffer from the current wip, with ido completion.
The buffer is only removed from the wip, not killed globally."
  (interactive)
  (wip--ensure-current)
  (let ((names (mapcar #'buffer-name (wip--live-buffers wip--current))))
    (if (null names)
        (message "No buffers in wip %s" (wip--wip-name wip--current))
      (let* ((default (car (member (buffer-name) names)))
             (name (ido-completing-read "Evict from wip: " names nil t
                                        nil nil default))
             (buffer (get-buffer name)))
        (if (null buffer)
            (message "No such buffer: %s" name)
          (wip--remove-buffer buffer wip--current)
          (message "Evicted %s from wip %s"
                   name (wip--wip-name wip--current)))))))

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
removed from the wip but never killed globally.  Both regular marks
and deletion marks count."
  (interactive)
  (let ((wip (or wip-ibuffer--wip wip--current))
        (buffers (wip-ibuffer--target-buffers)))
    (unless wip
      (user-error "This ibuffer is not attached to a wip"))
    (dolist (buffer buffers)
      (wip--remove-buffer buffer wip))
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
re-creating a killed wip can recover it."
  (let ((file (wip--pad-file wip))
        (buffer (generate-new-buffer wip-pad-buffer-name)))
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
Runs from `kill-emacs-hook'."
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

;;;###autoload
(define-minor-mode wip-mode
  "Global minor mode providing the `C-c w' wip commands.
Entering a wip with `wip' enables this mode automatically.  Disabling
it exits the current wip (active wips are kept)."
  :global t
  :keymap wip-mode-map
  :lighter (:eval (if wip--current
                      (format " wip[%s]" (wip--wip-name wip--current))
                    " wip"))
  (if wip-mode
      (add-to-list 'minor-mode-map-alist
                   (cons 'wip--current wip--session-map))
    (when wip--current
      (wip-exit))))

(provide 'wip)

;;; wip.el ends here
