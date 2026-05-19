;;; misc-defs.el --- Miscellaneous Definitions  -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code
(defcustom cool-misc-prettify nil
  "turtle."
  :type 'boolean
  :local t
  :safe #'booleanp)

(add-hook 'hack-local-variables-hook
          (lambda ()
            (when cool-misc-prettify
              (setq-local prettify-symbols-alist
                          (cons '("misc" . ?🐢) prettify-symbols-alist))
              (setq-local prettify-symbols-compose-predicate
                          (lambda (start end _)
                            (or (prettify-symbols-default-compose-p start end _)
                                (eq (char-before start) ?-)
                                (eq (char-after end) ?-))))
              (prettify-symbols-mode 1))))



(defun misc-in-flatpak-p ()
  (or (getenv "FLATPAK_ID")
      (file-exists-p "/.flatpak-info")))
(defalias 'in-flatpak-p #'misc-in-flatpak-p)



(defcustom completion-misc-load-orderless nil
  "Load the `orderless' feature"
  :type 'boolean
  :set (lambda (_ val)
         (when val
           (require 'orderless))))



(defconst misc-flatpak-spawn-prefix "flatpak-spawn --host "
  "Prefix for flatpak spawn")

(defcustom tramp-misc-enabled-methods '()
  "List of symbols or strings representing TRAMP methods to enable.
See `tramp-enable-method'."
  :type '(repeat (choice symbol string))
  :group 'tramp-cmds
  :set (lambda (_ list)
         (require 'tramp)
         (dolist (method list)
           (tramp-enable-method method))))



(defvar misc--recursion-indicator
  '(:eval (let ((depth (recursion-depth)))
            (when (> depth 0)
              (format " ⤵️%d" depth))))
  "aa")

(put 'misc--recursion-indicator 'risky-local-variable t)

(defcustom misc-embellish-recursion-indicator nil
  "aa"
  :type 'boolean
  :group 'mode-line
  :set (lambda (_ set)
         (when set ;;not a use case to unset mid-session
           (setq mode-line-modes (remove "%[" (remove "%]" mode-line-modes)))
           (setq-default mode-line-format (append (default-value 'mode-line-format)
                                                  '(misc--recursion-indicator))))))



(defcustom treesit-misc-show-indicator nil
  "When non-nil, show a tree-sitter indicator in the mode line."
  :type 'boolean
  :set (lambda (sym val)
         (set-default sym val)
         (if val
             (add-to-list 'global-mode-string '(:eval (when (treesit-parser-list) "🌳")))
           (setq global-mode-string
                 (remove '(:eval (when (treesit-parser-list) "🌳")) global-mode-string)))))



(defun misc-get-history-list (&optional window)
  "Return linear history for WINDOW or current window."
  (let* ((window (or window (selected-window)))
	     (current-buffer (window-buffer window))
         (next-buffers (remq current-buffer (window-next-buffers window)))
         ;; `window-prev-buffers' gives a list of (BUFFER WINDOW-START POS)
	     (prev-buffers (mapcar #'car (window-prev-buffers window)))
	     ;; some buffers may be already killed or also appear in next-buffers
         (prev-buffers (seq-remove (lambda (buf) (or (not (buffer-live-p buf))
                                                     (memq buf next-buffers)))
                                   prev-buffers))
	     ;; `windows-prev-buffers' returns a most-recent-first ordered list
	     (prev-buffers (reverse prev-buffers)))
    (append prev-buffers (list current-buffer) next-buffers)))

(defun misc-popup-history-menu (event)
  "Spawn a context menu with selectable buffers from window selected by EVENT."
  (interactive "e")
  (let ((window (posn-window (event-start event))))
    (select-window window)
    (let* ((current-buffer (window-buffer window))
           (history (misc-get-history-list window))
           (map-to-entry (lambda (buf) (vector (buffer-name buf)
					                           buf
					                           (not (eq buf current-buffer)))))
	       (entries (mapcar map-to-entry history))
           (menu (cons "Buffer history" entries)))
      (when-let* ((choice (popup-menu menu))
                  (bufferp choice))
        (switch-to-buffer choice t t)))))

(defun misc-buf-not-in-window-history-p (window buffer _bury-or-kill)
  "Returns non-nil if BUFFER is not in history of WINDOW.
Used by `switch-to-prev-buffer-skip'"
  (not (or (assq buffer (window-prev-buffers window))
	       (memq buffer (window-next-buffers window)))))



;; https://emacsredux.com/blog/2025/06/01/let-s-make-keyboard-quit-smarter/
;; credits to Prot for this function
(defun misc-keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))



(defcustom misc-termux-setup-path t
  "Whether to add Termux binaries and libraries to PATH and variable `exec-path'."
  :type 'boolean
  :group 'environment
  :set (lambda (sym val)
         (set-default sym val)
         (let ((termux-bin "/data/data/com.termux/files/usr/bin")
               (termux-lib "/data/data/com.termux/files/usr/lib"))
           (if val
               (when (file-directory-p termux-bin)
                 (setenv "PATH" (concat termux-bin ":" (getenv "PATH")))
                 (setenv "LD_LIBRARY_PATH" (concat termux-lib ":" (getenv "LD_LIBRARY_PATH")))
                 (cl-pushnew termux-bin exec-path :test #'string=))
             (setenv "PATH" (string-join (delete termux-bin (split-string (getenv "PATH") ":")) ":"))
             (setenv "LD_LIBRARY_PATH" (string-join (delete termux-lib (split-string (or (getenv "LD_LIBRARY_PATH") "") ":")) ":"))
             (setq exec-path (delete termux-bin exec-path))))))



(defun tool-bar-misc--merge-with-local ()
  (when (local-variable-p 'tool-bar-map)
    (setq-local tool-bar-map
                (append (default-value 'tool-bar-map)
                        '((separator menu-item "--"))
                        (cdr tool-bar-map)))))

(defcustom tool-bar-misc-global-merge-local nil
  ""
  :type 'boolean
  :group 'tool-bar
  :set (lambda (sym var)
         (if var
             (add-hook 'after-change-major-mode-hook #'tool-bar-misc--merge-with-local)
           (remove-hook 'after-change-major-mode-hook #'tool-bar-misc--merge-with-local))))



(defun desktop-save-misc-deferred-mode--enable ()
  (unwind-protect
      (progn
        (desktop-save-mode 1)
        (desktop-read))
    (dolist (frame (frame-list))
      (set-frame-parameter frame 'visibility t))
    (setq default-frame-alist
          (assq-delete-all 'visibility default-frame-alist))
    (remove-hook 'server-after-make-frame-hook #'desktop-save-misc-deferred-mode--enable)))

(defcustom desktop-save-misc-deferred-mode nil
  "Enable `desktop-save-mode' after first emacsclient frame and then show frame.

If non-nil, first emacsclient frame is not shown until the desktop is loaded.
If set to t after the Emacs init process, this has no effect.
If set to t in a standalone Emacs instance, this sets `desktop-save-mode' to t."
  :type 'boolean
  :group 'desktop
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (if (not (daemonp))
                 ;; Fall back to normal (and weird) behaviour
                 ;;  where `desktop-read' is called inside
                 ;;  of `after-init-hook' if `desktop-save-mode'
                 ;;  is non-nil
                 (setopt desktop-save-mode t)
               (unless after-init-time
                 (add-to-list 'default-frame-alist '(visibility . nil))
                 (add-hook 'server-after-make-frame-hook
                           #'desktop-save-misc-deferred-mode--enable)))
           (remove-hook 'server-after-make-frame-hook
                        #'desktop-save-misc-deferred-mode--enable))))



(defcustom ispell-misc-program-name nil
  "`ispell-program-name' variant that defers setting when set."
  :type '(choice string
                 (const :tag "default" nil))
  :set (lambda (symbol value)
         (set-default symbol value)
         (when value
           (with-eval-after-load "ispell"
             (setopt ispell-program-name value)))))

(defcustom ispell-misc-hunspell-dictionary nil
  "`ispell-dictionary' variant that calls the appropriate hunspell function."
  :type '(choice string
                 (const :tag "default" nil))
  :set (lambda (symbol value)
         (set-default symbol value)
         (when value
           (with-eval-after-load "ispell"
             (setopt ispell-dictionary value)
             (ispell-hunspell-add-multi-dic value)))))



(defun flyspell-misc--remove-mouse-face (overlay)
  "Remove mouse face from flyspell OVERLAY."
  (and (overlayp overlay)
       (overlay-put overlay 'mouse-face nil))
  overlay)

(defcustom flyspell-misc-remove-mouse-face nil
  "When non-nil, remove mouse face from flyspell overlays."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (with-eval-after-load "flyspell"
           (if value
               (advice-add 'make-flyspell-overlay :filter-return
                           #'flyspell-misc--remove-mouse-face)
             (advice-remove 'make-flyspell-overlay
                            #'flyspell-misc--remove-mouse-face)))))



(defun misc-crm-indicator (args)
  "Add prompt indicator to `completing-read-multiple'.
We display [CRM<separator>], e.g., [CRM,] if the separator is a comma."
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))

(defcustom misc-add-crm-indicator nil
  "When non-nil, add a separator indicator to `completing-read-multiple'."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (advice-add 'completing-read-multiple :filter-args
                         #'crm-indicator)
           (advice-remove 'completing-read-multiple #'misc-crm-indicator))))



(defcustom diff-hl-misc-enabled-features nil
  "List of minor modes to enable alongside `diff-hl-mode'."
  :type '(set (const diff-hl-flydiff-mode)
              (const diff-hl-margin-mode)
              (const diff-hl-show-hunk-mouse-mode))
  :set (lambda (symbol value)
	 (dolist (feature (and (boundp symbol) (symbol-value symbol)))
           (remove-hook 'diff-hl-mode-hook feature))
         (set-default symbol value)
         (dolist (feature value)
           (add-hook 'diff-hl-mode-hook feature))))



(defcustom minibuffer-misc-cursor-intangible nil
  "When non-nil, enable `cursor-intangible-mode' in the minibuffer."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
           (remove-hook 'minibuffer-setup-hook #'cursor-intangible-mode))))



(defcustom backline-misc-enable nil
  "When non-nil, advise `outline-flag-region' to call `backline-update'."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (with-eval-after-load "outline"
           (if value
               (advice-add 'outline-flag-region :after #'backline-update)
             (advice-remove 'outline-flag-region #'backline-update)))))



(defcustom consult-misc-register-preview nil
  "When non-nil, replace `register-preview' with `consult-register-window'."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (if value
             (advice-add 'register-preview :override #'consult-register-window)
           (advice-remove 'register-preview #'consult-register-window))))



(defvar my/re-builder-positions nil
    "Store point and region bounds before calling re-builder")
  (advice-add 're-builder
              :before
              (defun my/re-builder-save-state (&rest _)
                "Save into `my/re-builder-positions' the point and region
positions before calling `re-builder'."
                (setq my/re-builder-positions
                      (cons (point)
                            (when (region-active-p)
                              (list (region-beginning)
                                    (region-end)))))))
(defun reb-replace-regexp (&optional delimited)
  "Run `query-replace-regexp' with the contents of re-builder. With
non-nil optional argument DELIMITED, only replace matches
surrounded by word boundaries."
  (interactive "P")
  (reb-update-regexp)
  (let* ((re (reb-target-value reb-regexp))
         (replacement (query-replace-read-to
                       re
                       (concat "Query replace"
                               (if current-prefix-arg
                                   (if (eq current-prefix-arg '-) " backward" " word")
                                 "")
                               " regexp"
                               (if (with-selected-window reb-target-window
                                     (region-active-p)) " in region" ""))
                       t))
         (pnt (car my/re-builder-positions))
         (beg (cadr my/re-builder-positions))
         (end (caddr my/re-builder-positions)))
    (with-selected-window reb-target-window
      (goto-char pnt) ; replace with (goto-char (match-beginning 0)) if you want
                      ; to control where in the buffer the replacement starts
                      ; with re-builder
      (setq my/re-builder-positions nil)
      (reb-quit)
      (query-replace-regexp re replacement delimited beg end))))

(provide 'misc-defs)
;;; misc-defs.el ends here
;; Local Variables:
;; cool-misc-prettify: t
;; End:
