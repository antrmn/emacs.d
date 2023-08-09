
;;; Editing
(set-default-coding-systems 'utf-8)
;;;; Soft wrap
(setopt visual-line-mode t
        fill-column 80)

;; Disable fill-paragraph when visual-line-mode enabled
(define-key visual-line-mode-map [remap fill-paragraph] 'ignore)

;;;; Indentation
(setopt tab-always-indent 'complete
        indent-tabs-mode nil
        tab-width 4
	highlight-indent-guides-method 'character
	aggressive-indent-mode t)

;;;; Whitespaces
;; TODO set whitespace-mode, built-in way to see whitespaces
(setopt require-final-newline t)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;;; Parens
(setopt show-paren-mode t
        show-paren-context-when-offscreen 'child-frame
        show-paren-when-point-in-periphery t
        show-paren-when-point-inside-paren t)

;;; Scroll
(setopt scroll-conservatively 101
        scroll-margin 0
        scroll-preserve-screen-position t
	    pixel-scroll-precision-mode t
	    auto-window-vscroll nil
	    fast-but-imprecise-scrolling t
	    on-screen-global-mode t)


;;;; Selecting (Marking) stuff and also killing and yanking
(setopt delete-selection-mode t
	smart-mark-mode t)

(require 'misc-utils)
(bind-keys ("C-=" . er/expand-region))
(global-set-key [remap zap-to-char] 'zop-to-char)

;;;; Code folding
;; TODO Make outline-minor-mode work with C like languages and python
;; TODO Make decent bindings for outline-minor-mode + hideshow
;;      Particularly, define some repeatable keymap to move subtrees around
;; TODO Make outline-minor-mode recognizable from imenu

(setopt outline-minor-mode-cycle t
	outline-mode-cycle-filter nil)

(after! outline
  (advice-add 'outline-flag-region :after 'backline-update))

(add-hook 'outline-minor-mode-hook #'outline-minor-faces-mode)

(require 'misc-utils) ;;just to get hs-toggle-hiding. remove
(bind-keys ("C-c h" . hs-toggle-hiding))

;;;; Undo/redo
(bind-keys ("C-z" . vundo))

;;;; Search and replace

;; Add stuff in isearch-mode-map
(require 'misc-utils)
(after! 're-builder
    (define-key reb-mode-map (kbd "RET") #'reb-replace-regexp)
    (define-key reb-lisp-mode-map (kbd "RET") #'reb-replace-regexp)
    (global-set-key (kbd "C-M-%") #'re-builder))

;; Xref
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)


;;;; Line numbers
(setopt display-line-numbers-type 'relative)


;;; Registers
;; TODO add consult bindings in C-x r

;; Optionally configure the register formatting. This improves the register
;; preview for `consult-register', `consult-register-load',
;; `consult-register-store' and the Emacs built-ins.
(setq register-preview-delay 0.5
      register-preview-function #'consult-register-format)
(advice-add #'register-preview :override #'consult-register-window)


;; Do not create lockfiles
(setopt create-lockfiles nil)

;; Keep track of recently opened files
(setopt recentf-mode t)

;;;; Auto revert
(setopt global-auto-revert-mode t
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)

;;;; Backup
(setopt
	backup-directory-alist  `(("." . ,(expand-file-name
					   (concat user-emacs-directory "backups"))))
        backup-by-copying t
        make-backup-files t
        kept-new-versions 9
        delete-old-versions t)

;;;; Auto save
(setopt auto-save-default t
        auto-save-timeout 20
        auto-save-interval 200)

;;;; So long
(setopt global-so-long-mode t)

;;;; Savehist
(setopt savehist-mode t)

;;;; Saveplace
(setopt save-place-file (expand-file-name ".places" user-emacs-directory)
	save-place-mode t)

;;;; Bookmark
(setopt bookmark-default-file "~/.emacs-bookmark")

;; remap bookmark-jump
(bind-keys :map ctl-x-map
           ("r b" . consult-bookmark))


;;;; Uniquify
(setopt uniquify-after-kill-buffer-p t
        uniquify-buffer-name-style 'reverse
        uniquify-ignore-buffers-re "^\\*"
        uniquify-separator " • ")

;;;; Misc
(setopt confirm-nonexistent-file-or-buffer nil
	delete-by-moving-to-trash t
	kill-do-not-save-duplicates t
	view-read-only t
	visible-bell nil
	default-major-mode 'text-mode)

;; Bindings
(setopt repeat-mode t)

;; Enable contextual menu
(setopt context-menu-mode t)

;; ffap (Find File At Point) is cool but shows an annoying *Completion* buffer
;; even when vertico is on. Let's disable it
(advice-add #'ffap-menu-ask :around
            (lambda (&rest args)
              (cl-letf (((symbol-function #'minibuffer-completion-help)
                         #'ignore))
                (apply args))))

;; tmm-menubar (Text Menu Bar) also show an undesired *Completion* buffer
(advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)

(setopt which-key-mode t
        which-key-show-early-on-C-h t)

(bind-keys ("C-." . embark-act)
           ("M-." . embark-dwim)
           ("C-h B" . embark-bindings))
;;TODO export, act all, collect, live, become (last one is incldued in C-.)
;;Hide the mode line of the Embark live/completions buffers
(add-to-list 'display-buffer-alist
	         '("\\*Embark Collect \\(Live\\|Completions\\)\\*"
	           nil
	           (window-parameters (mode-line-format . none))))

(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

;;;; Quit current context
(require 'misc-utils)
(global-set-key [remap keyboard-quit] #'keyboard-quit-context+)

;;; Spell checking
(setopt ispell-dictionary "it_IT,en_US")
(after! ispell
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "it_IT,en_US"))

(after! flyspell
  (advice-add 'make-flyspell-overlay :filter-return #'my/remove-mouse-face))

;; Display ugly ^L characters as horizontal lines
;; In Emacs by default you can add a ^L character with C-q C-l
;;   and you can navigaste them with backward-page and forward-page
(setopt global-form-feed-mode t)

;;; Version control
(setopt version-control t
         vc-make-backup-files t)

;; View changed lines
(setopt diff-hl-margin-symbols-alist '((insert  .  "+") (delete . "-") (change . "*")
                                       (unknown .  "?") (ignored . "i")))

(add-hook! 'diff-hl-mode-hook '(diff-hl-flydiff-mode
				diff-hl-margin-mode
				diff-hl-show-hunk-mouse-mode))

;; Eglot - LSP client
(setopt eglot-autoshutdown t)

;; Eldoc
;; Never attempt to resize echo area when showing doc.
(setopt eldoc-echo-area-use-multiline-p nil
	;; If need to read long documentation (typically generated by Eglot)
	;; just use eldoc-doc-buffer or eldoc-box's commands
	;; TODO bind eldoc-box-help-at-point and eldoc-doc-buffer
        eldoc-box-only-multi-line t
        eldoc-box-hover-at-point-mode t)

;;; Global binding

(bind-keys
 ("C-z" . ignore)
 ("C-c l" . org-store-link)
 ("C-c a" . org-agenda)
 ("C-c c" . org-capture)
 ("C-c M-x" . consult-mode-command)
 ("M-y" . consult-yank-pop)        ; orig. yank-pop
 :map ctl-x-map
 ("M-:" . consult-complex-command) ; orig. repeat-complex-command
 ("C-m" . consult-minor-mode-menu)
 ("C-z" . ignore)
 ("b" . consult-buffer) ; orig. switch-to-buffer
 ("4 b" . consult-buffer-other-window) ; orig. switch-to-buffer-other-window
 ("5 b" . consult-buffer-other-frame) ; orig. switch-to-buffer-other-frame
 ("p b" . consult-project-buffer) ; orig. project-switch-to-buffer
 ("t" . shell)
 ("k" . kill-current-buffer)
 :map search-map ; M-s
 ("f" . consult-find)
 ("F" . consult-locate)
 ("g" . consult-grep)
 ("G" . consult-git-grep)
 ("r" . consult-ripgrep)
 ("l" . consult-line)
 ("L" . consult-line-multi)
 ("k" . consult-keep-lines)
 ("u" . consult-focus-lines)
 ("e" . consult-isearch-history)
 :map help-map
 ("u i" . consult-info)
 ("u m" . consult-man)
 :map goto-map ; M-g
 ("e" . consult-compile-error)
 ("f" . consult-flymake)
 ("g" . consult-goto-line)      ; orig. goto-line
 ("M-g" . consult-goto-line)    ; orig. goto-line
 ("o" . consult-outline)        ; Alternative: consult-org-heading
 ("m" . consult-mark)
 ("k" . consult-global-mark)
 ("i" . consult-imenu)
 ("I" . consult-imenu-multi))

;;TODO find decent alternatives
 ;; ("C-c h" . consult-history)
 ;; ("C-c k" . consult-kmacro)

(provide 'init-essential)
