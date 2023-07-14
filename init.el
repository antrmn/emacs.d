;;; init.el --- My Init File  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;; TODO: popper e project-x
;; TODO tab-bar-format aggiungi menu-bar e togli tab orizzontali
;; TODO no littering
;; TODO persistent scratch
(setopt user-full-name "Antonio Romano"
        user-mail-address (string-join
                           (nreverse '("me" "." "pm" "@" "ntnrmn"))))

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(setopt custom-file
        (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

(set-default-coding-systems 'utf-8)
(setopt load-prefer-newer t)
;;; Package and use-package
(setopt use-package-always-ensure t
        use-package-always-defer t
        use-package-expand-minimally t)
(setopt read-process-output-max (* 1024 1024))

(use-package package
  :custom
  (package-quickstart nil)
  (package-archives '(("melpa" . "https://melpa.org/packages/")
                      ("gnu" . "https://elpa.gnu.org/packages/")
                      ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
   :init
   (package-initialize))

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)
;;; File backup and autosave
(setopt backup-directory-alist  `(("." . ,(expand-file-name (concat user-emacs-directory "backups"))))
        backup-by-copying t
        make-backup-files t
        kept-new-versions 9
        delete-old-versions t)
(setopt auto-save-default t
        auto-save-timeout 20
        auto-save-interval 200)
;;; Emacs Server
(require 'server)
(unless (server-running-p)
  (server-start))

;;; Completion
(setopt completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))
        completion-cycle-threshold 3
        completions-detailed t
        read-extended-command-predicate #'command-completion-default-include-p)
(use-package cape) ;builtin
(use-package cape-yasnippet
  :vc (:fetcher github :repo elken/cape-yasnippet))
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)

;;; Snippets
(use-package yasnippet-snippets)
(use-package yasnippet-classic-snippets)
(use-package yasnippet)

;;; Minibuffer

;; Do not allow cursor in minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Generally ignore case when completing
(setopt read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)

;; I do not like recursive minibuffer, but may change idea...
(setopt enable-recursive-minibuffers t)

;; Add prompt indicator to `completing-read-multiple` to show the separator to use
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(use-package vertico
  :custom
  (vertico-multiform-mode t)
  (vertico-mouse-mode t)
  (vertico-resize t)
  (vertico-cycle t)
  (vertico-mode t)
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Add annotations to completion candidates in the minibuffer
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (:map minibuffer-local-map
  ("M-a" . marginalia-cycle))
  :custom
(marginalia-mode t))

(require 'nano-theme) ; Load nano-theme in order to set some face attributes down here. TODO remove

;; Set orderless as default completion style (with basic as fallback)
(use-package orderless
  :demand t
  :config
  ;; Define an orderless matching style that also looks for initials (abc -> async-byte-compile)
  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp)))
  ;; Basic must be included as a fallback completion style
  (setopt completion-styles '(orderless+initialism basic)
          orderless-component-separator #'orderless-escapable-split-on-space)
  :custom-face
  (orderless-match-face-0 ((t (:inherit (nano-salient nano-strong)))))
  (orderless-match-face-1 ((t (:inherit (nano-critical nano-strong)))))
  (orderless-match-face-2 ((t (:inherit nil :underline (:color ,nano-light-salient :style line :position nil)))))
  (orderless-match-face-3 ((t (:inherit nil :underline (:color ,nano-light-critical :style line :position nil))))))

;; Do not use orderless for file path completion
;; Use "basic" as first completion style in order to make it work with TRAMP
;; Note: not necessary with Emacs 30
;; Partial-completion: ~/.c/ema/s-l/as.el -> ~/.config/emacs/site-lisp/asoc.el
(setopt completion-category-overrides '((file (styles basic partial-completion))))



(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format))

(use-package consult-dir)
(use-package consult-eglot)
(use-package consult-flyspell)
(use-package consult-ls-git)
(use-package consult-project-extra)
(use-package consult-yasnippet)

;;; Code Completion

(setopt completion-in-region-function #'consult-completion-in-region)

;;TODO cape

;;; Scroll
(setopt scroll-conservatively 101
        scroll-margin 0
        scroll-preserve-screen-position t)
(setopt pixel-scroll-precision-mode t
        auto-window-vscroll nil
        fast-but-imprecise-scrolling t)
;;; Regexp builder search
(use-package xr) ;;convert regexp to rx
(require 'misc-utils)
(with-eval-after-load 're-builder
    (define-key reb-mode-map (kbd "RET") #'reb-replace-regexp)
    (define-key reb-lisp-mode-map (kbd "RET") #'reb-replace-regexp)
    (global-set-key (kbd "C-M-%") #'re-builder))
;;; Quit current context
(require 'misc-utils)
(global-set-key [remap keyboard-quit] #'keyboard-quit-context+)
;;; Uniquify
(setopt uniquify-after-kill-buffer-p t
        uniquify-buffer-name-style 'reverse
        uniquify-ignore-buffers-re "^\\*"
        uniquify-separator " • ")
;;; Bootstrap
(setopt initial-scratch-message ""
        inhibit-startup-screen nil
        default-major-mode 'text-mode)
;;; Modes that IMO should be on by default
(setopt global-so-long-mode t
        delete-selection-mode t)

;;; Where to put this!?
(setq create-lockfiles nil)

(setopt visual-line-mode t
        fill-column 80)

(require 'recentf)
(setopt recentf-mode t)

(setopt global-auto-revert-mode t
        global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)

(setopt switch-to-buffer-in-dedicated-window 'pop
        switch-to-buffer-obey-display-actions t)
(setopt ibuffer-old-time 24)

(setopt tab-always-indent 'complete
        indent-tabs-mode nil
        tab-width 4)

(setopt require-final-newline t)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
;; TODO set whitespace-mode, built-in way to see whitespaces

(setopt bookmark-default-file "~/.emacs-bookmark"
        confirm-nonexistent-file-or-buffer nil
        delete-by-moving-to-trash t
        display-line-numbers-type 'relative
        frame-resize-pixelwise t
        kill-do-not-save-duplicates t
        save-place-file (expand-file-name ".places" user-emacs-directory)
        view-read-only t
        visible-bell nil)


(use-package literate-calc-mode)
(use-package crux) ;;utility functions
(use-package no-littering)
(use-package highlight-indent-guides
  :custom (highlight-indent-guides-method 'character))
(use-package expand-region
  :bind ("C-=" . er/expand-region))





(use-package pandoc-mode)

(use-package markdown-mode
  :custom
  (markdown-enable-math t)
  (markdown-enable-highlighting-syntax t)
  :hook (markdown-mode . pandoc-mode))

;; Cool package that automatically convert Latex macros to
 ;; unicode characters. Use with C-\
(use-package math-symbols)


;;TODO doc tools/ doc toc tools?
(use-package doc-toc)
(use-package pdf-tools
  :custom
  (pdf-view-display-size 'fit-height)
  :config
  (pdf-tools-install))

(use-package nov)

(use-package aggressive-indent
  :custom
  (aggressive-indent-mode t))





(use-package vundo)





(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
	    '(("TODO"   . "#FF6F00"))))










(use-package denote)
(use-package denote-menu)
(use-package consult-notes)










(use-package dired-toggle-sudo)
(use-package dired-git-info)
(use-package dired-du)

(use-package ibuffer-vc)
(use-package ibuffer-project)
(use-package ibuffer-sidebar)


(use-package smart-mark
  :custom
  (smart-mark-mode t))

(use-package zop-to-char
  :init
  (global-set-key [remap zap-to-char] 'zop-to-char))


(use-package envrc) ;; per virtualenv

(use-package on-screen
  :custom
  (on-screen-global-mode t))


;;; Save history
(setopt savehist-mode t)
;;; Show paren
(setopt show-paren-mode t
        show-paren-context-when-offscreen 'child-frame
        show-paren-when-point-in-periphery t
        show-paren-when-point-inside-paren t)
;;; Global bindings
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "C-x t") 'shell)
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; TODO this is not a global bindings
;;disable fill-paragraph when visual-line-mode enabled
(define-key visual-line-mode-map [remap fill-paragraph] 'ignore)

;;; Tree sitter
;; TODO combobulate or ts-movement for structural editing
(use-package treesit-auto
  :custom
  (global-tree-sitter-mode t))

(use-package combobulate
  :vc (:fetcher github :repo mickeynp/combobulate))

;;; Eldoc
;; Never attempt to resize echo area when showing doc.
(setopt eldoc-echo-area-use-multiline-p nil)

;; If need to read long documentation (typically generated by Eglot)
;; just use eldoc-doc-buffer or eldoc-box's commands
(use-package eldoc-box
  :custom
  (eldoc-box-only-multi-line t)
  (eldoc-box-hover-at-point-mode t))
;; TODO bind eldoc-box-help-at-point and eldoc-doc-buffer

;;; Window manipulation
;;(use-package wind-move)
;; TODO alternativa: perspective.el
;; TODO iwindow: interactively manipulate windows
(setopt temp-buffer-max-height 15
        temp-buffer-resize-mode t)

(use-package ace-window)
(use-package transpose-frame)
(use-package eyebrowse)
(use-package windresize)
(use-package switchy-window)
(use-package other-frame-window)
(use-package winner)

;;; Garbage collector
;; Temporarily disabled in order to use emacs-gc-stats
;; (use-package gcmh
;;   :disabled t
;;   :custom
;;   (gcmh-mode t))

;;; Eglot
(use-package eglot
  :ensure t
  :commands eglot
  :config
  (add-to-list 'eglot-server-programs
               `(python-mode
                 . ,(eglot-alternatives '("pylsp"
                                          "jedi-language-server"
                                          ("pyright-langserver" "--stdio")))))
  (setq eglot-autoshutdown t))
(add-hook 'python-base-mode-hook 'eglot-ensure)

;;; Version control
(setopt version-control t
         vc-make-backup-files t)

(use-package magit
  :demand t)

(use-package forge)

(use-package magit-todos)

;; View changed lines
(use-package diff-hl
  :custom
  (diff-hl-margin-symbols-alist '((insert  .  "+") (delete . "-") (change . "*")
                                  (unknown .  "?") (ignored . "i")))
  ;; I should find a way to automatically unset all face attributes... TODO
  ;; Maybe swap changed and deleted foreground colors?
  :custom-face
  (diff-hl-insert ((t (:foreground unspecified :inherit diff-added default)) face-override-spec))
  (diff-hl-change ((t (:foreground unspecified :background unspecified :inherit diff-changed default)) face-override-spec))
  (diff-hl-delete ((t (:foreground unspecified :inherit diff-removed default)) face-override-spec))
  :hook
  (diff-hl-mode . diff-hl-flydiff-mode)
  (diff-hl-mode . diff-hl-margin-mode)
  (diff-hl-mode . diff-hl-show-hunk-mouse-mode)
  (prog-mode . diff-hl-mode))

;;; Help
;;TODO shortdoc
;;https://github.com/xuchunyang/elisp-demos/issues/14#issuecomment-1179648036

(use-package helpful
  :defines helpful-mode-map
  :bind
  (("<remap> <describe-command>" . helpful-command)
   ("<remap> <describe-function>" . helpful-callable)
   ("<remap> <describe-key>" . helpful-key)
   ("<remap> <describe-symbol>" . helpful-symbol)
   ("<remap> <describe-variable>" . helpful-variable)
   :map helpful-mode-map
   ("<remap> <revert-buffer>" . helpful-update)))

(with-eval-after-load 'elisp-demos
  (require 'elisp-demos)
  (elisp-demos-advice-describe-function-1))

(use-package elisp-demos
  :after helpful
  :config
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package info-colors
  :init
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))


;;; Spell checking
(require 'ispell)
(use-package ispell
  :demand t
  :custom
  (ispell-dictionary "it_IT,en_US")
  :config
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "it_IT,en_US"))

(use-package flyspell :disabled t
  :config
  (advice-add 'make-flyspell-overlay :filter-return #'my/remove-mouse-face)
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  :custom-face
  (flyspell-incorrect ((t
  (:underline
   (:color "blue violet" :style wave :position wave)
   :inherit nil)))))


;;; Keybindings
(setopt context-menu-mode t)
(setopt repeat-mode t)

(use-package advice-patch)
(use-package which-key
  :custom
  (which-key-mode t)s
  (which-key-show-early-on-C-h 'prompt)
  :bind (:map which-key-C-h-map
              ("m"   . my/which-key-call-embark-bindings-in-keymap)
              ("C-m" . my/which-key-call-embark-bindings-in-keymap))
  :config
  (advice-patch #'which-key-C-h-dispatch
                '(read-key (concat prompt " m" which-key-separator "show in minibuffer"))
                '(read-key prompt)))

(defun my/which-key-call-embark-bindings-in-keymap (&optional _)
  "Call the command `embark-bindings-in-keymap' from `which-key-C-h-map'.
This function is a copy of `which-key-show-standard-help'"
  (interactive)
  (let ((which-key-inhibit t)
        (popup-showing (which-key--popup-showing-p)))
    (which-key--hide-popup-ignore-command)
    ;; If prefix is active, show bindings with that prefix.
    ;; If there is no prefix (calling which-key-show-major-mode or which-key-show-top-level)
    ;; then bindings from global-map are shown
    (let ((keymap (if (string-empty-p (which-key--current-key-string))
                      global-map
                    (key-binding (kbd (which-key--current-key-string)) 'accept-default))))
      (embark-bindings-in-keymap keymap))))

(defun my/which-key--before-C-h-dispatch ()
  (when (and (not (which-key--popup-showing-p))
             (eq which-key-show-early-on-C-h 'prompt))
    (which-key--create-buffer-and-show (kbd (which-key--current-key-string)))
    (setq unread-command-events (listify-key-sequence
                                 (kbd (which-key--current-key-string))))))
(advice-add 'which-key-C-h-dispatch :before #'my/which-key--before-C-h-dispatch)

(use-package embark
  ;;TODO export, act all, collect, live, become (last one is incldued in C-.)
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :custom-face
  (embark-target ((t (:inherit (nano-subtle)))))
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  ;; Same as above but for Embark actions TODO unify
(add-to-list 'display-buffer-alist
               '("\\*Embark Actions\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;; Eye candy
;; TODO modern fringe
;; https://github.com/SpecialBomb/emacs-modern-fringes.git
;;;; Set theme
(use-package nano-theme
  :init
  (load-theme 'nano t))

;;;; Cursor
(setopt cursor-in-non-selected-windows nil
        cursor-type '(hbar .  2))
;;;; Fonts
(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

(dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                      " *Minibuf-1*" " *Echo Area 1*"))
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (face-remap-add-relative 'default 'nano-faded)
      (face-remap-add-relative 'nano-strong :weight 'normal)
      (face-remap-add-relative 'default :weight 'light))))

(add-hook 'minibuffer-setup-hook
          '(lambda()
             (face-remap-add-relative 'nano-strong :weight 'normal)
             (face-remap-add-relative 'default :weight 'light)))

(use-package prog-mode
  :ensure nil
  :hook
  (prog-mode . hl-line-mode)
  (prog-mode . display-line-numbers-mode)
  (prog-mode . (lambda() (buffer-face-set '(:family "Iosevka")))))

(set-face-attribute 'variable-pitch nil
                    :family "Inter")

(use-package ligature
  :custom
  (global-ligature-mode t)
  :config
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++")))
;;;; Icons
(use-package all-the-icons
  :custom
  (all-the-icons-scale-factor 1.0))

(use-package all-the-icons-ibuffer
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package all-the-icons-completion
  :custom
  (all-the-icons-completion-mode t))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;;;; Modeline
(use-package minions
  :custom
  (minions-mode t))
;;;; Some other stuff
(setopt tool-bar-mode nil
        menu-bar-mode nil
        scroll-bar-mode nil)

(setopt window-divider-default-bottom-width 2
        window-divider-default-right-width 2
        window-divider-default-places t
        window-divider-mode t)
(custom-set-faces
 '(window-divider ((t (:foreground "white smoke")))))

;; TODO fix
(use-package dimmer
  :custom
  (dimmer-fraction 0.3)
  (dimmer-mode t))

(use-package beacon
  :custom
  (beacon-color "misty rose")
  (beacon-size 20)
  (beacon-blink-when-window-scrolls nil)
  (beacon-mode t))

;; Display ugly ^L characters as horizontal lines
;; In Emacs by default you can add a ^L character with C-q C-l
;;   and you can navigaste them with backward-page and forward-page
(use-package form-feed
  :custom
  (global-form-feed-mode t)
  :custom-face
  (form-feed-lighter ((t (:strike-through "light gray")))))

;;; Code folding and outline
;; TODO Make outline-minor-mode work with C like languages and python
;; TODO Make decent bindings for outline-minor-mode + hideshow
;;      Particularly, define some repeatable keymap to move subtrees around
;; TODO Make outline-minor-mode recognizable from imenu

;;This overrides TABs when point is on headline. TODO find comfy alternative
(setopt outline-minor-mode-cycle t
        outline-minor-mode-cycle-filter nil)

;; Highlights section headings, but do not highlight top level sexps
;; Also, makes separate faces from the one used in the outline major mode
(use-package outline-minor-faces :demand t
  :after outline
  :hook
  (outline-minor-mode . outline-minor-faces-mode)
  :config
  (set-face-attribute 'outline-minor-0 nil :weight 'unspecified :background 'unspecified :slant 'italic :extend t :inherit 'font-lock-comment-face)
  (set-face-attribute 'outline-minor-1 nil :overline "gainsboro" :inherit 'outline-minor-0)
  (set-face-attribute 'outline-minor-2 nil :inherit 'outline-minor-0)
  (set-face-attribute 'outline-minor-3 nil :inherit 'outline-minor-0)
  (set-face-attribute 'outline-minor-4 nil :inherit 'outline-minor-0)
  (set-face-attribute 'outline-minor-5 nil :inherit 'outline-minor-0)
  (set-face-attribute 'outline-minor-6 nil :inherit 'outline-minor-0)
  (set-face-attribute 'outline-minor-7 nil :inherit 'outline-minor-0)
  (set-face-attribute 'outline-minor-8 nil :inherit 'outline-minor-0))

;; Extend face over line even if heading is collapsed
(use-package backline :demand t
  :after outline
  :config (advice-add 'outline-flag-region :after 'backline-update))

;; TODO where to put this?
(add-hook 'lisp-data-mode-hook #'outline-minor-mode)

(require 'misc-utils)
(use-package hideshow
  :hook ((prog-mode) . hs-minor-mode)
  :bind (("C-c h" . hs-toggle-hiding)))

;;; Org mode + Uncategorized
;;;; Package config
(use-package  olivetti)
(use-package el-easydraw
  :vc (:fetcher github :repo misohena/el-easydraw))
(use-package sketch-mode)
(use-package org-ql)
;(use-package org-roam-ql)
(use-package org-remark)
(use-package org-notify)
(use-package org-notifications)
(use-package org-special-block-extras)
(use-package org-transclusion)
(use-package ob-ipython) ; supporta inline matplotlib
(use-package ox-reveal)
(use-package ox-hugo)
(use-package org-ref)
(use-package org-present)
(use-package org-noter-pdftools)
(use-package org-noter)
(use-package org-notebook)
(use-package org-board)
(use-package org-caldav)
(use-package org-calibre-notes)
(use-package org-drill)
(use-package org-mpv-notes)
(use-package transcribe)
(use-package toc-org)
(use-package ob-mermaid)
(use-package ox-pandoc)


(use-package org
  :custom
  (org-indent-mode t)
  (org-indent-mode-turns-on-hiding-stars nil)
  (org-num-mode t)
  (org-appear-mode t)
  (global-org-modern-mode t)
  :config
  (require 'org-inlinetask)
  (require 'yasnippet)
  (require 'org-lazy-babel)
  ;(org-download-enable)
  :hook
  (org-mode . olivetti-mode)
  (org-mode . org-fragtog-mode)
  (org-mode . (lambda()(yas-activate-extra-mode 'latex-mode)))
  (org-mode . (lambda ()
                (local-set-key "\M-n" 'outline-next-visible-heading)
                (local-set-key "\M-p" 'outline-previous-visible-heading))))
(use-package org-download)
(setopt org-download-screenshot-method "spectacle -br -o %s")
(use-package org-modern
  :custom
  (org-modern-star nil)
  (org-modern-hide-stars (propertize "*" 'face
                                     '(font-lock-face (:foreground "#ededed")))))
(use-package org-fragtog)
(use-package org-cliplink)
(use-package org-recur)
(use-package org-protocol-capture-html
  :vc (:fetcher github :repo alphapapa/org-protocol-capture-html))
(use-package org-capture-ref)
(use-package cdlatex)

(use-package org-appear)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

;;;; Uncategorized
;; ffap (Find File At Point) is cool but shows an annoying *Completion* buffer
;; even when vertico is on. Let's disable it
(advice-add #'ffap-menu-ask :around
            (lambda (&rest args)
              (cl-letf (((symbol-function #'minibuffer-completion-help)
                         #'ignore))
                (apply args))))

;; tmm-menubar (Text Menu Bar) also show an undesired *Completion* buffer
(advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)

(setopt org-timer-display 'both)
(setopt org-table-header-line-p t)
(setopt org-edit-src-persistent-message t)
(setopt org-mouse-features '(context-menu move-tree yank-link activate-stars activate-bullets activate-checkboxes))
(setopt org-num-skip-unnumbered t
        org-num-skip-tags t
        org-num-skip-footnotes t
        org-num-skip-commented t
        org-num-max-level nil)
(setopt org-list-allow-alphabetical t)
(setopt org-checkbox-hierarchical-statistics nil) ;;Global COOKIE DATA recursive
(setopt org-list-indent-offset 2)
(setopt org-list-demote-modify-bullet
        '(("+" . "-") ("-" . "+") ("*" . "+")))
(setopt org-crypt-disable-auto-save-data 'ask)
(setopt org-use-speed-commands t)
(setopt org-inlinetask-show-first-start t)
(setopt org-goto-interface 'outline-path-completion)
(setopt org-footnote-auto-adjust t)
(setopt org-id-link-to-org-use-id nil)
(setopt org-attach-store-link-p 'attached)
(setopt org-attach-archive-delete nil)

(require 'org-mouse)
(custom-set-faces
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#FFFFFF" :foreground "#37474F" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 120 :width normal :foundry "ADBO" :family "Roboto Mono"))))
 '(org-document-info ((t (:inherit org-document-title :height 0.7))))
 '(org-document-title ((t (:inherit nano-strong :height 2.0 :family "Jost"))))
 '(org-level-1 ((t (:inherit nano-strong :extend nil :height 1.8 :family "Jost"))))
 '(org-level-2 ((t (:inherit nano-strong :extend nil :height 1.5 :family "Jost"))))
 '(org-level-3 ((t (:inherit nano-strong :extend nil :height 1.3 :family "Jost"))))
 '(org-level-4 ((t (:inherit nano-strong :extend nil :height 1.1 :family "Jost"))))
 '(org-level-8 ((t (:inherit nano-strong :extend nil)))))

(setopt org-attach-directory (expand-file-name "attachments" org-directory))
(setopt org-attach-method 'mv)
(setopt org-attach-use-inheritance nil)


;;;; Org roam
(use-package org-roam
  :custom
  (org-roam-db-autosync-mode t)
  (org-roam-node-display-template (concat "${title:*} "
                                          (propertize "${tags:10}" 'face 'org-tag))))
(use-package org-roam-ui)
(use-package consult-org-roam)


;;; Fun packages
(use-package zones)
(use-package zone-nyan)
(use-package zone-rainbow)
(use-package zone-select)
(use-package zone-sl)
(use-package landmark)

;;; End
(provide 'init)
;;init.el ends here
