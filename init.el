;;; init.el --- My Init File  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:
;; (require 'benchmark-init)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)
(setopt gc-cons-treshold 10000000)
(add-hook 'emacs-startup-hook #'gcmh-mode)
(setopt custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'doom-lib)
(require 'misc-defs)

(setopt
 misc-add-crm-indicator t
 misc-embellish-recursion-indicator t
 misc-termux-setup-path (eq system-type 'android)
 aggressive-indent-mode t
 auth-sources '(default "secrets:Login")
 auto-revert-verbose nil
 auto-save-default t
 auto-save-file-name-transforms `((".*" "~/.emacs-saves/" t))
 auto-save-interval 200
 auto-save-timeout 20
 auto-window-vscroll nil
 backup-by-copying t
 backup-directory-alist  `(("." . ,(expand-file-name
                                    (concat user-emacs-directory "backups"))))
 backline-misc-enable t
 bookmark-default-file "~/.emacs-bookmark"
 buffer-menu-human-readable-sizes t
 color-scheme-sync (featurep 'dbusbind)
 completion-at-point-functions (list #'cape-file
                                     #'cape-dabbrev)
 completion-auto-help 'always
 completion-auto-select 'second-tab
 completion-category-defaults nil
 completion-category-overrides '((file (styles partial-completion)))
 completion-cycle-threshold 3
 completion-eager-display t
 completion-eager-display t
 completion-ignore-case t
 completion-misc-load-orderless t
 completion-pcm-leading-wildcard t
 completion-show-help nil
 completion-sort 'historical
 completion-styles '(orderless basic)
 completions-detailed t
 completions-format 'one-column
 completions-header-format nil
 completions-max-height 15
 confirm-nonexistent-file-or-buffer nil
 consult-narrow-key "<"
 cosult-misc-register-preview t
 context-menu-mode t
 corfu-mode t
 create-lockfiles nil
 current-language-environment "UTF-8"
 cursor-in-non-selected-windows nil
 cursor-type '(hbar .  2)
 custom-safe-themes t
 custom-enabled-themes '(libadwaita)
 debug-on-error t
 default-major-mode 'text-mode
 delete-by-moving-to-trash t
 delete-old-versions t
 delete-selection-mode t
 delete-trailing-whitespace-mode t
 desktop-path (list user-emacs-directory)
 desktop-dirname user-emacs-directory
 desktop-later-save-mode t
 desktop-load-locked-desktop t
 desktop-restore-eager t
 desktop-lazy-verbose t
 desktop-lazy-idle-delay 5
 desktop-save-misc-deferred-mode t
 diff-hl-misc-enabled-features '(diff-hl-flydiff-mode
                               diff-hl-margin-mode
                               diff-hl-show-hunk-mouse-mode)
 display-buffer-alist '(;;Window at bottom, anchored
                        ("\\*Completions\\*"
                         (display-buffer-reuse-window
                          display-buffer-at-bottom)
                         (window-height . 15)
                         (dedicated . t)
                         (preserve-size . (nil . t))))
 eldoc-box-only-multi-line t
 eldoc-echo-area-use-multiline-p nil
 electric-indent-actions '(yank)
 elisp-flymake-byte-compile-load-path load-path
 elisp-fontify-semantically t
 enable-recursive-minibuffers t
 fast-but-imprecise-scrolling t
 fill-column 80
 flymake-mode-line-lighter "⚡"
 flyspell-mode-line-string " 🌐"
 flyspell-misc-remove-mouse-face t
 frame-title-format '("%b")
 fringe-mode '(nil . 0)
 gc-cons-threshold most-positive-fixnum
 global-auto-revert-mode t
 global-auto-revert-non-file-buffers t
 global-prettify-symbols-mode t
 global-so-long-mode t
 highlight-indent-guides-method 'character
 horizontal-scroll-bar-mode t
 hs-display-lines-hidden t
 hs-indicator-type nil
 hs-show-indicators t
 ibuffer-old-time 24
 indent-tabs-mode nil
 inhibit-startup-message t
 initial-scratch-message nil
 ispell-quietly t
 ispell-misc-program-name (executable-find "hunspell")
 ispell-misc-hunspell-dictionary "it_IT,en_US"
 kept-new-versions 9
 kill-buffer-delete-auto-save-files t
 kill-buffer-quit-windows t ;; see also quit-restore-window-no-switch
 kill-do-not-save-duplicates t
 load-path-filter-function #'load-path-filter-cache-directory-files
 load-prefer-newer t
 make-backup-files t
 marginalia-align 'left
 marginalia-align-offest 0
 marginalia-mode t
 menu-bar-mode nil
 minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
 minibuffer-misc-cursor-intangible t
 minions-mode t
 mode-line-collapse-minor-modes '(gcmh-mode which-key-mode hs-minor-mode hs-mode
                                            eldoc-mode global-eldoc-mode
                                            outline-minor-mode form-feed-mode)
 mode-line-modes-delimiters nil
 modifier-bar-mode (eq system-type 'android)
 mouse-shift-adjust-mode t
 native-comp-async-report-warnings-errors 'silent ;; Don't pop up the *Warnings* buffer
 on-screen-global-mode t
 orderless-component-separator #'orderless-escapable-split-on-space
 outline-minor-mode-cycle t
 outline-mode-cycle-filter nil
 package-autosuggest-mode t
 pixel-scroll-precision-mode t
 project-mode-line t
 project-x-save-interval 600
 project-x-mode t
 project-prompter #'project-x--project-prompt
 quit-restore-window-no-switch t
 read-buffer-completion-ignore-case t
 read-extended-command-predicate #'command-completion-default-include-p
 read-file-name-completion-ignore-case t
 recentf-mode t
 register-preview-delay 0.5
 register-preview-function #'consult-register-format
 repeat-mode t
 require-final-newline t
 save-place-file (expand-file-name ".places" user-emacs-directory)
 save-place-mode t
 savehist-mode t
 ;; server-client-instructions nil
 ;; server-mode (not (server-running-p))
 server-stop-automatically 'delete-frame
 scroll-bar-mode 'right
 scroll-conservatively 101
 scroll-margin 0
 scroll-preserve-screen-position t
 show-paren-context-when-offscreen 'child-frame
 show-paren-mode t
 show-paren-when-point-inside-paren t
 smart-mark-mode t
 speedbar-prefer-window t
 switch-to-buffer-in-dedicated-window 'pop
 switch-to-buffer-obey-display-actions t
 switch-to-prev-buffer-skip #'misc-buf-not-in-window-history-p
 tab-always-indent 'complete
 tab-width 4
 temp-buffer-max-height 15
 temp-buffer-resize-mode t
 tool-bar-map (define-keymap
                "<speedbar>" '(menu-item "Speedbar" speedbar
                                         :enable t
                                         :help "Show speedbar"
                                         :vert-only t
                                         :image (image
                                                 :type svg
                                                 :file "/var/home/antonio/Downloads/icons/dock-left-symbolic.svg"))
                "<menu>" '(menu-item "Menu" menu-bar-open
                                         :enable t
                                         :help "Show Menu"
                                         :vert-only t
                                         :image (image
                                                 :type svg
                                                 :file "/var/home/antonio/Downloads/icons/menu-symbolic.svg")))
 tool-bar-mode t
 tool-bar-position 'bottom
 tool-bar-misc-global-merge-local t
 tramp-misc-enabled-methods '(flatpak toolbox)
 tramp-connection-properties (and (misc-in-flatpak-p)
                                  `(("/flatpak:" "tramp-login-program"
                                     ,(concat misc-flatpak-spawn-prefix "flatpak"))
                                    ("/toolbox:" "tramp-login-program"
                                     ,(concat misc-flatpak-spawn-prefix "toolbox"))
                                    ("/podman:" "tramp-login-program"
                                     ,(concat misc-flatpak-spawn-prefix "podman"))))
 treesit-auto-install-grammar 'always
 treesit-enabled-modes t
 treesit-misc-show-indicator t
 uniquify-after-kill-buffer-p t
 uniquify-buffer-name-style 'forward
 uniquify-ignore-buffers-re "^\\*"
 user-full-name "Antonio Romano"
 user-mail-address (string-join (reverse '("it" "." "posteo" "@" "cidra")))
 vc-make-backup-files t
 version-control t
 view-read-only t
 visible-bell nil
 visual-line-mode t
 visual-wrap-prefix-mode t
 which-key-mode t
 which-key-show-early-on-C-h t
 whitespace-page-delimiters-mode t
 window-divider-default-places t
 window-divider-mode t
 xref-mouse-mode t
 xref-show-definitions-function #'consult-xref
 xref-show-xrefs-function #'consult-xref
 delete-me-list '(("etc/images/open" . "folder-documents-symbolic")
                  ("etc/images/diropen" . "document-open")
                  ("etc/images/bookmark_add" . "bookmark-new")
                  ("images/package-menu/execute" . "object-select-symbolic")
                  ("images/package-menu/delete" . "list-remove-symbolic")
                  ("images/package-menu/unmark" . "checkbox-symbolic")
                  ("images/package-menu/url" . "web-browser")
                  ("etc/images/cancel" . "process-stop-symbolic")
                  ("etc/images/index" . "view-list-ordered-symbolic")
                  ("etc/images/close" . "application-exit-symbolic")
                  ("etc/images/cancel" ."web-browser")
                  ("etc/images/connect" . "network-transmit-receive-symbolic")
                  ("etc/images/contact" . "x-office-address-book-symbolic")
                  ("etc/images/disconnect" . "network-offline-symbolic")
                  ("etc/images/lock-broken" . "channel-insecure-symbolic")
                  ("etc/images/lock-ok" . "channel-secure-symbolic")
                  ("etc/images/lock" . "camera-photo-symbolic")
                  ("etc/images/next-page" . "go-next-symbolic")
                  ("etc/images/sort-column-ascending" . "view-sort-ascending-symbolic")
                  ("etc/images/sort-criteria" . "thunderbolt-symbolic")
                  ("etc/images/next-node" . "pan-end-symbolic")
                  ("etc/images/prev-node" . "pan-start-symbolic")
                  ("etc/images/up-node" . "pan-up-symbolic")
                  ("etc/images/sort-row-ascending" . "view-sort-ascending-symbolic")
                  ("images/gnus/toggle-subscription" . "starred-symbolic")
                  ("images/mail/copy" . "edit-copy-symbolic")
                  ("images/mail/forward" . "mail-forward-symbolic")
                  ("images/mail/inbox" . "view-refresh-symbolic")
                  ("images/mail/move" . "document-save-as-symbolic")
                  ("images/mail/not-spam" . "mail-mark-notjunk-symbolic")
                  ("images/mail/outbox" . "mail-send-symbolic")
                  ("images/mail/reply-all" . "mail-reply-all-symbolic")
                  ("images/mail/reply" . "mail-reply-sender-symbolic")
                  ("images/mail/save-draft" . "document-save-symbolic")
                   ("images/mail/save" . "document-save-symbolic")
                  ("images/mail/spam" . "mail-mark-junk-symbolic")
                  ("images/gud/break" . "face-angel-symbolic")
                  ("images/mail/preview" . "view-reveal-symbolic"))
 icon-map-list '(delete-me-list x-gtk-stock-map)
 )



(bind-keys
 ("C-=" . er/expand-region)
 ("C-." . embark-act)
 ("M-." . embark-dwim)
 ("C-h B" . embark-bindings)
 ("C-z" . vundo)
 ("<mouse-2>" . ignore)
 ("<remap> <zap-to-char>" . zop-to-char)
 ("<remap> <keyboard-quit>" . misc-keyboard-quit-dwim)
 ("C-c M-x" . consult-mode-command)
 ("M-y" . consult-yank-pop)              ; orig. yank-pop
 ([KEYCODE_CAMERA] . ignore)            ; Xperia
 ([KEYCODE_FOCUS]  . context-menu-open) ; Xperia
 :map input-decode-map
 ([volume-up]      . tool-bar-event-apply-meta-modifier)
 ([volume-down]    . tool-bar-event-apply-control-modifier)
 :map ctl-x-map
 ("r b" . consult-bookmark)
 ("M-:" . consult-complex-command)       ; orig. repeat-complex-command
 ("C-m" . consult-minor-mode-menu)
 ("C-z" . ignore)
 ("b" . consult-buffer)                  ; orig. switch-to-buffer
 ("4 b" . consult-buffer-other-window)   ; orig. switch-to-buffer-other-window
 ("5 b" . consult-buffer-other-frame)    ; orig. switch-to-buffer-other-frame
 ("p b" . consult-project-buffer)        ; orig. project-switch-to-buffer
 ("t" . shell)
 ("k" . kill-current-buffer)

 :map search-map                         ; M-s
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

 :map goto-map                           ; M-g
 ("e" . consult-compile-error)
 ("f" . consult-flymake)
 ("g" . consult-goto-line)              ; orig. goto-line
 ("M-g" . consult-goto-line)            ; orig. goto-line
 ("o" . consult-outline)                ; Alternative: consult-org-heading
 ("m" . consult-mark)
 ("k" . consult-global-mark)
 ("i" . consult-imenu)
 ("I" . consult-imenu-multi)

 :map image-map
 ("<mouse-1>" . org-open-at-point)

 :map minibuffer-local-map
 ("M-a" . marginalia-cycle)
 ("M-s" . consult-history)              ; orig. next-matching-history-element
 ("M-r" . consult-history)              ; orig. previous-matching-history-element

 :map visual-line-mode-map
 ("<remap> <fill-paragraph>" . ignore)) ; Disable fill-paragraph in visual-line-mode

(add-hook! 'completion-list-mode-hook (lambda ()
                                       (setq-local mode-line-format nil)
                                       (visual-wrap-prefix-mode 1)
                                       (set-window-fringes nil 0 0)
                                       (set-window-margins nil 0 0)
                                       (setq-local truncate-string-ellipsis "")))

(add-hook! 'emacs-lisp-mode-hook #'flymake-mode)

(add-hook! 'prog-mode-hook '(hs-minor-mode
			                 hl-line-mode
                             diff-hl-mode
			                 display-line-numbers-mode
			                 flyspell-prog-mode
                             ligature-mode))

(add-hook! '(java-mode-hook java-ts-mode-hook)
           '(eglot-ensure))

(add-hook! 'text-mode-hook '(flyspell-mode
			                 mixed-pitch-mode))

(add-hook! 'lisp-data-mode-hook #'outline-minor-mode)

(add-hook! 'python-base-mode-hook #'eglot-ensure)

(after! dired
  (bind-keys :map dired-mode-map
             ("<mouse-2>" . dired-mouse-find-file)))

(after! message
  (setopt
   message-auto-save-directory nil
   message-draft-headers '(References From Date)))

(after! org
  (require 'conf-org))
  
(after! gnus
  (require 'conf-gnus))

(load custom-file 'noerror 'nomessage)
;; Local Variables:
;; cool-misc-prettify: t
;; End:
