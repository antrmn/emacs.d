(setopt completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))
        completion-cycle-threshold 3
        completions-detailed t
        read-extended-command-predicate #'command-completion-default-include-p)

(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)

;; Generally ignore case when completing
(setopt read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)

;; Add prompt indicator to `completing-read-multiple` to show the separator to use
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(require 'orderless)
;; Set orderless as default completion style (with basic as fallback)
;; Define an orderless matching style that also looks for initials (abc -> async-byte-compile)
(orderless-define-completion-style orderless+initialism
				   (orderless-matching-styles '(orderless-initialism
								orderless-literal
								orderless-regexp)))
;; Basic must be included as a fallback completion style
(setopt completion-styles '(orderless+initialism basic)
        orderless-component-separator #'orderless-escapable-split-on-space)

;; Do not use orderless for file path completion
;; Use "basic" as first completion style in order to make it work with TRAMP
;; Note: not necessary with Emacs 30
;; Partial-completion: ~/.c/ema/s-l/as.el -> ~/.config/emacs/site-lisp/asoc.el
(setopt completion-category-overrides '((file (styles basic partial-completion))))


;; Enable automatic preview at point in the *Completions* buffer. This is
;; relevant when you use the default completion UI.
(add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)


;; Optionally configure the narrowing key.
;; Both < and C-+ work reasonably well.
(setq consult-narrow-key "<") ;; "C-+"


(provide 'init-completion)
