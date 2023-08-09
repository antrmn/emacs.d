;; Do not allow cursor in minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setopt enable-recursive-minibuffers t)

(setopt vertico-multiform-mode t
        vertico-mouse-mode t
        vertico-resize t
        vertico-cycle t
        vertico-mode t)
(bind-keys :map vertico-map
           ("RET" . vertico-directory-enter)
           ("DEL" . vertico-directory-delete-char)
           ("M-DEL" . vertico-directory-delete-word))
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)


(bind-keys :map minibuffer-local-map
	       ("M-a" . marginalia-cycle))

(setopt marginalia-mode t)

(bind-keys  :map minibuffer-local-map
            ("M-s" . consult-history) ; orig. next-matching-history-element
            ("M-r" . consult-history)); orig. previous-matching-history-element

(setopt consult-narrow-key "<")

(provide 'init-minibuffer)
