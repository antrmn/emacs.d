;;;; Some other stuff
(setopt tool-bar-mode nil
        menu-bar-mode nil
        scroll-bar-mode nil)

(setopt window-divider-default-places t
        window-divider-mode t)

;; TODO fix
(setup dimmer-fraction 0.3
       dimmer-mode t)

(setopt beacon-blink-when-window-scrolls nil
        beacon-mode t)

;;;; Icons
(when (display-graphic-p)
  (require 'all-the-icons)
  (setopt all-the-icons-scale-factor 1.0)
  (add-hook 'ibuffer-mode-hook #'all-the-icons-ibuffer-mode)
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))


(with-eval-after-load 'ligature
  (ligature-set-ligatures t '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->"
                              "<-->" "<--->" "<---->" "<!--" "<==" "<===" "<="
                              "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>"
                              "<====>" "<!---" "<~~" "<~" "~>" "~~>" "::" ":::"
                              "==" "!=" "===" "!==" ":=" ":-" ":+" "<*" "<*>"
                              "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>"
                              "++" "+++")))

;;;; Cursor
(setopt cursor-in-non-selected-windows nil
        cursor-type '(hbar .  2))

(setopt window-divider-default-bottom-width 2
        window-divider-default-right-width 2
        beacon-color "misty rose"
        beacon-size 20)

;;; Setta tema
(load-theme 'nano t)
(require 'outline-minor-faces)
(set-face-attribute 'outline-minor-0 nil
                    :weight 'unspecified
                    :background 'unspecified
                    :slant 'italic
                    :extend t
                    :inherit 'font-lock-comment-face)
(set-face-attribute 'outline-minor-1 nil
                    :overline "gainsboro"
                    :inherit 'outline-minor-0)
(set-face-attribute 'outline-minor-2 nil
                    :inherit 'outline-minor-0)
(set-face-attribute 'outline-minor-3 nil
                    :inherit 'outline-minor-0)
(set-face-attribute 'outline-minor-4 nil
                    :inherit 'outline-minor-0)
(set-face-attribute 'outline-minor-5 nil
                    :inherit 'outline-minor-0)
(set-face-attribute 'outline-minor-6 nil
                    :inherit 'outline-minor-0)
(set-face-attribute 'outline-minor-7 nil
                    :inherit 'outline-minor-0)
(set-face-attribute 'outline-minor-8 nil
                    :inherit 'outline-minor-0)

(setopt modern-fringes-mode t)

(dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                      " *Minibuf-1*" " *Echo Area 1*"))
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (face-remap-add-relative 'default 'nano-faded)
      (face-remap-add-relative 'nano-strong :weight 'normal)
      (face-remap-add-relative 'default :weight 'light))))

(add-hook 'minibuffer-setup-hook
          (lambda()
             (face-remap-add-relative 'nano-strong :weight 'normal)
             (face-remap-add-relative 'default :weight 'light)))

(custom-set-faces
'(window-divider ((t (:foreground "white smoke"))))
'(form-feed-lighter ((t (:strike-through "light gray"))))
'(embark-target ((t (:inherit (nano-subtle)))))
'(orderless-match-face-0 ((t (:inherit (nano-salient nano-strong)))))
'(orderless-match-face-1 ((t (:inherit (nano-critical nano-strong)))))
`(orderless-match-face-2 ((t (:inherit nil :underline
                                       (:color ,nano-light-salient :style line
                                               :position nil)))))
`(orderless-match-face-3 ((t (:inherit nil :underline
                                       (:color ,nano-light-critical :style line
                                               :position nil)))))
'(diff-hl-insert ((t (:foreground unspecified :inherit diff-added default))
                  face-override-spec))
'(diff-hl-change ((t (:foreground unspecified :background unspecified
                                  :inherit diff-changed default))
                  face-override-spec))
'(diff-hl-delete ((t (:foreground unspecified :inherit diff-removed default))
                  face-override-spec))
'(flyspell-incorrect ((t (:underline (:color "blue violet" :style wave
                                             :position wave)
                                     :inherit nil))))
'(default ((t (:inherit nil :extend nil :stipple nil :background "#FFFFFF"
                        :foreground "#37474F" :inverse-video nil :box nil
                        :strike-through nil :overline nil :underline nil
                        :slant normal :weight regular :height 120
                        :width normal :foundry "ADBO" :family "Roboto Mono"))))
'(org-document-info ((t (:inherit org-document-title :height 0.7))))
'(org-document-title ((t (:inherit nano-strong :height 2.0 :family "Jost"))))
'(org-level-1 ((t (:inherit nano-strong :extend nil :height 1.8 :family "Jost"))))
'(org-level-2 ((t (:inherit nano-strong :extend nil :height 1.5 :family "Jost"))))
'(org-level-3 ((t (:inherit nano-strong :extend nil :height 1.3 :family "Jost"))))
'(org-level-4 ((t (:inherit nano-strong :extend nil :height 1.1 :family "Jost"))))
'(org-level-8 ((t (:inherit nano-strong :extend nil)))))



(setopt hl-todo-keyword-faces '(("TODO"   . "#FF6F00")))

(set-face-attribute 'variable-pitch nil
                    :family "Inter")

(add-hook 'prog-mode-hook (lambda ()(buffer-face-set '(:family "Iosevka"))))
(provide 'init-eye-candy)
