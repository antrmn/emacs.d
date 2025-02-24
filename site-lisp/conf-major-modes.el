(add-hook! 'prog-mode-hook '(hs-minor-mode
			    hl-todo-mode
			    hl-line-mode
			    diff-hl-mode
			    display-line-numbers-mode
			    flyspell-prog-mode
                ligature-mode))

(add-hook! '(java-mode-hook java-ts-mode-hook)
           '(eglot-ensure))


(add-hook! 'text-mode-hook '(flyspell-mode
			    mixed-pitch-mode))

(add-hook! 'markdown-mode-hook '(pandoc-mode))

(setopt markdown-enable-math t
	markdown-enable-highlighting-syntax t)

(add-hook! 'lisp-data-mode-hook #'outline-minor-mode)

(add-hook! 'python-base-mode-hook #'eglot-ensure)

(setopt pdf-view-display-size 'fit-page)
(after! pdf-tools
  (pdf-tools-install)
  (bind-keys :map pdf-view-mode-map
             ("\\" . hydra-pdftools/body)
             ("<s-spc>" .  pdf-view-scroll-down-or-next-page)
             ("g"  . pdf-view-first-page)
             ("G"  . pdf-view-last-page)
             ("l"  . image-forward-hscroll)
             ("h"  . image-backward-hscroll)
             ("j"  . pdf-view-next-page)
             ("k"  . pdf-view-previous-page)
             ("e"  . pdf-view-goto-page)
             ("u"  . pdf-view-revert-buffer)
             ("al" . pdf-annot-list-annotations)
             ("ad" . pdf-annot-delete)
             ("aa" . pdf-annot-attachment-dired)
             ("am" . pdf-annot-add-markup-annotation)
             ("at" . pdf-annot-add-text-annotation)
             ("y"  . pdf-view-kill-ring-save)
             ("i"  . pdf-misc-display-metadata)
             ("s"  . pdf-occur)
             ("b"  . pdf-view-set-slice-from-bounding-box)
             ("r"  . pdf-view-reset-slice))

  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))



(provide 'conf-major-modes)
;;; conf-major-modes.el ends here
