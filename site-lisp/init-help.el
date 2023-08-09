;;; Help
;;TODO shortdoc
;;https://github.com/xuchunyang/elisp-demos/issues/14#issuecomment-1179648036

(bind-keys ("<remap> <describe-command>" . helpful-command)
            ("<remap> <describe-function>" . helpful-callable)
            ("<remap> <describe-key>" . helpful-key)
            ("<remap> <describe-symbol>" . helpful-symbol)
            ("<remap> <describe-variable>" . helpful-variable))

(after! helpful
  (bind-keys :map helpful-mode-map
             ("<remap> <revert-buffer>" . helpful-update)))

(advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)
(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(provide 'init-help)
