(use-package advice-patch)
:custom (which-key-show-early-on-C-h 'prompt)
 :bind (:map which-key-C-h-map
              ("m"   . my/which-key-call-embark-bindings-in-keymap)
              ("C-m" . my/which-key-call-embark-bindings-in-keymap))


              :config (advice-patch #'which-key-C-h-dispatch
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
