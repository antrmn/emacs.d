
(unless (eq system-type 'android)
  (error "Not Android"))

(setenv "PATH" (format "%s:%s" "/data/data/com.termux/files/usr/bin"
		       (getenv "PATH")))
(setenv "LD_LIBRARY_PATH" (format "%s:%s"
				  "/data/data/com.termux/files/usr/lib"
				  (getenv "LD_LIBRARY_PATH")))
(push "/data/data/com.termux/files/usr/bin" exec-path)

;; after init
(require 'tool-bar)
(add-hook 'after-init-hook
          (lambda ()
            (tool-bar-mode 1)
            (menu-bar-mode 1)
            (set-frame-parameter nil 'tool-bar-position-bottom)
            (modifier-bar-mode 1)
            (server-start)
            (define-key input-decode-map [volume-down] #'tool-bar-event-apply-control-modifier)
            (define-key input-decode-map [volume-up] #'tool-bar-event-apply-meta-modifier)

            (defun my/modifier-bar-button (init-modifier-list)
              "Decode the key sequence associated with a modifier bar button.
INIT-MODIFIER-LIST is a list of one symbol describing the button
being pressed.

Bind `modifier-bar-modifier-list' to INIT-MODIFIER-LIST.  Read
events, adding each subsequent modifier bar event's associated
modifier to that list while updating the tool bar to disable
buttons that were pressed.  Return any other event read with all
modifier keys read applied.

Temporarily disable text conversion and display the on screen
keyboard while doing so."
              ;; Save the previously used text conversion style.
              (let ((old-text-conversion-style text-conversion-style)
                    ;; Clear the list of modifiers currently pressed.
                    (modifier-bar-modifier-list init-modifier-list))
                ;; Disable text conversion.
                (when (fboundp 'set-text-conversion-style)
                  (set-text-conversion-style nil))
                (unwind-protect
                    (progn
                      ;; Display the on screen keyboard.
                      (frame-toggle-on-screen-keyboard nil nil)
                      ;; Update the tool bar to disable this modifier key.
                      (force-mode-line-update)
                      (let* ((modifiers init-modifier-list)
                             event1
                             (overriding-text-conversion-style nil)
                             (event (read-event)))
                        ;; Combine any more modifier key presses.
                        (while (memq event '(tool-bar volume-up volume-down))
                          (setq event1 (cond ((eq event 'tool-bar) (event-basic-type
                                                                    (read-event)))
                                             ((eq event 'volume-up) 'meta)
                                             ((eq event 'volume-down) 'control)))
                          ;; Reject unknown tool bar events.
                          (unless (memq event1 '(alt super hyper shift control meta))
                            (user-error "Unknown tool-bar event %s" event1))
                          ;; If `event' is the name of a modifier key, apply that
                          ;; modifier key as well.
                          (unless (memq event1 modifiers)
                            (push event1 modifiers)
                            ;; This list is used to check which tool bar buttons
                            ;; need to be enabled.
                            (push event1 modifier-bar-modifier-list))
                          ;; Update the tool bar to disable the modifier button
                          ;; that was read.
                          (force-mode-line-update)
                          (redisplay)
                          ;; Read another event.
                          (setq event (read-event)))
                        ;; EVENT is a keyboard event to which the specified list of
                        ;; modifier keys should be applied.
                        (vector (tool-bar-apply-modifiers event modifiers))))
                  ;; Re-enable text conversion if necessary.
                  (unless (or (not (fboundp 'set-text-conversion-style))
                              (eq old-text-conversion-style text-conversion-style))
                    (set-text-conversion-style old-text-conversion-style t))
                  ;; Re-enable all modifier bar buttons which may have been
                  ;; disabled.
                  (force-mode-line-update))))

            (advice-add 'modifier-bar-button :override #'my/modifier-bar-button)

            ;; TODO use it somewhere
            ;; (popup-menu (lookup-key-ignore-too-long global-map (vector 'menu-bar)))

            (global-set-key [KEYCODE_CAMERA] #'ignore)
            (global-set-key [KEYCODE_FOCUS] #'context-menu-open)))

(provide 'os-android)
