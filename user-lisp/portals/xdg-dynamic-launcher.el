;; -*- lexical-binding: t; -*-
(require 'dbus)

(defun dbus--file-to-gbytes-icon (file)
  "Read FILE and return a D-Bus GIcon variant encoding a GBytesIcon."
  (let* ((bytes (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally file)
                  (string-to-list (buffer-string))))
         (tagged-bytes (mapcan (lambda (b) (list :byte b)) bytes)))
    `(:variant (:struct "bytes" (:variant ,tagged-bytes)))))

(defun xdg-dynamic-launcher--prepare-install (name icon)
    (dbus-call-method
     :session
     "org.freedesktop.portal.Desktop"
     "/org/freedesktop/portal/desktop"
     "org.freedesktop.portal.DynamicLauncher"
     "PrepareInstall"
     :timeout 30000
     "" name
     (dbus--file-to-gbytes-icon icon)
     (list :array
           (list :dict-entry "editable_icon" 
                 '(:variant t)))))

;;;###autoload
(defun xdg-dynamic-launcher-install (name icon id desktop-entry)
  (dbus-register-signal
   :session
   "org.freedesktop.portal.Desktop"
   (xdg-dynamic-launcher--prepare-install name icon)
   "org.freedesktop.portal.Request"
   "Response"
   (lambda (response-code results)
     (let ((token (caadr (assoc "token" results))))
       (dbus-call-method
        :session
        "org.freedesktop.portal.Desktop"
        "/org/freedesktop/portal/desktop"
        "org.freedesktop.portal.DynamicLauncher"
        "Install"
        :timeout 30000
        token
        id
        desktop-entry
        '(:array :signature "{sv}"))))))

(provide 'xdg-dynamic-launcher)
