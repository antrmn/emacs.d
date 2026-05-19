;; -*- lexical-binding: t; -*-
(require 'seq)
(require 'dbus)

(defconst xdg-portal-inhibit--flags
  '((logout      . 1)
    (user-switch . 2)
    (suspend     . 4)
    (idle        . 8)))

(defun xdg-portal-inhibit--compute-flags (flags)
  (let ((flags (ensure-list flags)))
    (seq-reduce #'logior
                (mapcar (lambda (f) (alist-get f xdg-portal-inhibit--flags))
                        flags)
                0)))

;;;###autoload
(defun xdg-portal-inhibit (flags &optional reason)
  (apply #'dbus-call-method
         :session
         "org.freedesktop.portal.Desktop"
         "/org/freedesktop/portal/desktop"
         "org.freedesktop.portal.Inhibit"
         "Inhibit"
         ""
         (xdg-portal-inhibit--compute-flags flags)
         (when reason
           `((:array (:dict-entry "reason"
                                  (:variant ,reason)))))))

(defun xdg-portal-inhibit-close (handle)
    (dbus-call-method
     :session
     "org.freedesktop.portal.Desktop"
     handle
     "org.freedesktop.portal.Request"
     "Close"))



(defvar xdg-portal-inhibit--unsaved-buf-handle nil)

(defun xdg-portal-inhibit--check-unsaved-buf ()
  (if-let* ((modified (buffer-modified-p)))
      (and buffer-file-name
           (not xdg-portal-inhibit--unsaved-buf-handle)
           (setq xdg-portal-inhibit--unsaved-buf-handle
                 (xdg-portal-inhibit 'logout "There are unsaved buffers"))))
  (when (and buffer-file-name
             xdg-portal-inhibit--unsaved-buf-handle
             (not (seq-some #'buffer-modified-p
                            (seq-filter #'buffer-file-name
                                        (buffer-list)))))
    (xdg-portal-inhibit-close xdg-portal-inhibit--unsaved-buf-handle)
    (setq xdg-portal-inhibit--unsaved-buf-handle nil)))

(defgroup xdg-portal-inhibit nil
  "aa")

(defcustom xdg-portal-inhibit-on-unsaved-buffers nil
  :type 'boolean
  :group 'xdg-portal-inhibit
  :set (lambda (_sym var)
         (if var
             (add-hook 'post-command-hook #'xdg-portal-inhibit--check-unsaved-buf)
           (remove-hook 'post-command-hook #'xdg-portal-inhibit--check-unsaved-buf))))

(provide 'xdg-portal-inhibit)
