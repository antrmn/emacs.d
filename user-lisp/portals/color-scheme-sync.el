;;; color-scheme-sync.el --- Sync portal color-scheme changes to gsettings  -*- lexical-binding: t; -*-

;; This is a temporary workaround until Emacs natively handles
;; org.freedesktop.portal.Settings SettingChanged signals for color-scheme sync.
;; Track upstream progress at: https://debbugs.gnu.org/

;;; Code:
(require 'dbus)

(defun color-scheme-sync--set-scheme (value)
  "Set gsettings color-scheme based on VALUE (1=dark, 0=light)."
  (call-process "gsettings" nil nil nil
                "set" "org.gnome.desktop.interface" "color-scheme"
                (if (eq value 1) "prefer-dark" "prefer-light")))

(defun color-scheme-sync--get ()
  "Get current color-scheme from portal as a numerical value."
  (caar (dbus-call-method
         :session
         "org.freedesktop.portal.Desktop"
         "/org/freedesktop/portal/desktop"
         "org.freedesktop.portal.Settings"
         "Read"
         "org.freedesktop.appearance"
         "color-scheme")))

(defvar color-scheme-sync--dbus-registration nil
  "DBus registration object for the portal SettingChanged signal.")

(defun color-scheme-sync--register ()
    (or color-scheme-sync--dbus-registration
      (setq color-scheme-sync--dbus-registration
            (dbus-register-signal
             :session
             "org.freedesktop.portal.Desktop"
             "/org/freedesktop/portal/desktop"
             "org.freedesktop.portal.Settings"
             "SettingChanged"
             (lambda (_namespace _key value)
               (color-scheme-sync--set-scheme (car value)))
             :arg-namespace "org.freedesktop.appearance"
             :arg1 "color-scheme"))))

(defun color-scheme-sync--unregister ()
    (when color-scheme-sync--dbus-registration
      (dbus-unregister-object color-scheme-sync--dbus-registration)
      (setq color-scheme-sync--dbus-registration nil)))

(defun color-scheme-sync--set-frame-bg (theme)
  "Sets variable `frame-background-mode' to THEME."
  (setopt frame-background-mode theme))

(defgroup color-scheme-sync nil
  "Sync XDG desktop portal color-scheme changes to GNOME gsettings."
  :group 'environment)

;;;###autoload
(defcustom color-scheme-sync t
  "Whether to synchronize the portal color-scheme with GNOME gsettings.

When non-nil, listens for XDG portal appearance changes and updates
`org.gnome.desktop.interface color-scheme` accordingly."
  :type 'boolean
  :group 'color-scheme-sync
  :set
  (lambda (symbol value)
    (set-default symbol value)
    (if value
        (progn
          (color-scheme-sync--set-scheme (color-scheme-sync--get))
          (color-scheme-sync--register)
          (add-hook 'toolkit-theme-set-functions #'color-scheme-sync--set-frame-bg))
      (color-scheme-sync--unregister)
      (remove-hook 'toolkit-theme-set-functions #'color-scheme-sync--set-frame-bg))))

(provide 'color-scheme-sync)
;;; color-scheme-sync.el ends here
