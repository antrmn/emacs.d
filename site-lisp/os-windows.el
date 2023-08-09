;; WINDOWS
(unless (memq system-type '(cygwin windows-nt ms-dos))
  (error "Not Windows"))

;; $HOME isn't normally defined on Windows, but many unix tools expect it.
(when-let (realhome
           (and (null (getenv-internal "HOME"))
                (getenv "USERPROFILE")))
  (setenv "HOME" realhome)
  (setq abbreviated-home-dir nil))

(provide 'os-windows)
