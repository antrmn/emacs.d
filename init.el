;;; init.el --- My Init File  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(use-package org :load-path "~/.config/emacs/elpa/org-mode/lisp/")
(setopt user-full-name "Antonio Romano"
        user-mail-address (string-join
                           (nreverse '("me" "." "pm" "@" "ntnrmn"))))

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;;scratch

(setopt inhibit-startup-message t)
(setopt initial-scratch-message nil)
(setopt desktop-save-mode t)

(require 'server)
(unless (server-running-p) (server-start))


(when (eq system-type 'android)
  (require 'os-android))
(when (memq system-type '(cygwin windows-nt ms-dos))
  (require 'os-windows))

(setopt custom-file
        (expand-file-name "custom.el" user-emacs-directory))

(setopt load-prefer-newer t)

;;package was here TODO

;;; Macros - syntax sugar
(require 'doom-lib)

;;; Modules
(require 'init-essential)
(require 'init-help)
(require 'init-completion)
(require 'init-minibuffer)
(require 'init-modeline)
(require 'init-window-buffers)
(require 'init-eye-candy)



;;; Major modes

(require 'conf-major-modes)
(require 'conf-org2)

;;; Plugins
(require 'org-lazy-babel)
;; TODO TODO TODO (require 'which-key-patch)
(require 'misc-utils)

(load custom-file 'noerror 'nomessage)
;;; End
(provide 'init)
;;init.el ends here
(put 'narrow-to-region 'disabled nil)
