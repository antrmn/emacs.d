;;; init.el --- My Init File  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(setopt user-full-name "Antonio Romano"
        user-mail-address (string-join
                           (nreverse '("me" "." "pm" "@" "ntnrmn"))))

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(when (eq system-type 'android)
  (require 'os-android))
(when (memq system-type '(cygwin windows-nt ms-dos))
  (require 'os-windows))

(setopt custom-file
        (expand-file-name "custom.el" user-emacs-directory))

(setopt load-prefer-newer t)

;;; Package Manager
(setopt package-quickstart t
        package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(require 'init-package-list)

;;; Macros - syntax sugar
(require 'doom-lib)

;;; Modules
(require 'no-littering)
(require 'init-essential)
(require 'init-help)
(require 'init-completion)
(require 'init-minibuffer)
(require 'init-modeline)
(require 'init-window-buffers)
(require 'init-eye-candy)



;;; Major modes
;; TODO TODO TODO (require 'init-gnus)
(require 'conf-major-modes)
(require 'conf-org)

;;; Plugins
(require 'org-lazy-babel)
;; TODO TODO TODO (require 'which-key-patch)
(require 'misc-utils)

;(load custom-file 'noerror 'nomessage)
;;; End
(provide 'init)
;;init.el ends here
