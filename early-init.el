;;; early-init.el --- Emacs early init -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs 27 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens.

;;; Code:



;; PERF: Garbage collection is a big contributor to startup times. This fends it
;;   off, but will be reset later by `gcmh-mode'. Not resetting it later will
;;   cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook #'gcmh-mode)


;; PERF: Don't use precious startup time checking mtime on elisp bytecode.
;;   Ensuring correctness is 'doom sync's job, not the interactive session's.
;;   Still, stale byte-code will cause *heavy* losses in startup efficiency.
;;(setq load-prefer-newer noninteractive) ; Disabled because i don't need it.

;; (package-activate-all)
(setenv "GTK_THEME" "adw-gtk3" t)

;; Disable package at startup
(setq package-enable-at-startup nil)


;;; Package Manager
(setopt package-quickstart t
        package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; ;; Less clutter when starting up
;; (push '(tool-bar-lines . 0) default-frame-alist)
;; (push '(menu-bar-lines . 0) default-frame-alist)
;; (push '(left-fringe . 0) default-frame-alist)
;; (push '(right-fringe . 0) default-frame-alist)
;; (push '(undecorated-round . t) default-frame-alist)
;; (push '(vertical-scroll-bars . nil) default-frame-alist)
;; (push '(horizontal-scroll-bars . nil) default-frame-alist)
;; (push '(internal-border-width . 18) default-frame-alist)
;; (push '(child-frame-border-width . 2) default-frame-alist)
;; (push '(width . 85) default-frame-alist)
;; (push '(height . 42) default-frame-alist)
;; (push '(background-color . "white") default-frame-alist)
;; (setq mode-line-format nil)

;; ;; Resizing the Emacs frame can be a terribly expensive part of changing the font
;; (setq frame-inhibit-implied-resize t
;;       frame-resize-pixelwise t)

;; ;; Ignore X resources
;; (advice-add #'x-apply-session-resources :override #'ignore)

;; ;; Log native compilation warnings but do not pop up the *Warnings* buffer
;; (setq native-comp-async-report-warnings-errors 'silent)

(setq-default frame-title-format '("%b"))


;; ;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
;; (setq idle-update-delay 1)

;; ;; Disabling bidi (bidirectional editing stuff)
;; (setq-default bidi-display-reordering 'left-to-right
;;               bidi-paragraph-direction 'left-to-right)
;; (setq bidi-inhibit-bpa t)

;; (setq fast-but-imprecise-scrolling t)

(package-initialize)
(package-activate-all)
(load-theme 'my-adwaita t)
(require 'custom-css)
(setopt auto-dark-mode t)
;; (custom-css-load 'lol (with-temp-buffer
;; 			(insert-file-contents
;; 			 (expand-file-name "./style.css" user-emacs-directory))
;; 			(buffer-string)))

(push '(tool-bar-position . bottom) default-frame-alist)
(setopt scroll-bar-mode 'right
	horizontal-scroll-bar-mode t
	tool-bar-mode t
	menu-bar-mode nil
	context-menu-mode t)



(provide 'early-init)
;;; early-init.el ends here
