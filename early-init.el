;;; early-init.el --- Emacs early init -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs 27 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens.

;;; Code:


;; Faster to disable these here (before they've been initialized)
(push '(width . 85) default-frame-alist)
(push '(height . 42) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '((left-fringe . 0) (right-fringe . 0)) default-frame-alist)
(push '(background-color . "white") default-frame-alist)
(setq mode-line-format nil)
(eval '(setq inhibit-startup-echo-area-message (user-login-name)))
(setq inhibit-startup-buffer-menu t)
(setq inhbit-startup-screen t)
(setq inhibit-splash-screen t)
(fset 'display-startup-echo-area-message 'ignore)
;(setq inhibit-message t)

(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (add-to-list 'native-comp-eln-load-path (locate-user-emacs-file "var/eln"))
  (setq native-comp-deferred-compilation nil)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-compile-prune-cache t))


(setq package-enable-at-startup nil)


(setq-default frame-title-format '("%b  -  GNU Emacs"))


;; Uncomment this to debug.
;; (setq init-file-debug t)
;; (setq messages-buffer-max-lines 100000)

;; If an `.el' file is newer than its corresponding `.elc', load the `.el'.
(setq load-prefer-newer t)

;; Set Garbage Collection threshold to 1GB during startup. `gcmh' will clean
;; things up later.
;; (setq gc-cons-threshold most-positive-fixnum
;;       gc-cons-percentage 0.6)



;; optimizations (froom Doom's core.el). See that file for descriptions.
(setq idle-update-delay 1.0)

;; Disabling bidi (bidirectional editing stuff)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(setq highlight-nonselected-windows nil)
(setq fast-but-imprecise-scrolling t)
(setq inhibit-compacting-font-caches t)
;; Data emacs reads from process
(setq read-process-output-max (* 1024 1024)) ;; 1mb




;; Resizing the Emacs frame can be a terribly expensive part of changing the font
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Ignore X resources
(advice-add #'x-apply-session-resources :override #'ignore)

;; Unset `file-name-handler-alist' too (temporarily)
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-old)))


(provide 'early-init)
;;; early-init.el ends here
