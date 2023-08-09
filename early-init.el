;;; early-init.el --- Emacs early init -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs 27 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens.

;;; Code:



;; PERF: Garbage collection is a big contributor to startup times. This fends it
;;   off, but will be reset later by `gcmh-mode'. Not resetting it later will
;;   cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; Enable gcmh at the end of init.el
(package-activate-all)
(add-hook 'after-init-hook #'gcmh-mode)
(package-activate-all)

;; PERF: Don't use precious startup time checking mtime on elisp bytecode.
;;   Ensuring correctness is 'doom sync's job, not the interactive session's.
;;   Still, stale byte-code will cause *heavy* losses in startup efficiency.
;;(setq load-prefer-newer noninteractive) ; Disabled because i don't need it.

;; UX: Respect DEBUG envvar as an alternative to --debug-init, and to make are
;;   startup sufficiently verbose from this point on.
(when (getenv-internal "DEBUG")
  (setq init-file-debug t
        debug-on-error t))

;;; Disable package at startup
(setq package-enable-at-startup nil)

;; Less clutter
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(left-fringe . 0) default-frame-alist)
(push '(right-fringe . 0) default-frame-alist)
(push '(undecorated . t) default-frame-alist)
(push '(undecorated-round . t) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)
(push '(internal-border-width . 18) default-frame-alist)
(push '(child-frame-border-width . 2) default-frame-alist)

(push '(width . 85) default-frame-alist)
(push '(height . 42) default-frame-alist)
(push '(background-color . "white") default-frame-alist)

(setq mode-line-format nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing the font
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Ignore X resources
(advice-add #'x-apply-session-resources :override #'ignore)

(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (add-to-list 'native-comp-eln-load-path (locate-user-emacs-file "var/eln"))
  (setq native-comp-deferred-compilation nil)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-compile-prune-cache t))

(setq-default frame-title-format '("%b  -  GNU Emacs"))

;; Uncomment this to debug.
;; (setq init-file-debug t)
;; (setq messages-buffer-max-lines 100000)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0)

;; Disabling bidi (bidirectional editing stuff)
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(setq fast-but-imprecise-scrolling t)

;; Data emacs reads from process
;; REVIEW is it necessary?
;;(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; !!!! DOOM !!!!!

(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))
(require 'doom-lib)

;;
;;; Startup optimizations

(unless (daemonp)
  ;; PERF: `file-name-handler-alist' is consulted on each call to `require',
  ;;   `load', or various file/io functions (like `expand-file-name' or
  ;;   `file-remote-p'). You get a noteable boost to startup time by unsetting
  ;;   or simplifying its value.
  (let ((old-value (default-toplevel-value 'file-name-handler-alist)))
    (setq file-name-handler-alist
          ;; HACK: If the bundled elisp for this Emacs install isn't
          ;;   byte-compiled (but is compressed), then leave the gzip file
          ;;   handler there so Emacs won't forget how to read read them.
          ;;
          ;;   calc-loaddefs.el is our heuristic for this because it is built-in
          ;;   to all supported versions of Emacs, and calc.el explicitly loads
          ;;   it uncompiled. This ensures that the only other, possible
          ;;   fallback would be calc-loaddefs.el.gz.
          (if (eval-when-compile
                (locate-file-internal "calc-loaddefs.el" load-path))
              nil
            (list (rassq 'jka-compr-handler old-value))))

    ;; Make sure the new value survives any current let-binding.
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    ;; Remember it so it can be reset where needed.
    (put 'file-name-handler-alist 'initial-value old-value)
    ;; COMPAT: ...but restore `file-name-handler-alist' later, because it is
    ;;   needed for handling encrypted or compressed files, among other things.
    (add-hook! 'emacs-startup-hook :depth 101
      (defun doom--reset-file-handler-alist-h ()
        (setq file-name-handler-alist
              ;; Merge instead of overwrite because there may have been changes to
              ;; `file-name-handler-alist' since startup we want to preserve.
              (delete-dups (append file-name-handler-alist old-value))))))

  (unless noninteractive
    ;; PERF: Resizing the Emacs frame (to accommodate fonts that are smaller or
    ;;   larger than the system font) appears to impact startup time
    ;;   dramatically. The larger the delta in font size, the greater the delay.
    ;;   Even trivial deltas can yield a ~1000ms loss, though it varies wildly
    ;;   depending on font size.
    (setq frame-inhibit-implied-resize t)

    ;; PERF,UX: Reduce *Message* noise at startup. An empty scratch buffer (or
    ;;   the dashboard) is more than enough, and faster to display.
    (setq inhibit-startup-screen t
          inhibit-startup-echo-area-message user-login-name)

    ;; PERF: Shave seconds off startup time by starting the scratch buffer in
    ;;   `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
    ;;   pull in a ton of packages. `doom/open-scratch-buffer' provides a better
    ;;   scratch buffer anyway.
    (setq initial-major-mode 'fundamental-mode
          initial-scratch-message nil)

    (unless initial-window-system
      ;; PERF: Inexplicably, `tty-run-terminal-initialization' can sometimes
      ;;   take 2-3s when starting up Emacs in the terminal. Whatever slows it
      ;;   down at startup doesn't appear to affect it if it's called a little
      ;;   later in the startup process, so that's what I do.
      ;; REVIEW: This optimization is not well understood. Investigate it!
      (define-advice tty-run-terminal-initialization (:override (&rest _) defer)
        (advice-remove #'tty-run-terminal-initialization #'tty-run-terminal-initialization@defer)
        (add-hook 'window-setup-hook
                  (doom-partial #'tty-run-terminal-initialization
                                (selected-frame) nil t))))

    (unless init-file-debug
      ;; PERF,UX: Site files tend to use `load-file', which emits "Loading X..."
      ;;   messages in the echo area. Writing to the echo-area triggers a
      ;;   redisplay, which can be expensive during startup. This may also cause
      ;;   an flash of white when creating the first frame.
      (define-advice load-file (:override (file) silence)
        (load file nil 'nomessage))
      ;; COMPAT: But undo our `load-file' advice later, as to limit the scope of
      ;;   any edge cases it could induce.
      (define-advice startup--load-user-init-file (:before (&rest _) undo-silence)
        (advice-remove #'load-file #'load-file@silence))

      ;; PERF: `load-suffixes' and `load-file-rep-suffixes' are consulted on each
      ;;   `require' and `load'. Doom won't load any dmodules this early, so omit
      ;;   .so for a small startup boost. This is later restored in doom-start.
      (put 'load-suffixes 'initial-value (default-toplevel-value 'load-suffixes))
      (put 'load-file-rep-suffixes 'initial-value (default-toplevel-value 'load-file-rep-suffixes))
      (set-default-toplevel-value 'load-suffixes '(".elc" ".el"))
      (set-default-toplevel-value 'load-file-rep-suffixes '(""))
      ;; COMPAT: Undo any problematic startup optimizations; from this point, I make
      ;;   no assumptions about what might be loaded in userland.
      (add-hook! 'doom-before-init-hook
        (defun doom--reset-load-suffixes-h ()
          (setq load-suffixes (get 'load-suffixes 'initial-value)
                load-file-rep-suffixes (get 'load-file-rep-suffixes 'initial-value))))

      ;; ;; PERF: The mode-line procs a couple dozen times during startup. This is
      ;; ;;   normally quite fast, but disabling the default mode-line and reducing the
      ;; ;;   update delay timer seems to stave off ~30-50ms.
      (put 'mode-line-format 'initial-value (default-toplevel-value 'mode-line-format))
      (setq-default mode-line-format nil)
      (dolist (buf (buffer-list))
        (with-current-buffer buf (setq mode-line-format nil)))

      ;; PERF,UX: Premature redisplays can substantially affect startup times and
      ;;   produce ugly flashes of unstyled Emacs.
      (setq-default inhibit-redisplay t
                    inhibit-message t)

      ;; COMPAT: Then reset it with advice, because `startup--load-user-init-file'
      ;;   will never be interrupted by errors. And if these settings are left
      ;;   set, Emacs could appear frozen or garbled.
      (defun doom--reset-inhibited-vars-h ()
        (setq-default inhibit-redisplay nil
                      ;; Inhibiting `message' only prevents redraws and
                      inhibit-message nil)
        (redraw-frame))
      (add-hook 'after-init-hook #'doom--reset-inhibited-vars-h)
      (define-advice startup--load-user-init-file (:after (&rest _) undo-inhibit-vars)
        (when init-file-had-error
          (doom--reset-inhibited-vars-h))
        (unless (default-toplevel-value 'mode-line-format)
          (setq-default mode-line-format (get 'mode-line-format 'initial-value))))

      ;; PERF: Doom disables the UI elements by default, so that there's less for
      ;;   the frame to initialize. However, the toolbar is still populated
      ;;   regardless, so I lazy load it until tool-bar-mode is actually used.
      (advice-add #'tool-bar-setup :override #'ignore)
      (define-advice startup--load-user-init-file (:before (&rest _) defer-tool-bar-setup)
        (advice-remove #'tool-bar-setup #'ignore)
        (add-transient-hook! 'tool-bar-mode (tool-bar-setup)))

      ;; PERF: Unset a non-trivial list of command line options that aren't
      ;;   relevant to our current OS, but `command-line-1' still processes.
      (unless (eq system-type 'darwin)
        (setq command-line-ns-option-alist nil))
      (unless (memq initial-window-system '(x pgtk))
        (setq command-line-x-option-alist nil))
      )))





(provide 'early-init)
;;; early-init.el ends here
