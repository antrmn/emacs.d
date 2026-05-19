;;; patch-flymake-elisp-packages.el ---  -*- lexical-binding: t; -*-

;;; Commentary:
;;

(defun elisp-flymake-byte-compile-with-packages (report-fn &rest _args)
  "A Flymake backend for elisp byte compilation.
Spawn an Emacs process that byte-compiles a file representing the
current buffer state and calls REPORT-FN when done."
  (unless (trusted-content-p)
    ;; FIXME: Use `bwrap' and friends to compile untrusted content.
    ;; FIXME: We emit a message *and* signal an error, because by default
    ;; Flymake doesn't display the warning it puts into "*flmake log*".
    (message "Disabling elisp-flymake-byte-compile in %s (untrusted content)"
             (buffer-name))
    (user-error "Disabling elisp-flymake-byte-compile in %s (untrusted content)"
                (buffer-name)))
  (when elisp-flymake--byte-compile-process
    (when (process-live-p elisp-flymake--byte-compile-process)
      (kill-process elisp-flymake--byte-compile-process)))
  (let ((temp-file (make-temp-file "elisp-flymake-byte-compile"))
        (source-buffer (current-buffer))
        (coding-system-for-write 'utf-8-unix)
        (coding-system-for-read 'utf-8))
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) temp-file nil 'nomessage))
    (let* ((output-buffer (generate-new-buffer " *elisp-flymake-byte-compile*"))
           ;; Hack: suppress warning about missing lexical cookie in
           ;; *scratch* buffers.
           (warning-suppression-opt
            (and (derived-mode-p 'lisp-interaction-mode)
                 '("--eval"
                   "(setq bytecomp--inhibit-lexical-cookie-warning t)"))))
      (setq
       elisp-flymake--byte-compile-process
       (make-process
        :name "elisp-flymake-byte-compile"
        :buffer output-buffer
        :command `(,(expand-file-name invocation-name invocation-directory)
                   "-Q"
                   "--batch"
                   ;; "--eval" "(setq load-prefer-newer t)" ; for testing
                   ,@(mapcan (lambda (path) (list "-L" path))
                             elisp-flymake-byte-compile-load-path)
                   ,@warning-suppression-opt
                   "-f" "package-initialize"
                   "-f" "elisp-flymake--batch-compile-for-flymake"
                   ,temp-file)
        :connection-type 'pipe
        :sentinel
        (lambda (proc _event)
          (unless (process-live-p proc)
            (unwind-protect
                (cond
                 ((not (and (buffer-live-p source-buffer)
                            (eq proc (with-current-buffer source-buffer
                                       elisp-flymake--byte-compile-process))))
                  (flymake-log :warning
                               "byte-compile process %s obsolete" proc))
                 ((zerop (process-exit-status proc))
                  (elisp-flymake--byte-compile-done report-fn
                                                    source-buffer
                                                    output-buffer))
                 (t
                  (funcall report-fn
                           :panic
                           :explanation
                           (format "byte-compile process %s died" proc))))
              (ignore-errors (delete-file temp-file))
              (kill-buffer output-buffer))))
        :stderr " *stderr of elisp-flymake-byte-compile*"
        :noquery t)))))

(define-minor-mode patch-flymake-elisp-packages-mode
  ""
  :global t
  (if patch-flymake-elisp-packages-mode
      (advice-add 'elisp-flymake-byte-compile :override #'elisp-flymake-byte-compile-with-packages)
    (advice-remove 'elisp-flymake-byte-compile #'elisp-flymake-byte-compile-with-packages)))


(provide 'patch-flymake-elisp-packages)

;;; patch-flymake-elisp-packages.el ends here
