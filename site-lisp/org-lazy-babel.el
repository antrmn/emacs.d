(defvar +org-babel-mode-alist
  '((cpp . C)
    (C++ . C)
    (D . C)
    (sh . shell)
    (bash . shell)
    (matlab . octave)
    (amm . ammonite))
  "An alist mapping languages to babel libraries. This is necessary for babel
libraries (ob-*.el) that don't match the name of the language.

For example, with (fish . shell) will cause #+BEGIN_SRC fish to load ob-shell.el
when executed.")

(defvar +org-babel-load-functions ()
  "A list of functions executed to load the current executing src block. They
take one argument (the language specified in the src block, as a string). Stops
at the first function to return non-nil.")

(defun +org--babel-lazy-load (lang)
  (cl-check-type lang symbol)
  (or (run-hook-with-args-until-success '+org-babel-load-functions lang)
      (require (intern (format "ob-%s" lang)) nil t)
      (require lang nil t)))

(defun +org--src-lazy-load-library-a (lang)
  "Lazy load a babel package to ensure syntax highlighting."
  (or (cdr (assoc lang org-src-lang-modes))
      (+org--babel-lazy-load lang)))
(advice-add #'org-src--get-lang-mode :before #'+org--src-lazy-load-library-a)

(defun +org--babel-lazy-load-library-a (info)
  "Load babel libraries lazily when babel blocks are executed."
  (let* ((lang (nth 0 info))
         (lang (cond ((symbolp lang) lang)
                     ((stringp lang) (intern lang))))
         (lang (or (cdr (assq lang +org-babel-mode-alist))
                   lang)))
    (when (and lang
               (not (cdr (assq lang org-babel-load-languages)))
               (+org--babel-lazy-load lang))
      (when (assq :async (nth 2 info))
        ;; ob-async has its own agenda for lazy loading packages (in the
        ;; child process), so we only need to make sure it's loaded.
        (require 'ob-async nil t))
      (add-to-list 'org-babel-load-languages (cons lang t)))
    t))
(advice-add #'org-babel-confirm-evaluate :after-while #'+org--babel-lazy-load-library-a)
(provide 'org-lazy-babel)
