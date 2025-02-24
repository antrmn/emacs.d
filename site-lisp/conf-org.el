;;; conf-org.el --- Config for Org Mode

;;; Commentary:
;;


(require 'org)
(require 'org-agenda)
(require 'org-capture)

(setopt global-org-modern-mode t ; Enabled in both org and org-agenda buffers
        org-modern-star nil
        org-modern-hide-stars (propertize "*" 'face
                                          '(font-lock-face (:foreground "#ededed"))))

(add-hook! 'org-mode-hook '(olivetti-mode
                            org-special-block-extras-mode
                            org-fragtog-mode
                            org-appear-mode))

(setopt org-download-screenshot-method "spectacle -br -o %s"
        org-mouse-features '(context-menu move-tree yank-link
                                          activate-stars activate-bullets
                                          activate-checkboxes)
        org-num-skip-unnumbered t
        org-num-skip-footnotes t
        org-num-skip-commented t
        org-num-max-level nil
        org-list-allow-alphabetical t
        org-list-indent-offset 2
        org-list-demote-modify-bullet
        '(("+" . "-") ("-" . "+") ("*" . "+"))
        org-crypt-disable-auto-save-data 'ask
        org-use-speed-commands t
        org-inlinetask-show-first-start t
        org-goto-interface 'outline-path-completion
        org-footnote-auto-adjust t
        org-id-link-to-org-use-id nil
        org-attach-store-link-p 'attached
        org-attach-archive-delete nil
        org-attach-directory (expand-file-name "attachments" org-directory)
        org-attach-method 'mv
        org-attach-use-inheritance nil)

(setopt org-directory "~/Org")
(setopt org-agenda-files `(,(expand-file-name "Agendas/" org-directory)))
(setopt org-agenda-text-search-extra-files org-agenda-files)
(setopt org-default-notes-file (expand-file-name "Inbox.org" (car org-agenda-files))
        org-archive-location "./Archive/%s_archive::")


;;;; Org agenda-archives
(setopt org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-todo-ignore-scheduled nil
        org-agenda-todo-ignore-deadlines nil
        org-agenda-todo-ignore-time-comparison-use-seconds t
        org-agenda-todo-ignore-with-date nil
        ;;org-agenda-span 'day
        org-extend-today-until 4
        org-use-effective-time t
        org-stuck-projects '("" ("NEXT") nil "")
        org-agenda-clockreport-parameter-plist '(:link t :maxlevel 5 :fileskip0 t
                                                       :compact t :narrow 80)
        org-agenda-repeating-timestamp-show-all t
        org-agenda-show-all-dates t
        org-agenda-dim-blocked-tasks t
        org-agenda-compact-blocks t
        org-agenda-start-on-weekday 1
        org-agenda-sticky t
        ;;org-agenda-custom-commands
        org-deadline-warning-days 30 ;;agenda AND sparse trees
        org-agenda-persistent-filter t)

(setq org-agenda-skip-scheduled-delay-if-deadline t)
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-window-setup 'only-window)
(setq org-agenda-todo-list-sublevels t)
(setq org-agenda-show-inherited-tags t)
(setq org-agenda-search-headline-for-time nil)
(setq org-agenda-use-time-grid nil)
(setq org-deadline-warning-days 30)
(setq org-agenda-span 'day)
(setq org-agenda-sorting-strategy '((agenda deadline-down time-up habit-up priority-down timestamp-down category-keep)
				    (todo priority-down category-keep)
				    (tags priority-down category-keep)
				    (search category-keep)))
(setq org-agenda-tags-todo-honor-ignore-options t)
(setf org-agenda-sticky t)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-skip-deadline-prewarning-if-scheduled nil)
(setq org-habit-show-habits-only-for-today t)

;;;; Inline tasks
(after! org
  (bind-keys :map org-mode-map
           ("M-n" . outline-next-visibile-heading)
           ("M-p" . outline-previous-visible-heading))
  (require 'org-inlinetask)
  (setq org-inlinetask-default-state "TODO")
  (setq org-inlinetask-min-level 18))

(after! cdlatex
  (unbind-key "_" org-cdlatex-mode-map)
  (unbind-key "^" org-cdlatex-mode-map))

(setopt org-link-elisp-confirm-function nil)


;;;; Org clock
(setopt org-clock-history-length 23
        org-clock-in-resume t
        ;;org-clock-in-switch-to-state
        org-clock-sound "/usr/local/lib/tngchime.wav"
        org-clock-out-remove-zero-time-clocks t
        org-clock-into-drawer t
        org-clock-persist t
        org-clock-persist-query-resume t
        org-clock-persist-query-save t
        org-clock-auto-clock-resolution 'when-no-clock-is-running
        org-clock-report-include-clocking-task t)

;; Org log and notes
(setopt org-log-done 'time
        org-log-reschedule 'time
        org-log-redeadline 'time
        org-log-into-drawer t
        org-log-state-notes-insert-after-drawers nil
        org-reverse-note-order nil)

;; Org id
(setopt org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
        org-id-method 'uuid)

;;;; Org refile
(setopt org-refile-target '((nil :maxlevel . 9)
                            (org-agenda-files :maxlevel . 9))
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm)




(provide 'conf-org)
;;; conf-org.el ends here
