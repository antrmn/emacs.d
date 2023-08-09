;;; conf-org.el --- Config for Org Mode

;;; Commentary:
;;



(setopt global-org-modern-mode t ; Enabled in both org and org-agenda buffers
        org-modern-star nil
        org-modern-hide-stars (propertize "*" 'face
                                          '(font-lock-face (:foreground "#ededed"))))

(add-hook! 'org-mode-hook '(olivetti-mode
                            org-special-block-extras-mode
                            org-fragtog-mode
                            org-appear-mode))

(setopt org-download-screenshot-method "spectacle -br -o %s"
        org-timer-display 'both
        org-table-header-line-p t
        org-edit-src-persistent-message t
        org-mouse-features '(context-menu move-tree yank-link
                                          activate-stars activate-bullets
                                          activate-checkboxes)
        org-num-skip-unnumbered t
        org-num-skip-tags t
        org-num-skip-footnotes t
        org-num-skip-commented t
        org-num-max-level nil
        org-list-allow-alphabetical t
        org-checkbox-hierarchical-statistics nil ;;Global COOKIE DATA recursive
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

(with-eval-after-load 'org-agenda
  (org-super-agenda-mode))


(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-compact-blocks t)

(setopt org-directory "~/Nextcloud/Org"
        org-agenda-files '("~/Nextcloud/Org/inbox.org")
        org-archive-location "%s_archive::"
        org-agenda-include-diary nil
        org-agenda-diary-filel "~/Nextcloud/Org/diary.org"
        ;;org-agenda-text-search-extra-files
        org-default-notes-file "~/Nextcloud/inbox.org")

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

(setq org-agenda-custom-commands
      '(("o" "Overview"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                          :time-grid t
                          :date today
                          :todo "TODAY"
                          :scheduled today
                          :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Next to do"
                           :todo "NEXT"
                           :order 1)
                          (:name "Important"
                           :tag "Important"
                           :priority "A"
                           :order 6)
                          (:name "Due Today"
                           :deadline today
                           :order 2)
                          (:name "Due Soon"
                           :deadline future
                           :order 8)
                          (:name "Overdue"
                           :deadline past
                           :face error
                           :order 7)
                          (:name "Assignments"
                           :tag "Assignment"
                           :order 10)
                          (:name "Issues"
                           :tag "Issue"
                           :order 12)
                          (:name "Emacs"
                           :tag "Emacs"
                           :order 13)
                          (:name "Projects"
                           :tag "Project"
                           :order 14)
                          (:name "Research"
                           :tag "Research"
                           :order 15)
                          (:name "To read"
                           :tag "Read"
                           :order 30)
                          (:name "Waiting"
                           :todo "WAITING"
                           :order 20)
                          (:name "University"
                           :tag "uni"
                           :order 32)
                          (:name "Trivial"
                           :priority<= "E"
                           :tag ("Trivial" "Unimportant")
                           :todo ("SOMEDAY" )
                           :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))

(setq +org-capture-todo-file "~/Nextcloud/Org/inbox.org")
(defun set-org-capture-templates ()
    (setq org-capture-templates
          (doct `(("Personal todo" :keys "t"
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %?"
                              "%i %a"))
                  ("Personal note" :keys "n"
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* %?"
                              "%i %a"))
                  ("Email" :keys "e"
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %^{type|reply to|contact} %\\3 %? :email:"
                              "Send an email %^{urgancy|soon|ASAP|anon|at some point|eventually} to %^{recipiant}"
                              "about %^{topic}"
                              "%U %i %a"))
                  ("Interesting" :keys "i"
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Interesting"
                   :type entry
                   :template ("* [ ] %{desc}%? :%{i-type}:"
                              "%i %a")
                   :children (("Webpage" :keys "w"
                               :desc "%(org-cliplink-capture) "
                               :i-type "read:web")
                              ("Article" :keys "a"
                               :desc ""
                               :i-type "read:reaserch")
                              ("Information" :keys "i"
                               :desc ""
                               :i-type "read:info")
                              ("Idea" :keys "I"
                               :desc ""
                               :i-type "idea")))
                  ("Tasks" :keys "k"
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Tasks"
                   :type entry
                   :template ("* TODO %? %^G%{extra}"
                              "%i %a")
                   :children (("General Task" :keys "k"
                               :extra "")
                              ("Task with deadline" :keys "d"
                               :extra "\nDEADLINE: %^{Deadline:}t")
                              ("Scheduled Task" :keys "s"
                               :extra "\nSCHEDULED: %^{Start time:}t")))
                  ("Project" :keys "p"
                   :prepend t
                   :type entry
                   :headline "Inbox"
                   :template ("* %{time-or-todo} %?"
                              "%i"
                              "%a")
                   :file ""
                   :custom (:time-or-todo "")
                   :children (("Project-local todo" :keys "t"
                               :time-or-todo "TODO"
                               :file +org-capture-todo-file)))))))

(set-org-capture-templates)



(provide 'conf-org)

;;; conf-org.el ends here
