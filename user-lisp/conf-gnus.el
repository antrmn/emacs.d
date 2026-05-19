;; -*- lexical-binding: t; -*-
(require 'gnus)
(require 'secrets)

;; Gnus / auth-source IMAP credentials setup
;;
;; Gnus searches for credentials with these exact params:
;;   host: "posteo" or "posteo.de"
;;   user: "<username>@posteo.com"
;;   port: 993, "imaps", "imap", "993", or "143"
;;
;; Gnome Keyring entries must match one of those combinations.
;; Create them with secret-tool (delete and recreate, no in-place edit):
;;
;;   secret-tool store --label="Gnus IMAP posteo.de" \
;;     host posteo.de \
;;     user ...@posteo.com \
;;     port 993
;;
;;   secret-tool store --label="Gnus IMAP posteo.de imaps" \
;;     host posteo.de \
;;     user ...@posteo.com \
;;     port imaps
;;
;; Verify the entry is found by auth-source:
;;   (auth-source-search :host "posteo.de" :port 993 :user "...@posteo.com")
;; Should return a plist with :secret — if nil, the keyring entry doesn't match.
;;
;; Debug auth-source lookups live with:
;;   (setq auth-source-debug t)
;; then check *Messages* when Gnus prompts for password.

(setq gnus-select-method '(nnimap "posteo"
                                  (nnimap-address "posteo.de")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)
                                  (nnimap-authenticator login)))

(setq gnus-secondary-select-methods
      '((nntp "news.gmane.io"
              (nntp-port-number 119)
              (nntp-stream starttls))
        (nntp "news.eternal-september.org"
              (nntp-port-number 563)
              (nntp-open-connection-function nntp-open-ssl-stream)
              (nntp-authinfo-force t))
        (nntp "nntp.lore.kernel.org"
              (nntp-port-number 119))))

(setq smtpmail-smtp-server "posteo.de"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl
      smtpmail-servers-requiring-authorization "posteo.de")

;; Folders
(setopt gnus-message-archive-group "nnimap+posteo:Sent"
        gnus-draft-group "nnimap+posteo:Drafts"
        gnus-trash-group "nnimap+posteo:Trash")


;; --- Display ---
(setq gnus-sum-thread-tree-enable t
      gnus-sum-thread-tree-root "● "
      gnus-sum-thread-tree-false-root "○ "
      gnus-sum-thread-tree-single-indent "◎ "
      gnus-sum-thread-tree-vertical "│ "
      gnus-sum-thread-tree-indent " "
      gnus-sum-thread-tree-leaf-with-other "├─► "
      gnus-sum-thread-tree-single-leaf "╰─► ")

(setq gnus-summary-line-format "%U%R%z %(%&user-date; %-20,20f%) %B%s\n"
      gnus-user-date-format-alist '((t . "%d %b %H:%M"))
      gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)

;; Show all articles (read and unread) by default for all groups
(setq gnus-parameters
      '((".*"
         (display . all))))

;; --- Threading ---
(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-date)
      gnus-subthread-sort-functions
      '(gnus-thread-sort-by-date)
      gnus-thread-hide-subtree nil
      gnus-fetch-old-headers nil)

;; --- Article ---
(setq gnus-article-date-headers '(combined-lapsed)
      gnus-visible-headers
      '("^From:" "^To:" "^Cc:" "^Date:" "^Subject:")
      gnus-article-browse-delete-temp t
      gnus-inhibit-images nil)

;; --- Groups ---
(setq gnus-group-line-format "%M%S%p%P%5y:%B%G\n"
      gnus-group-sort-function 'gnus-group-sort-by-alphabet
      gnus-show-all-newsgroups nil
      gnus-group-use-permanent-levels t
      gnus-activate-level 2)

;; --- Misc ---
(setq gnus-auto-select-first nil
      gnus-paging-select-next nil
      gnus-mime-display-multipart-related-as-mixed t
      mm-text-html-renderer 'shr
      mm-discouraged-alternatives '("text/html" "text/richtext"))

(provide 'conf-gnus)
