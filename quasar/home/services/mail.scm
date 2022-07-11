(define-module (quasar home services mail)
  #:use-module (quasar home)
  #:use-module (conses home services emacs)
  #:use-module (gnu services)
  #:use-module (gnu home-services mail)
  #:use-module (gnu packages mail)
  #:use-module (guix gexp)
  #:use-module (gnu packages emacs-xyz)
  #:export (mail-service))

(define (mail-service)
  (list
   (service home-isync-service-type
            (home-isync-configuration
             (xdg-flavor? #t)
             (config
              `((IMAPAccount personal)
                (Host mail.gandi.net)
                (User ,(password-store-get "mail/personal/username"))
                (PassCmd pass mail/mail.gandi.net)
                (SSLType IMAPS)
                (CertificateFile "/etc/ssl/certs/ca-certificates.crt")
                ,#~""
                (IMAPStore personal-remote)
                (Account personal)
                ,#~""
                (MaildirStore personal-local)
                (Subfolders Verbatim)
                (Path "~/.local/share/mail/personal")
                (Inbox "~/.local/share/mail/personal/Inbox")
                ,#~""
                (Channel personal)
                (Far "personal-remote")
                (Near ":personal-local")
                (Patterns *)
                (Create Both)
                (SyncState *)))))
   (elisp-configuration-service
    `((define-key mode-specific-map "g" 'gnus)
      (setq user-full-name (password-store-get-field "mail/mail.gandi.net" "username")
            user-mail-address (password-store-get-field "mail/mail.gandi.net" "email"))
      (with-eval-after-load 'gnus
        (with-eval-after-load 'ebdb
          (require 'ebdb-gnus))
        (custom-set-variables
         '(gnus-use-full-window nil)
         '(gnus-use-cache t)
         '(gnus-select-method '(nnnil))
         '(gnus-thread-sort-functions
           '(gnus-thread-sort-by-most-recent-date
             (not gnus-thread-sort-by-number)))
         '(gnus-parameters '())
         '(gnus-posting-styles `(("personal"
                                  (name ,(password-store-get-field "mail/mail.gandi.net" "full-name")))))
         '(gnus-directory "~/.cache/gnus/News")
         '(gnus-home-directory (locate-user-emacs-file "gnus"))
         '(gnus-cache-directory "~/.cache/gnus/News/cache/")
         '(gnus-kill-files-directory "~/.cache/gnus/News/")
         '(gnus-article-save-directory "~/.cache/gnus/News/")
         '(gnus-large-newsgroup 100))
        (setq gnus-secondary-select-methods
              `((nntp "gwene"
                      (nntp-address "news.gwene.org"))
                (nnimap "personal"
                        (nnimap-address ,(password-store-get-field "mail/mail.gandi.net" "host"))
                        (nnimap-server-port 993)
                        (nnir-search-engine imap)
                        (nnimap-stream ssl))
                (nnfolder "archive"
                          (nnfolder-directory "~/.local/share/mail/archive")
                          (nnfolder-active-file "~/.local/share/mail/archive/active")
                          (nnfolder-get-new-mail nil)
                          (nnfolder-inhibit-expiry t)))))
      ,#~""
      (with-eval-after-load 'mail-source
        (custom-set-variables
         '(mail-source-directory "~/.cache/gnus/Mail")
         '(mail-default-directory "~/.cache/gnus")))
      ,#~""
      (with-eval-after-load 'gnus-start
        (custom-set-variables
         '(gnus-dribble-directory (locate-user-emacs-file "gnus"))
         '(gnus-startup-file (locate-user-emacs-file "gnus/.newsrc"))
         '(gnus-check-new-newsgroups nil)
         '(gnus-subscribe-newsgroup-method 'gnus-subscribe-interactively)
         '(gnus-save-killed-list nil)))
      ,#~""
      (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
      (with-eval-after-load 'gnus-group
        (define-key gnus-group-mode-map "o" 'eb-mail-group-list-subscribed-groups)
        (custom-set-variables
         '(gnus-topic-alist '(("personal"
                               "nnimap+personal:INBOX"
                               "nnimap+personal:Drafts"
                               "nnimap+personal:Sent"
                               "nnimap+personal:Junk"
                               "nnimap+personal:Deleted")
                              ("clojure"
                               "nntp+gwene:gwene.clojure.planet")
                              ("lisp"
                               "nntp+gwene:gwene.org.lisp.planet"
                               "nntp+gwene:gwene.engineer.atlas.nyxt")
                              ("technology"
                               "nntp+gwene:gwene.org.fsf.news")
                              ("emacs"
                               "nntp+gwene:gmane.emacs.devel"
                               "nntp+gwene:gmane.emacs.erc.general")
                              ("guix"
                               "nntp+gwene:gmane.comp.gnu.guix.bugs"
                               "nntp+gwene:gmane.comp.gnu.guix.patches"
                               "nntp+gwene:gwene.org.gnu.guix.feeds.blog")
                              ("misc"
                               "nnfolder+archive:sent.2022"
                               "nndraft:drafts")
                              ("Gnus")))))
      ,#~""
      (with-eval-after-load 'gnus-sum
        (custom-set-variables
         '(gnus-thread-hide-subtree t)))
      ,#~""
      (with-eval-after-load 'gnus-topic
        (custom-set-variables
         '(gnus-message-archive-group
           '((".*" "Sent")))
         '(gnus-gcc-mark-as-read t)
         '(gnus-server-alist '(("archive" nnfolder "archive"
                                (nnfolder-directory "~/.local/share/mail/archive")
                                (nnfolder-get-new-mail nil)
                                (nnfolder-inhibit-expiry t))))
         '(gnus-topic-topology '(("Gnus" visible)
                                 (("personal" visible))
                                 (("misc" visible))
                                 (("clojure" visible))
                                 (("lisp" visible))
                                 (("technology" visible))
                                 (("emacs" visible))
                                 (("guix" visible)))))
        (setq gnus-message-archive-method
              `(nnimap ,(password-store-get-field "mail/mail.gandi.net" "host"))))
      ,#~"
(with-eval-after-load 'gnus-art
  (define-key gnus-article-mode-map [remap shr-mouse-browse-url] #'shr-mouse-browse-url-new-window)
  (define-key gnus-article-mode-map [remap shr-browse-url] #'eb-web-shr-browse-url-new-window))
"
      ,#~""
      (let ((map mode-specific-map))
        (define-key map "ba" 'ebdb-display-all-records)
        (define-key map "bi" 'ebdb-create-record-extended))
      (with-eval-after-load 'ebdb
        (require 'ebdb-i18n)
        (require 'ebdb-vcard)
        (require 'ebdb-org)
        (custom-set-variables
         '(ebdb-sources `(,(expand-file-name "contacts" (xdg-user-dir "DOCUMENTS"))))
         '(ebdb-default-country nil)
         '(ebdb-default-window-size 0.2)
         '(ebdb-dedicated-window 'ebdb)
         '(ebdb-mail-avoid-redundancy t)
         '(ebdb-complete-mail 'capf)
         '(ebdb-completion-display-record nil)
         '(ebdb-complete-mail-allow-cycling nil)
         '(ebdb-save-on-exit t))
        (define-key ebdb-mode-map "q" 'kill-this-buffer))
      ,#~""
      (with-eval-after-load 'ebdb-mua
        (custom-set-variables
         '(ebdb-mua-pop-up nil)))
      ,#~""
      (require 'org-mime)
      (add-hook 'org-mime-html-hook 'eb-mail-org-mime-darken-codeblocks)
      (add-hook 'org-mime-html-hook 'eb-mail-org-mime-indent-quotes)
      (setq org-mime-export-options '(:section-numbers nil :with-author nil :with-toc nil))
      ,#~""
      (add-hook 'message-send-hook 'org-mime-confirm-when-no-multipart)
      (add-hook 'message-mode-hook 'eb-mail-message-mode)
      (with-eval-after-load 'message
        (require 'ebdb-message)
        (let ((map message-mode-map))
          (define-key map (kbd "C-c M-z") 'org-mime-htmlize)
          (define-key map (kbd "C-c M-o") 'org-mime-edit-mail-in-org-mode))
        (define-key org-mode-map (kbd "C-c M-o") 'org-mime-org-buffer-htmlize)
        (custom-set-variables
         '(message-send-mail-function 'smtpmail-send-it)
         '(message-kill-buffer-on-exit t)))
      ,#~""
      (with-eval-after-load 'sendmail
        (custom-set-variables
         '(send-mail-function 'smtpmail-send-it)))
      ,#~""
      (with-eval-after-load 'smtpmail
        (custom-set-variables
         '(smtpmail-smtp-service 587)
         '(smtpmail-stream-type 'starttls)
         '(smtpmail-queue-dir "~/.cache/gnus/Mail/queued-mail"))
        (setq smtpmail-smtp-server (password-store-get-field "mail/mail.gandi.net" "host")
              smtpmail-default-smtp-server (password-store-get-field "mail/mail.gandi.net" "host"))))
    #:elisp-packages (list emacs-ebdb
                           emacs-debbugs))))
