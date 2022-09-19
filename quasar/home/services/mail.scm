(define-module (quasar home services mail)
  #:use-module (quasar home)
  #:use-module (conses home services emacs)
  #:use-module (rde home services mail)
  #:use-module (rde home services emacs-xyz)
  #:use-module (gnu services)
  #:use-module (gnu home services mcron)
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
                (Host ,(getenv "MAIL_PERSONAL_HOST"))
                (User ,(getenv "MAIL_PERSONAL_EMAIL"))
                (Pass ,(getenv "MAIL_PERSONAL_PASSWORD"))
                (SSLType IMAPS)
                (CertificateFile "/etc/ssl/certs/ca-certificates.crt")
                ,#~""
                (IMAPStore personal-remote)
                (Account personal)
                ,#~""
                (MaildirStore personal-local)
                (Subfolders Verbatim)
                (Path "~/.local/share/mail/personal/")
                (Inbox "~/.local/share/mail/personal/Inbox")
                ,#~""
                (Channel personal)
                (Far :personal-remote:)
                (Near :personal-local:)
                (Patterns *)
                (Create Both)
                (SyncState *)))))
   (simple-service 'isync-mcron-job
                   home-mcron-service-type
                   (list #~(job '(next-minute (range 0 60 5))
                                (lambda ()
                                  (system* "mbsync" "--all")))))
   (service home-goimapnotify-service-type
            (home-goimapnotify-configuration
             (config
              `((host . ,(getenv "MAIL_PERSONAL_HOST"))
                (port . 143)
                (tls . #f)
                (tlsOptions . ((rejectUnauthorized . #t)))
                (username . ,(getenv "MAIL_PERSONAL_EMAIL"))
                (password . ,(getenv "MAIL_PERSONAL_PASSWORD"))
                (xoauth2 . #f)
                (onNewMail . "mbsync --all")
                (onNewMailPost . ,(format #f "emacsclient -e '~s'"
                                          '(notifications-notify :app-name "goimapnotify"
                                                                 :title "New email received"
                                                                 :timeout 0)))
                (trigger . 20)
                (boxes . #("Inbox"))))))
   (simple-service
    'home-mail-templates
    home-emacs-tempel-service-type
    `(,#~"text-mode"
      (cut "--8<---------------cut here---------------start------------->8---" n r n
           "--8<---------------cut here---------------end--------------->8---" n)
      (asciibox "+-" (make-string (length str) ?-) "-+" n
                "| " (s str) " |" n
                "+-" (make-string (length str) ?-) "-+" n)))
   (elisp-configuration-service
    `((setq user-full-name ,(getenv "MAIL_PERSONAL_USERNAME"))
      (setq user-mail-address ,(getenv "MAIL_PERSONAL_EMAIL"))
      (define-key mode-specific-map "g" 'gnus)
      (setq mail-user-agent 'gnus-user-agent)
      (with-eval-after-load 'gnus
        (with-eval-after-load 'ebdb
          (require 'ebdb-gnus))
        (custom-set-variables
         '(gnus-use-full-window nil)
         '(gnus-use-cache t)
         '(gnus-novice-user nil)
         '(gnus-interactive-exit nil)
         '(gnus-select-method '(nnnil))
         '(gnus-thread-sort-functions
           '(gnus-thread-sort-by-most-recent-date
             (not gnus-thread-sort-by-number)))
         '(gnus-permanently-visible-groups "^nnmaildir")
         '(gnus-parameters '(("^nnmaildir"
                              (display . 100)
                              (gcc-self . "nnmaildir+personal:Sent"))
                             ("^nntp"
                              (display . 1000))))
         '(gnus-directory "~/.cache/gnus/News")
         '(gnus-home-directory (locate-user-emacs-file "gnus"))
         '(gnus-cache-directory "~/.cache/gnus/News/cache/")
         '(gnus-kill-files-directory "~/.cache/gnus/News/")
         '(gnus-article-save-directory "~/.cache/gnus/News/")
         '(gnus-large-newsgroup 100))
        (setq gnus-posting-styles
              '(("personal"
                 (name ,(getenv "MAIL_PERSONAL_FULLNAME")))
                ((header "to" ".*@lists.sr.ht")
                 (name ,(getenv "MAIL_PERSONAL_USERNAME"))
                 (cc ,(getenv "MAIL_PERSONAL_EMAIL"))
                 (signature ,(string-append "Best regards,\n"
                                            (getenv "MAIL_PERSONAL_USERNAME")))
                 (To (eb-gnus-get-article-participants)))
                ("^nntp.+:"
                 (name ,(getenv "MAIL_PERSONAL_USERNAME"))
                 (cc ,(getenv "MAIL_PERSONAL_EMAIL"))
                 (signature ,(string-append "Best regards,\n"
                                            (getenv "MAIL_PERSONAL_USERNAME")))
                 (To (eb-gnus-get-article-participants)))))
        (setq gnus-secondary-select-methods
              `((nntp "gwene"
                      (nntp-address "news.gwene.org"))
                (nnmaildir "personal"
                           (directory "~/.local/share/mail/personal"))
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
         '(gnus-subscribe-newsgroup-method 'gnus-subscribe-hierarchically)
         '(gnus-save-killed-list nil)))
      ,#~""
      (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
      (add-hook 'gnus-group-mode-hook 'hl-line-mode)
      ,#~""
      (with-eval-after-load 'gnus-sum
        (custom-set-variables
         '(gnus-thread-hide-subtree t)))
      ,#~""
      (add-hook 'gnus-topic-mode-hook 'eb-gnus-topic-mode)
      (with-eval-after-load 'eb-gnus
        (custom-set-variables
         '(eb-gnus-topic-alist '(("personal"
                                  "nnmaildir+personal:Inbox"
                                  "nnmaildir+personal:Drafts"
                                  "nnmaildir+personal:Sent"
                                  "nnmaildir+personal:Junk"
                                  "nnmaildir+personal:Trash")
                                 ("clojure"
                                  "nntp+gwene:gwene.clojure.planet"
                                  "nntp+gwene:gwene.com.google.groups.clojure")
                                 ("lisp"
                                  "nntp+gwene:gwene.org.lisp.planet"
                                  "nntp+gwene:gwene.engineer.atlas.nyxt"
                                  "nntp+gwene:gwene.org.wingolog")
                                 ("technology"
                                  "nntp+gwene:gwene.org.fsf.news"
                                  "nntp+gwene:gwene.rs.lobste"
                                  "nntp+gwene:gwene.org.hnrss.newest.points"
                                  "nntp+gwene:gwene.com.unixsheikh"
                                  "nntp+gwene:gwene.com.drewdevault.blog"
                                  "nntp+gwene:gwene.net.lwn.headlines.newrss"
                                  "nntp+gwene:gwene.com.usesthis"
                                  "nntp+gwene:gwene.org.sourcehut.blog"
                                  "nntp+gwene:gwene.cc.tante"
                                  "nntp+gwene:gwene.org.matrix.blog")
                                 ("emacs"
                                  "nntp+gwene:gmane.emacs.devel"
                                  "nntp+gwene:gmane.emacs.erc.general"
                                  "nntp+gwene:gwene.com.oremacs"
                                  "nntp+gwene:gwene.org.emacslife.planet"
                                  "nntp+gwene:gwene.group.discourse.org-roam.latest")
                                 ("guix"
                                  "nntp+gwene:gmane.comp.gnu.guix.bugs"
                                  "nntp+gwene:gmane.comp.gnu.guix.patches"
                                  "nntp+gwene:gwene.org.gnu.guix.feeds.blog")
                                 ("Gnus")))
         '(eb-gnus-topic-topology '(("Gnus" visible)
                                    (("personal" visible nil))
                                    (("clojure" visible nil))
                                    (("lisp" visible nil))
                                    (("technology" visible nil))
                                    (("emacs" visible nil))
                                    (("guix" visible nil))))))
      (with-eval-after-load 'gnus-topic
        (custom-set-variables
         '(gnus-message-archive-group
           '((".*" "Sent")))
         '(gnus-gcc-mark-as-read t)
         '(gnus-server-alist '(("archive" nnfolder "archive"
                                (nnfolder-directory "~/.local/share/mail/archive")
                                (nnfolder-get-new-mail nil)
                                (nnfolder-inhibit-expiry t)))))
        (setq gnus-message-archive-method `(nnmaildir "personal"))
        (setq gnus-update-message-archive-method t))
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
      (add-hook 'org-mime-html-hook 'eb-org-mime-darken-codeblocks)
      (add-hook 'org-mime-html-hook 'eb-org-mime-indent-quotes)
      (setq org-mime-export-options '(:section-numbers nil :with-author nil :with-toc nil))
      ,#~""
      (add-hook 'message-send-hook 'org-mime-confirm-when-no-multipart)
      (add-hook 'message-mode-hook 'eb-message-mode)
      (with-eval-after-load 'message
        (require 'ebdb)
        (require 'ebdb-message)
        (add-hook 'message-header-setup-hook 'eb-message-add-gcc-header)
        (let ((map message-mode-map))
          (define-key map (kbd "C-c M-z") 'org-mime-htmlize)
          (define-key map (kbd "C-c M-o") 'org-mime-edit-mail-in-org-mode))
        (define-key org-mode-map (kbd "C-c M-o") 'org-mime-org-buffer-htmlize)
        (custom-set-variables
         '(message-signature ,(getenv "MAIL_PERSONAL_FULLNAME"))
         '(message-send-mail-function 'smtpmail-send-it)
         '(message-kill-buffer-on-exit t)))
      ,#~""
      (with-eval-after-load 'sendmail
        (custom-set-variables
         '(send-mail-function 'smtpmail-send-it)))
      ,#~""
      (with-eval-after-load 'smtpmail
        (custom-set-variables
         '(smtpmail-smtp-user ,(getenv "MAIL_PERSONAL_EMAIL"))
         '(smtpmail-smtp-service 587)
         '(smtpmail-stream-type 'starttls)
         '(smtpmail-queue-dir "~/.cache/gnus/Mail/queued-mail")
         '(smtpmail-debug-info t))
        (setq smtpmail-smtp-server ,(getenv "MAIL_PERSONAL_HOST")
              smtpmail-default-smtp-server ,(getenv "MAIL_PERSONAL_HOST"))))
    #:elisp-packages (list emacs-ebdb
                           emacs-debbugs))))
