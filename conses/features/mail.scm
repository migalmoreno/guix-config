(define-module (conses features mail)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features emacs)
  #:use-module (rde features mail)
  #:use-module (rde features predicates)
  #:use-module (rde home services mail)
  #:use-module (rde home services emacs-xyz)
  #:use-module (gnu services)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home-services mail)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:export (mail-acc
            mail-lst
            mail-directory-fn
            feature-goimapnotify
            feature-emacs-ebdb
            feature-emacs-gnus
            feature-emacs-message
            feature-emacs-smtpmail
            feature-emacs-org-mime
            feature-emacs-debbugs))

(define* (mail-acc id user type #:optional pass-cmd)
  "Make a simple mail account."
  (mail-account
   (id id)
   (fqda user)
   (type type)
   (pass-cmd (format #f "pass show mail/~a | head -1" id))))

(define (mail-lst id fqda urls)
  "Make a simple mailing list."
  (mailing-list
   (id id)
   (fqda fqda)
   (config (l2md-repo
            (name (symbol->string id))
            (urls urls)))))

(define (mail-directory-fn config)
  (string-append (getenv "XDG_STATE_HOME") "/mail"))


;;;
;;; feature-goimapnotify
;;;

(define* (feature-goimapnotify
          #:key
          (mail-account-ids #f)
          (goimapnotify go-gitlab.com-shackra-goimapnotify))
  "Set up and configure goimapnotify. If MAIL-ACCOUNT-IDS
is not provided, use all the mail accounts."
  (ensure-pred maybe-list? mail-account-ids)
  (ensure-pred any-package? goimapnotify)

  (define (get-home-services config)
    "Return home services related to goimapnotify."
    (require-value 'mail-accounts config)

    (define mail-accounts
      (if mail-account-ids
          (filter (lambda (x)
                    (member (mail-account-id x) mail-account-ids))
                  (get-value 'mail-accounts config))
          (get-value 'mail-accounts config)))

    (list
     (service
      home-goimapnotify-service-type
      (home-goimapnotify-configuration
       (goimapnotify goimapnotify)
       (config
        `#(,@(map
              (lambda (mail-acc)
                `((host . ,(assoc-ref (assoc-ref %default-msmtp-provider-settings
                                                 (mail-account-type mail-acc))
                                      'host))
                  (port . 143)
                  (tls . #f)
                  (tlsOptions . ((rejectUnauthorized . #t)))
                  (username . ,(mail-account-fqda mail-acc))
                  (password . ,(getenv "MAIL_PERSONAL_PASSWORD"))
                  (xoauth2 . #f)
                  (alias . ,(mail-account-id mail-acc))
                  (trigger . 20)
                  (boxes . #(((mailbox . "Inbox")
                              (onNewMail . ,(format #f "mbsync ~a" (mail-account-id mail-acc)))
                              (onNewMailPost . ,(format #f "emacsclient -e '~s'"
                                                        '(notifications-notify
                                                          :app-name "goimapnotify"
                                                          :title "New email received"
                                                          :timeout 5000))))))))
              mail-accounts)))))))

  (feature
   (name 'goimapnotify)
   (values `((goimapnotify . ,goimapnotify)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-ebdb
          #:key
          (emacs-ebdb emacs-ebdb)
          (ebdb-sources (list "~/documents/contacts"))
          (ebdb-popup-size 0.4)
          (ebdb-key "b"))
  "Configure the ebdb contact management package for Emacs.
EBDB-SOURCES is a list of filenames to retrieve database
information from.
You can control the size of ebdb popup windows via EBDB-POPUP-SIZE
with a floating-point value between 0 and 1."
  (ensure-pred file-like? emacs-ebdb)
  (ensure-pred list-of-strings? ebdb-sources)
  (ensure-pred number? ebdb-popup-size)
  (ensure-pred string? ebdb-key)

  (define emacs-f-name 'ebdb)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to EBDB."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((defvar rde-ebdb-map nil
          "Map to bind EBDB commands under.")
        (define-prefix-command 'rde-ebdb-map)
        (with-eval-after-load 'rde-keymaps
          (define-key rde-app-map (kbd ,ebdb-key) 'rde-ebdb-map)
          (let ((map rde-ebdb-map))
            (define-key map "a" 'ebdb-display-all-records)
            (define-key map "c" 'ebdb-create-record-extended)))
        (with-eval-after-load 'ebdb
          (require 'ebdb-i18n)
          (require 'ebdb-vcard)
          ,@(if (get-value 'emacs-org config)
                '((require 'ebdb-org))
                '())
          ,@(if (get-value 'mail-accounts config)
                '((require 'ebdb-mua))
                '())
          (setq ebdb-sources (list ,@ebdb-sources))
          (setq ebdb-default-country nil)
          (setq ebdb-default-window-size ,ebdb-popup-size)
          (setq ebdb-dedicated-window 'ebdb)
          (setq ebdb-mail-avoid-redundancy t)
          (setq ebdb-complete-mail 'capf)
          (setq ebdb-completion-display-record nil)
          (setq ebdb-complete-mail-allow-cycling nil)
          (setq ebdb-save-on-exit t)
          (define-key ebdb-mode-map "q" 'kill-this-buffer))
        (with-eval-after-load 'ebdb-mua
          (setq ebdb-mua-pop-up nil)))
      #:elisp-packages (list emacs-ebdb))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-ebdb)))
   (home-services-getter get-home-services)))


;;;
;;; feature-emacs-message
;;;

(define (default-message-signature config)
  (format #f "Best regards,\n~a" (get-value 'full-name config)))

(define-public (string-or-boolean-or-procedure? x)
  (or (string? x) (boolean? x) (procedure? x)))

(define* (feature-emacs-message
          #:key
          (message-signature default-message-signature))
  "Configure Message, the mail composition mode for Emacs."
  (ensure-pred string-or-boolean-or-procedure? message-signature)

  (define emacs-f-name 'message)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Message."
    (require-value 'full-name config)

    (list
     (emacs-xdg-service
      emacs-f-name
      "Emacs (Client) [mailto:]"
      #~(system*
         #$(file-append (get-value 'emacs config) "/bin/emacsclient") "--eval"
         (string-append
          "\
(progn
 (browse-url-mail \"" (cadr (command-line)) "\"))"))
      #:default-for '(x-scheme-handler/mailto))
     (simple-service
      'home-message-emacs-tempel-service
      home-emacs-tempel-service-type
      `(,#~"text-mode"
        (cut "--8<---------------cut here---------------start------------->8---" n r n
             "--8<---------------cut here---------------end--------------->8---" n)
        (asciibox "+-" (make-string (length str) ?-) "-+" n
                  "| " (s str) " |" n
                  "+-" (make-string (length str) ?-) "-+" n)))
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((defgroup rde-message nil
          "Message mode tweaks."
          :group 'rde)
        (defun rde-message-add-gcc-header ()
          "Prompt for a Gcc header from `rde-gnus-topic-alist'.
This will allow a message to be stored in the right directory
of the IMAP server (usually \"Sent\").
If this header is missing, the outgoing message will go through,
but it won't appear on the right Maildir directory."
          (if (gnus-alive-p)
              (unless (message-fetch-field "Gcc")
                (message-add-header (format "Gcc: %s"
                                            (let ((groups
                                                   (cl-remove-if-not
                                                    (lambda (group)
                                                      (string-match (rx "Sent" eol) group))
                                                    (rde-gnus--get-topic-groups))))
                                              (if (> 1 (length groups))
                                                  (completing-read "Account: " groups)
                                                (car groups))))))
            (error "Gnus is not running.  No GCC header will be inserted")))

        (define-minor-mode rde-message-mode
          "Tweak the appearance of `message-mode' buffers."
          :group 'rde-message
          (if rde-message-mode
              (display-line-numbers-mode -1)
            (display-line-numbers-mode 1)))
        ,@(if (get-value 'emacs-org-mime config)
              `((add-hook 'message-send-hook 'org-mime-confirm-when-no-multipart))
              '())

        (add-hook 'message-mode-hook 'rde-message-mode)
        (with-eval-after-load 'message
          ,@(if (get-value 'emacs-ebdb config)
                `((require 'ebdb-message))
              '())
          (add-hook 'message-header-setup-hook 'rde-message-add-gcc-header)
          (setq message-kill-buffer-on-exit t)
          (setq message-signature
                ,(match message-signature
                   ((? procedure? e) (e config))
                   ((? string? e) e)
                   (#f 'nil)
                   (_ 't)))
          ,@(cond
             ((get-value 'msmtp config)
              `((setq sendmail-program ,(file-append (get-value 'msmtp config) "/bin/msmtp"))
                (setq message-send-mail-function 'message-send-mail-with-sendmail)
                (setq message-sendmail-f-is-evil t)
                (setq message-sendmail-extra-arguments '("--read-envelope-from"))))
             ((get-value 'emacs-smtpmail config)
              `((setq message-send-mail-function 'smtpmail-send-it)))
             (#t '())))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             (message-signature . ,message-signature)))
   (home-services-getter get-home-services)))


;;;
;;; feature-emacs-org-mime
;;;

(define* (feature-emacs-org-mime
          #:key
          (emacs-org-mime emacs-org-mime))
  "Configure org-mime, an Org HTML export for text/html MIME emails."

  (define emacs-f-name 'org-mime)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to smtpmail.el."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'org-mime)
        (defgroup rde-org-mime nil
          "Minor tweaks for Org Mime."
          :group 'rde)
        (defun rde-org-mime-darken-codeblocks ()
          "Apply a dark background to HTML email body codeblocks."
          (org-mime-change-element-style
           "pre"
           "color: #E6E1Dc; background-color: #232323; padding: 0.5em;"))

        (defun rde-org-mime-indent-quotes ()
          "Add padding to block quotes in HTML email body."
          (org-mime-change-element-style
           "blockquote"
           "border-left: 2px solid gray; padding-left: 4px;"))

        (define-key org-mode-map (kbd "C-c M-o") 'org-mime-org-buffer-htmlize)
        (add-hook 'org-mime-html-hook 'rde-org-mime-darken-codeblocks)
        (add-hook 'org-mime-html-hook 'rde-org-mime-indent-quotes)
        (let ((map message-mode-map))
          (define-key map (kbd "C-c M-z") 'org-mime-htmlize)
          (define-key map (kbd "C-c M-o") 'org-mime-edit-mail-in-org-mode))
        (setq org-mime-export-options '(:section-numbers nil :with-author nil :with-toc nil)))
      #:elisp-packages (list emacs-org-mime))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-org-mime)))
   (home-services-getter get-home-services)))


;;;
;;; feature-emacs-smtpmail
;;;

(define* (feature-emacs-smtpmail
          #:key smtp-server smtp-user)
  "Configure smtpmail, a simple mail protocol for sending mail from Emacs."

  (define emacs-f-name 'smtpmail)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to smtpmail.el."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load 'smtpmail
          (setq smtpmail-smtp-user ,smtp-user)
          (setq smtpmail-smtp-service 587)
          (setq smtpmail-stream-type 'starttls)
          (setq smtpmail-queue-dir "~/.cache/gnus/Mail/queued-mail")
          (setq smtpmail-debug-info t)
          (setq smtpmail-smtp-server ,smtp-server)
          (setq smtpmail-default-smtp-server ,smtp-server))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


;;;
;;; feature-emacs-gnus
;;;

(define* (feature-emacs-gnus
          #:key
          (posting-styles #f)
          (group-parameters #f)
          (gnus-directory "~/.cache/gnus/News")
          (topic-alist #f)
          (topic-topology #f)
          (mail-account-ids #f)
          (message-archive-method #f)
          (message-archive-group #f))
  "Configure the Gnus newsreader.
If MAIL-ACCOUNT-IDS is not provided, use all the mail accounts."
  (ensure-pred maybe-list? posting-styles)
  (ensure-pred path? gnus-directory)
  (ensure-pred maybe-list? group-parameters)
  (ensure-pred maybe-list? topic-alist)
  (ensure-pred maybe-list? topic-topology)
  (ensure-pred maybe-list? mail-account-ids)
  (ensure-pred maybe-list? message-archive-method)
  (ensure-pred maybe-list? message-archive-group)

  (define emacs-f-name 'gnus)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Gnus."
    (require-value 'mail-accounts config)

    (define mail-accounts
      (if mail-account-ids
          (filter (lambda (x)
                    (member (mail-account-id x) mail-account-ids))
                  (get-value 'mail-accounts config))
          (get-value 'mail-accounts config)))
    (define mail-dir ((get-value 'mail-directory-fn config) config))

    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((defgroup rde-gnus nil
          "Customizations for the Gnus newsreader."
          :group 'rde)
        (defcustom rde-gnus-topic-topology nil
          "Topics topology for Gnus."
          :group 'rde-gnus
          :type 'list)
        (defcustom rde-gnus-topic-alist nil
          "Alist of Gnus topics."
          :group 'rde-gnus
          :type 'list)
        (defvar rde-gnus-subscribed-p nil
          "Whether we're currently subscribed to Gnus groups.")
        (defun rde-gnus--get-topic-groups ()
          "Return a flattened list of groups from `rde-gnus-topic-alist'."
          (flatten-list (mapcar (lambda (topic)
                                  (cdr topic))
                                rde-gnus-topic-alist)))

        (defun rde-gnus-get-article-participants ()
          "Retrieve the participants from the current article."
          (if (and (gnus-alive-p)
                   (message-fetch-field "from")
                   (message-fetch-field "to"))
              (with-current-buffer gnus-article-buffer
                (string-join
                 (remove-if
                  (lambda (address)
                    (string-match user-mail-address address))
                  (append
                   (split-string (message-fetch-field "from") ", ")
                   (split-string (message-fetch-field "to") ", ")))
                 ", "))
            ""))

        (defun rde-gnus-shr-browse-url-new-window ()
          "When using shr, open links in a new window."
          (interactive)
          (shr-browse-url nil nil t))

        (define-minor-mode rde-gnus-topic-mode
          "Apply Gnus topic settings declaratively and subscribe to groups."
          :group 'rde-gnus
          (setq gnus-topic-topology rde-gnus-topic-topology)
          (setq gnus-topic-alist rde-gnus-topic-alist)
          (unless rde-gnus-subscribed-p
            (mapc (lambda (topic)
                    (gnus-subscribe-hierarchically topic))
                  (rde-gnus--get-topic-groups)))
          (setq rde-gnus-subscribed-p t))

        (setq rde-gnus-topic-alist ',topic-alist)
        (setq rde-gnus-topic-topology ',topic-topology)
        (with-eval-after-load 'rde-keymaps
          (define-key rde-app-map "g" 'gnus))
        (setq mail-user-agent 'gnus-user-agent)
        (with-eval-after-load 'gnus
          ,@(if (get-value 'emacs-ebdb config)
                '((with-eval-after-load 'ebdb
                    (require 'ebdb-gnus)))
              '())
          (setq gnus-use-full-window nil)
          (setq gnus-use-cache t)
          ,@(if (get-value 'emacs-advanced-user? config)
                `((setq gnus-novice-user nil))
              '())
          (setq gnus-interactive-exit nil)
          (setq gnus-thread-sort-functions '(gnus-thread-sort-by-most-recent-date
                                             (not gnus-thread-sort-by-number)))
          (setq gnus-permanently-visible-groups "^nnmaildir")
          (setq gnus-parameters ',group-parameters)
          (setq gnus-directory ,gnus-directory)
          (setq gnus-home-directory (locate-user-emacs-file "gnus"))
          (setq gnus-cache-directory "~/.cache/gnus/News/cache/")
          (setq gnus-kill-files-directory "~/.cache/gnus/News/")
          (setq gnus-article-save-directory "~/.cache/gnus/News/")
          (setq gnus-large-newsgroup 100)
          ,@(if message-archive-method
                `((setq gnus-message-archive-method ',message-archive-method))
                '())
          ,@(if message-archive-group
                `((setq gnus-message-archive-group ',message-archive-group))
                '())
          (setq gnus-update-message-archive-method t)
          (setq gnus-posting-styles
                '(,@(if (get-value 'msmtp config)
                        '()
                      (map (lambda (mail-acc)
                             `(,(symbol->string (mail-account-id mail-acc))
                               (name ,(get-value 'full-name config))
                               (signature ,(match (get-value 'message-signature config)
                                             ((? procedure? e) (e config))
                                             ((? string? e) e)
                                             (#f 'nil)
                                             (_ 't)))
                               ("X-Message-SMTP-Method"
                                ,(format #f "smtp ~a ~a ~a"
                                         (assoc-ref (assoc-ref %default-msmtp-provider-settings
                                                               (mail-account-type mail-acc))
                                                    'host)
                                         (assoc-ref (assoc-ref %default-msmtp-provider-settings
                                                               (mail-account-type mail-acc))
                                                    'port)
                                         (mail-account-fqda mail-acc)))))
                           mail-accounts))
                  ,@posting-styles))
          (setq gnus-select-method '(nnnil))
          (setq gnus-secondary-select-methods
                '(,@(if (get-value 'isync config)
                        (map (lambda (mail-acc)
                               `(nnmaildir
                                 ,(symbol->string (mail-account-id mail-acc))
                                 (directory ,(string-append mail-dir "/accounts/"
                                                            (mail-account-fqda mail-acc)))))
                             mail-accounts)
                      '())
                  (nntp "gwene"
                        (nntp-address "news.gwene.org"))
                  (nnfolder "archive"
                            (nnfolder-directory ,(string-append mail-dir "/archive"))
                            (nnfolder-active-file ,(string-append mail-dir "/archive/active"))
                            (nnfolder-get-new-mail nil)
                            (nnfolder-inhibit-expiry t)))))
        (with-eval-after-load 'mail-source
          (setq mail-source-directory (expand-file-name "emacs/gnus/mail" (or (xdg-cache-home) "~/.cache")))
          (setq mail-default-directory (expand-file-name "emacs/gnus" (or (xdg-cache-home) "~/.cache"))))
        (with-eval-after-load 'gnus-start
          (setq gnus-dribble-directory (expand-file-name "emacs/gnus" (or (xdg-cache-home) "~/.cache")))
          (setq gnus-startup-file (expand-file-name "emacs/gnus/.newsrc" (or (xdg-cache-home) "~/.cache")))
          (setq gnus-subscribe-newsgroup-method 'gnus-subscribe-hierarchically)
          (setq gnus-check-new-newsgroups nil)
          (setq gnus-save-killed-list nil))
        (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
        (add-hook 'gnus-group-mode-hook 'hl-line-mode)
        (with-eval-after-load 'gnus-sum
          (setq gnus-thread-hide-subtree t))
        (with-eval-after-load 'nndraft-directory
          (setq nndraft-directory (expand-file-name "emacs/gnus/mail/drafts" (or (xdg-cache-home) "~/.cache"))))
        (add-hook 'gnus-topic-mode-hook 'rde-gnus-topic-mode)
        (with-eval-after-load 'gnus-topic
          (setq gnus-gcc-mark-as-read t)
          (setq gnus-server-alist '(("archive" nnfolder "archive"
                                     (nnfolder-directory ,(string-append mail-dir "/archive"))
                                     (nnfolder-get-new-mail nil)
                                     (nnfolder-inhibit-expiry t)))))
        (with-eval-after-load 'gnus-art
          (setq gnus-visible-headers
                '("^From:" "^To:" "^Cc:" "^Subject:" "^Newsgroups:" "^Date:"
                  "Followup-To:" "Reply-To:" "^Organization:" "^X-Newsreader:"
                  "^X-Mailer:" "^Message-ID:" "^In-Reply-To:" "^References:"))
          (setq gnus-sorted-header-list gnus-visible-headers))
        ,#~"(with-eval-after-load 'gnus-art
(define-key gnus-article-mode-map [remap shr-mouse-browse-url] #'shr-mouse-browse-url-new-window)
(define-key gnus-article-mode-map [remap shr-browse-url] #'rde-gnus-shr-browse-url-new-window))"))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


;;;
;;; feature-emacs-debbugs
;;;

(define* (feature-emacs-debbugs
          #:key
          (emacs-debbugs emacs-debbugs)
          (debbugs-gnu-default-packages
           (list "emacs" "guix" "guix-patches")))
  "Configure the Debbugs user interface for Emacs."
  (ensure-pred file-like? emacs-debbugs)
  (ensure-pred list? debbugs-gnu-default-packages)

  (define emacs-f-name 'debbugs)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Debbugs."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load 'debbugs
          (setq debbugs-gnu-default-packages ,debbugs-gnu-default-packages)))
      #:elisp-packages (list emacs-debbugs))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-debbugs)))
   (home-services-getter get-home-services)))
