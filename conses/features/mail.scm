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
  #:export (feature-goimapnotify
            feature-emacs-ebdb
            feature-emacs-gnus
            feature-emacs-message
            feature-emacs-smtpmail
            feature-emacs-org-mime
            %base-mail-features
            mail-acc
            mail-lst
            mail-directory-fn))

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
                                                          :timeout 0))))))))
              mail-accounts)))))))

  (feature
   (name 'goimapnotify)
   (values `((goimapnotify . ,goimapnotify)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-ebdb
          #:key
          (emacs-ebdb emacs-ebdb)
          (ebdb-sources (list "~/documents/contacts")))
  "Configure the EBDB contact management package for Emacs."
  (ensure-pred any-package? emacs-ebdb)
  (ensure-pred list-of-strings? ebdb-sources)

  (define emacs-f-name 'ebdb)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to EBDB."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((let ((map mode-specific-map))
          (define-key map "ba" 'ebdb-display-all-records)
          (define-key map "bi" 'ebdb-create-record-extended))
        (with-eval-after-load 'ebdb
          (require 'ebdb-i18n)
          (require 'ebdb-vcard)
          (require 'ebdb-org)
          (require 'ebdb-mua)
          (setq ebdb-sources (list ,@ebdb-sources))
          (setq ebdb-default-country nil)
          (setq ebdb-default-window-size 0.2)
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
      `((defgroup configure-message nil
          "Message mode tweaks."
          :group 'configure)

        (defun configure-message-add-gcc-header ()
          "Prompt for a Gcc header from `configure-gnus-topic-alist'.
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
                                                    (configure-gnus--get-topic-groups))))
                                              (if (> 1 (length groups))
                                                  (completing-read "Account: " groups)
                                                (car groups))))))
            (error "Gnus is not running.  No GCC header will be inserted")))

        (define-minor-mode configure-message-mode
          "Tweak the appearance of `message-mode' buffers."
          :global t :group 'configure-message
          (if configure-message-mode
              (display-line-numbers-mode -1)
            (display-line-numbers-mode 1)))
        ,@(if (get-value 'emacs-org-mime config)
              `((add-hook 'message-send-hook 'org-mime-confirm-when-no-multipart))
              '())

        (add-hook 'message-mode-hook 'configure-message-mode)
        (with-eval-after-load 'message
          ,@(if (get-value 'emacs-ebdb config)
                `((require 'ebdb-message))
              '())
          (add-hook 'message-header-setup-hook 'configure-message-add-gcc-header)
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
        (defgroup configure-org-mime nil
          "Minor tweaks for Org Mime."
          :group 'configure)

        (defun configure-org-mime-darken-codeblocks ()
          "Apply a dark background to HTML email body codeblocks."
          (org-mime-change-element-style
           "pre"
           "color: #E6E1Dc; background-color: #232323; padding: 0.5em;"))

        (defun configure-org-mime-indent-quotes ()
          "Add padding to block quotes in HTML email body."
          (org-mime-change-element-style
           "blockquote"
           "border-left: 2px solid gray; padding-left: 4px;"))

        (define-key org-mode-map (kbd "C-c M-o") 'org-mime-org-buffer-htmlize)
        (add-hook 'org-mime-html-hook 'configure-org-mime-darken-codeblocks)
        (add-hook 'org-mime-html-hook 'configure-org-mime-indent-quotes)
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

(define* (feature-emacs-smtpmail)
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
          (setq smtpmail-stream-type 'starttls)
          (setq smtpmail-queue-dir "~/.cache/gnus/Mail/queued-mail")
          (setq smtpmail-debug-info t))))))

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
          (mail-account-ids #f))
  "Configure the Gnus newsreader. If MAIL-ACCOUNT-IDS
is not provided, use all the mail accounts."
  (ensure-pred maybe-list? posting-styles)
  (ensure-pred maybe-list? group-parameters)
  (ensure-pred maybe-list? topic-alist)
  (ensure-pred maybe-list? topic-topology)
  (ensure-pred path? gnus-directory)
  (ensure-pred maybe-list? mail-account-ids)

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
      `((require 'configure-rde-keymaps)
        (defgroup configure-gnus nil
          "Customizations for the Gnus newsreader."
          :group 'configure)

        (defcustom configure-gnus-topic-topology nil
          "Topics topology for Gnus."
          :group 'configure-gnus
          :type 'list)

        (defcustom configure-gnus-topic-alist nil
          "Alist of Gnus topics."
          :group 'configure-gnus
          :type 'list)

        (defvar configure-gnus-subscribed-p nil
          "Whether we're currently subscribed to Gnus groups.")

        (defun configure-gnus--get-topic-groups ()
          "Return a flattened list of groups from `configure-gnus-topic-alist'."
          (flatten-list (mapcar (lambda (topic)
                                  (cdr topic))
                                configure-gnus-topic-alist)))

        (defun configure-gnus-get-article-participants ()
          "Retrieve the participants from the current article."
          (when (gnus-alive-p)
            (with-current-buffer gnus-article-buffer
              (string-join
               (remove-if
                (lambda (address)
                  (string-match user-mail-address address))
                (append
                 (split-string (message-fetch-field "from") ", ")
                 (split-string (message-fetch-field "to") ", ")))
               ", "))))

        (defun configure-gnus-shr-browse-url-new-window ()
          "When using shr, open links in a new window."
          (interactive)
          (shr-browse-url nil nil t))

        (define-minor-mode configure-gnus-topic-mode
          "Apply Gnus topic settings declaratively and subscribe to groups."
          :global t :group 'configure-gnus
          (setq gnus-topic-topology configure-gnus-topic-topology)
          (setq gnus-topic-alist configure-gnus-topic-alist)
          (unless configure-gnus-subscribed-p
            (mapc (lambda (topic)
                    (gnus-subscribe-hierarchically topic))
                  (configure-gnus--get-topic-groups)))
          (setq configure-gnus-subscribed-p t))

        (setq configure-gnus-topic-alist ',topic-alist)
        (setq configure-gnus-topic-topology ',topic-topology)
        (define-key rde-app-map "g" 'gnus)
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
          (setq gnus-select-method '(nnnil))
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
          (setq mail-source-directory "~/.cache/gnus/Mail")
          (setq mail-default-directory "~/.cache/gnus"))
        (with-eval-after-load 'gnus-start
          (setq gnus-dribble-directory (locate-user-emacs-file "gnus"))
          (setq gnus-startup-file (locate-user-emacs-file "gnus/.newsrc"))
          (setq gnus-subscribe-newsgroup-method 'gnus-subscribe-hierarchically)
          (setq gnus-check-new-newsgroups nil)
          (setq gnus-save-killed-list nil))
        (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
        (add-hook 'gnus-group-mode-hook 'hl-line-mode)
        (with-eval-after-load 'gnus-sum
          (setq gnus-thread-hide-subtree t))
        (add-hook 'gnus-topic-mode-hook 'configure-gnus-topic-mode)
        (with-eval-after-load 'gnus-topic
          (setq gnus-gcc-mark-as-read t)
          (setq gnus-server-alist '(("archive" nnfolder "archive"
                                     (nnfolder-directory ,(string-append mail-dir "/archive"))
                                     (nnfolder-get-new-mail nil)
                                     (nnfolder-inhibit-expiry t))))
          (setq gnus-update-message-archive-method t))
        ,#~"(with-eval-after-load 'gnus-art
(define-key gnus-article-mode-map [remap shr-mouse-browse-url] #'shr-mouse-browse-url-new-window)
(define-key gnus-article-mode-map [remap shr-browse-url] #'configure-gnus-shr-browse-url-new-window))")
      #:elisp-packages (list emacs-debbugs
                             (get-value 'emacs-configure-rde-keymaps config)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
