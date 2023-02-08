(define-module (rde features matrix)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (rde features web)
  #:use-module (rde home services matrix)
  #:use-module (rde system services matrix)
  #:use-module (conses packages emacs-xyz)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu services certbot)
  #:use-module (gnu services configuration)
  #:use-module (gnu services web)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages matrix)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (matrix-account
            matrix-account?
            matrix-account-id
            matrix-account-homeserver
            feature-matrix-settings
            feature-pantalaimon
            feature-synapse
            feature-emacs-ement))

(define-configuration/no-serialization matrix-account
  (id
   (maybe-string #f)
   "The Matrix ID to use. It should take the form of
 @code{\"@username:example.com\"}.")
  (homeserver
   (maybe-string #f)
   "The Matrix homeserver the account is registered under."))

(define (list-of-matrix-accounts? lst)
  (and (list? lst) (not (null? lst)) (every matrix-account? lst)))

(define (maybe-list-of-matrix-accounts? x)
  (or (list-of-matrix-accounts? x) (not x)))

(define* (feature-matrix-settings
          #:key
          (homeserver "https://matrix.org")
          (matrix-accounts #f)
          (synapse-configuration (synapse-configuration))
          (mautrix-whatsapp-configuration (mautrix-whatsapp-configuration)))
  (ensure-pred string? homeserver)
  (ensure-pred maybe-list-of-matrix-accounts? matrix-accounts)
  (ensure-pred synapse-configuration? synapse-configuration)
  (ensure-pred mautrix-whatsapp-configuration? mautrix-whatsapp-configuration)

  (feature
   (name 'matrix-settings)
   (values (append
            `((matrix-settings . #t)
              (matrix-homeserver . ,homeserver))
            (make-feature-values matrix-accounts synapse-configuration
                                 mautrix-whatsapp-configuration)))))

(define* (feature-pantalaimon
          #:key
          (pantalaimon pantalaimon)
          (port 8009)
          (ssl? #t)
          (ignore-device-verification? #f)
          (extra-config '()))
  "Configure Pantalaimon, an E2EE-aware proxy daemon for Matrix clients.
See @uref{https://github.com/matrix-org/pantalaimon/blob/master/docs/man/pantalaimon.5.md,
Pantalaimon's man page} for the list of available options."
  (ensure-pred file-like? pantalaimon)
  (ensure-pred integer? port)
  (ensure-pred boolean? ssl?)
  (ensure-pred boolean? ignore-device-verification?)
  (ensure-pred list? extra-config)

  (define (get-home-services config)
    "Return home services related to Pantalaimon"
    (require-value 'matrix-settings config)

    (list
     (service
      home-pantalaimon-service-type
      (home-pantalaimon-configuration
       (pantalaimon pantalaimon)
       (config
        `((Default
           ((LogLevel . debug)))
          (local-matrix
           ((Homeserver . ,(get-value 'matrix-homeserver config))
            (ListenAddress . localhost)
            (ListenPort . ,port)
            (Ssl . ,ssl?)
            (IgnoreVerification . ,ignore-device-verification?)
            (UseKeyring . #f)))
          ,@extra-config))))))

  (define (get-system-services config)
    "Return system services related to Pantalaimon."
    (define domain (get-value 'domain config))
    (define letsencrypt-dir
      (and domain (string-append "/etc/letsencrypt/live/pantalaimon." domain)))

    (append
     (if (get-value 'nginx config)
         (list
          (simple-service
           'add-pantalaimon-nginx-configuration
           nginx-service-type
           (list
            (nginx-server-configuration
             (listen '("443 ssl http2"))
             (server-name (list (string-append "pantalaimon." domain)))
             (ssl-certificate (string-append letsencrypt-dir "/fullchain.pem"))
             (ssl-certificate-key (string-append letsencrypt-dir "/privkey.pem"))
             (locations
              (list
               (nginx-location-configuration
                (uri "/")
                (body
                 (list "proxy_pass http://localhost:8009/;"
                       "proxy_set_header X-Forwarded-For $remote_addr;"
                       "proxy_set_header HOST $http_host;")))
               %letsencrypt-acme-challenge))))))
         '())
     (if (get-value 'certbot config)
         (list
          (simple-service
           'add-pantalaimon-ssl
           certbot-service-type
           (list
            (certificate-configuration
             (domains (list (string-append "pantalaimon." domain)))
             (deploy-hook %nginx-deploy-hook)))))
         '())
     '()))

  (feature
   (name 'pantalaimon)
   (values `((pantalaimon . ,pantalaimon)
             (pantalaimon-port . ,port)))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))

(define* (feature-synapse
          #:key
          (whatsapp-bridge? #f))
  "Configure Synapse, the Matrix flagship home server."
  (ensure-pred boolean? whatsapp-bridge?)

  (define (get-system-services config)
    "Return system services related to Synapse."
    (require-value 'matrix-settings config)
    (define homeserver (get-value 'matrix-homeserver config))
    (define server-name (string-drop homeserver (+ 1 (string-index-right homeserver #\/))))
    (define domain (string-drop server-name (+ 1 (string-index server-name #\.))))
    (define letsencrypt-dir (and domain (string-append "/etc/letsencrypt/live/matrix." domain)))

    (append
     (list
      (service synapse-service-type
               (get-value 'synapse-configuration config)))
     (if whatsapp-bridge?
         (list
          (service mautrix-whatsapp-service-type (get-value 'mautrix-whatsapp-configuration config)))
         '())
     (if (get-value 'nginx config)
         (list
          (simple-service
           'add-synapse-nginx-configuration
           nginx-service-type
           (list
            (nginx-server-configuration
             (listen '("443 ssl http2"
                       "[::]:443 ssl http2"
                       "8448 ssl http2 default_server"
                       "[::]:8448 ssl http2 default_server"))
             (server-name (list server-name))
             (ssl-certificate (string-append letsencrypt-dir "/fullchain.pem"))
             (ssl-certificate-key (string-append letsencrypt-dir "/privkey.pem"))
             (locations
              (list
               (nginx-location-configuration
                (uri "~ ^(/_matrix|/_synapse/client)")
                (body
                 (list "proxy_pass http://localhost:8008;"
                       "proxy_set_header X-Forwarded-For $remote_addr;"
                       "proxy_set_header X-Forwarded-Proto $scheme;"
                       "proxy_set_header Host $host;"
                       "client_max_body_size 50M;")))
               %letsencrypt-acme-challenge))))))
         '())
     (if (get-value 'certbot config)
         (list
          (simple-service
           'add-synapse-ssl
           certbot-service-type
           (list
            (certificate-configuration
             (name "matrix")
             (domains (list server-name))
             (deploy-hook %nginx-deploy-hook)))))
         '())))

  (feature
   (name 'synapse)
   (values `((synapse . #t)
             (matrix-whatsapp-bridge? . ,whatsapp-bridge?)))
   (system-services-getter get-system-services)))

(define* (feature-emacs-ement
          #:key
          (emacs-ement emacs-ement)
          (ement-key "e"))
  "Configure Ement, the Matrix client for Emacs."
  (ensure-pred file-like? emacs-ement)
  (ensure-pred string? ement-key)

  (define emacs-f-name 'ement)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Ement."
    (require-value 'matrix-settings config)
    (define homeserver (get-value 'matrix-homeserver config))

    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
          (require 'ement))
        (defgroup rde-ement nil
          "Utilities for Ement, the Emacs Matrix client."
          :group 'rde)
        (cl-defstruct rde-ement-user id homeserver)
        (defcustom rde-ement-users '()
          "List of `rde-ement-user' structs that hold Matrix accounts."
          :type '(repeat rde-ement-user)
          :group 'rde-ement)

        ,@(if (get-value 'emacs-consult-initial-narrowing? config)
              '((defvar rde-ement-buffer-source
                  `(:name "Ement"
                          :narrow ?e
                          :category buffer
                          :preview-key ,(kbd "M-.")
                          :state ,'consult--buffer-state
                          :items ,(lambda ()
                                    (mapcar 'buffer-name (rde-completion--mode-buffers
                                                          'ement-room-mode
                                                          'ement-room-list-mode))))
                  "Source for Ement buffers to be set in `consult-buffer-sources'.")
                (add-to-list 'consult-buffer-sources rde-ement-buffer-source)
                (add-to-list 'rde-completion-initial-narrow-alist '(ement-room-mode . ?e))
                (add-to-list 'rde-completion-initial-narrow-alist '(ement-room-list-mode . ?e)))
              '())

        (defun rde-ement-connect (user)
          "Connect to Matrix homeserver with USER."
          (interactive
           (list (cl-find (completing-read
                           "User: "
                           (lambda (string pred action)
                             (if (eq action 'metadata)
                                 `(metadata
                                   ,(cons 'display-sort-function 'identity))
                               (complete-with-action
                                action
                                (mapcar 'rde-ement-user-id rde-ement-users)
                                string pred))))
                          rde-ement-users :key 'rde-ement-user-id :test 'string=)))
          (let ((homeserver (rde-ement-user-homeserver user))
                (id (rde-ement-user-id user)))
            (ement-connect
             :user-id id
             :password (auth-source-pick-first-password
                        :host homeserver
                        :user (substring id 1 (string-match ":" id)))
             :uri-prefix ,(if (get-value 'pantalaimon config)
                              (string-append "http://localhost:"
                                             (number->string (get-value 'pantalaimon-port config)))
                              homeserver))))

        (with-eval-after-load 'rde-keymaps
          (define-key rde-app-map (kbd ,ement-key) 'rde-ement-connect))
        (setq rde-ement-users
              (list
               ,@(map
                  (lambda (matrix-acc)
                    `(make-rde-ement-user
                      :id ,(matrix-account-id matrix-acc)
                      :homeserver ,(matrix-account-homeserver matrix-acc)))
                  (get-value 'matrix-accounts config))))
        (with-eval-after-load 'ement
          (add-hook 'ement-room-compose-hook 'ement-room-compose-org)
          (let ((map ement-room-mode-map))
            (define-key map "c" 'ement-room-compose-message)
            (define-key map "E" 'ement-room-edit-message))
          (setq ement-room-send-message-filter 'ement-room-send-org-filter)
          (setq ement-notify-notification-predicates
                '(ement-notify--event-mentions-session-user-p
                  ement-notify--event-mentions-room-p))
          (setq ement-save-sessions t)
          (setq ement-room-send-read-receipts nil)))
      #:elisp-packages (list emacs-ement))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-ement)))
   (home-services-getter get-home-services)))
