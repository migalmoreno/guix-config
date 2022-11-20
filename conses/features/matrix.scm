(define-module (conses features matrix)
  #:use-module (conses utils)
  #:use-module (conses home services matrix)
  #:use-module (conses system services matrix)
  #:use-module (conses packages emacs-xyz)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu services web)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
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
   "The Matrix ID to use. It should take the form
of @code{\"@username:example.com\"}, for instance.")
  (homeserver
   (maybe-string #f)
   "The Matrix homeserver where this user @code{id} is registered under.")
  (local?
   (boolean #f)
   "Whether the Matrix account belongs to a personal homeserver."))

(define (list-of-matrix-accounts? lst)
  (and (list? lst) (not (null? lst)) (every matrix-account? lst)))

(define* (feature-matrix-settings
          #:key
          (homeserver "https://matrix.org")
          (matrix-accounts #f)
          (synapse-configuration (synapse-configuration))
          (mautrix-whatsapp-configuration (mautrix-whatsapp-configuration)))
  (ensure-pred string? homeserver)
  (ensure-pred list-of-matrix-accounts? matrix-accounts)
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
          (extra-config '()))
  "Configure Pantalaimon, an E2EE-aware proxy
daemon for Matrix clients."
  (ensure-pred any-package? pantalaimon)
  (ensure-pred list? extra-config)

  (define (get-home-services config)
    "Return home services related to Pantalaimon"
    (require-value 'matrix-settings config)

    (list
     (service
      home-pantalaimon-service-type
      (home-pantalaimon-configuration
       (config
        `((Default
           ((log-level . debug)))
          (local-matrix
           ((homeserver . ,(get-value 'matrix-homeserver config))
            (listen-address . localhost)
            (listen-port . 8009)
            (ssl . #f)
            (ignore-verification . #t)
            (use-keyring . #f)))
          ,@extra-config))))))

  (feature
   (name 'pantalaimon)
   (values `((pantalaimon . ,pantalaimon)))
   (home-services-getter get-home-services)))

(define* (feature-synapse
          #:key
          (whatsapp-bridge? #f))
  "Configure Synapse, the Matrix flagship home server."
  (ensure-pred boolean? whatsapp-bridge?)

  (define (get-system-services config)
    "Return system services related to Synapse."
    (require-value 'matrix-settings config)
    (define letsencrypt-dir "/etc/letsencrypt/live")
    (define homeserver (get-value 'matrix-homeserver config))
    (define server-name (string-drop homeserver (+ 1 (string-index-right homeserver #\/))))
    (define domain (string-drop server-name (+ 1 (string-index server-name #\.))))
    (define synapse-configuration (get-value 'synapse-configuration config))
    (define mautrix-whatsapp-configuration (get-value 'mautrix-whatsapp-configuration config))

    (append
     (list
      (simple-service
       'synapse-nginx-service
       nginx-service-type
       (list
        (nginx-server-configuration
         (listen '("443 ssl http2"
                   "[::]:443 ssl http2"
                   "8448 ssl http2 default_server"
                   "[::]:8448 ssl http2 default_server"))
         (server-name (list homeserver))
         (ssl-certificate (string-append letsencrypt-dir "/" domain "/fullchain.pem"))
         (ssl-certificate-key (string-append letsencrypt-dir "/" domain "/privkey.pem"))
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
           (nginx-location-configuration
            (uri "/.well-known")
            (body '("root /srv/http;"))))))))
      (service synapse-service-type synapse-configuration))
     (if whatsapp-bridge?
         (list
          (service mautrix-whatsapp-service-type mautrix-whatsapp-configuration))
         '())))

  (feature
   (name 'synapse)
   (values `((synapse . #t)
             (matrix-whatsapp-bridge? . ,whatsapp-bridge?)))
   (system-services-getter get-system-services)))


(define* (feature-emacs-ement
          #:key
          (emacs-ement emacs-ement-next))
  "Configure Ement, the Matrix client for Emacs."
  (ensure-pred any-package? emacs-ement)

  (define emacs-f-name 'ement)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Ement."
    (require-value 'matrix-accounts config)

    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
          (require 'ement))
        (require 'configure-rde-keymaps)

        (defgroup configure-ement nil
          "Utilities for Ement, the Emacs Matrix client."
          :group 'configure)

        (defcustom configure-ement-users '()
          "A list of `configure-ement-user' structs that hold Matrix accounts."
          :type 'list
          :group 'configure-ement)

        (cl-defstruct configure-ement-user
          "An Ement user."
          id homeserver local-p)

        ,@(if (get-value 'emacs-consult-initial-narrowing? config)
              '((defvar configure-ement-buffer-source
                  `(:name "Ement"
                          :narrow ?e
                          :category buffer
                          :preview-key ,(kbd "M-.")
                          :state ,'consult--buffer-state
                          :items ,(lambda ()
                                    (mapcar 'buffer-name (configure-completion--mode-buffers
                                                          'ement-room-mode
                                                          'ement-room-list-mode))))
                  "Source for Ement buffers to be set in `consult-buffer-sources'.")
                (add-to-list 'consult-buffer-sources configure-ement-buffer-source)
                (add-to-list 'configure-completion-initial-narrow-alist '(ement-room-mode . ?e))
                (add-to-list 'configure-completion-initial-narrow-alist '(ement-room-list-mode . ?e)))
              '())

        (defun configure-ement-connect (user)
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
                                (mapcar 'configure-ement-user-id configure-ement-users)
                                string pred))))
                          configure-ement-users :key 'configure-ement-user-id :test 'string=)))
          (let ((homeserver (configure-ement-user-homeserver user)))
            (ement-connect
             :user-id (configure-ement-user-id user)
             :password (auth-source-pick-first-password :host homeserver)
             :uri-prefix (if (configure-ement-user-local-p user)
                             "http://localhost:8009"
                           homeserver))))

        (define-key rde-app-map "e" 'configure-ement-connect)
        (setq configure-ement-users
              (list
               ,@(map
                  (lambda (matrix-acc)
                    `(make-configure-ement-user
                      :id ,(matrix-account-id matrix-acc)
                      :homeserver ,(matrix-account-homeserver matrix-acc)
                      :local-p ,(if (matrix-account-local? matrix-acc)
                                    't
                                  'nil)))
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
          (setq ement-room-send-read-receipts nil)
          (setq warning-suppress-log-types '((ement-room-send-event-callback)
                                             (ement-room-send-event-callback)))))
      #:elisp-packages (list emacs-ement
                             (get-value 'emacs-configure-rde-keymaps config)))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-ement)))
   (home-services-getter get-home-services)))
