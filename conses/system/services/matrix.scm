(define-module (conses system services matrix)
  #:use-module (conses packages matrix)
  #:use-module (conses serializers yaml)
  #:use-module (conses system services databases)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages matrix)
  #:use-module (gnu system shadow)
  #:use-module (gnu system accounts)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:export (synapse-configuration
            synapse-configuration?
            synapse-service-type
            synapse-extension
            mautrix-whatsapp-configuration
            mautrix-whatsapp-configuration?
            mautrix-whatsapp-service-type))

(define (maybe-string? x)
  (or (string? x) (not x)))

(define-maybe/no-serialization maybe-string)

(define-configuration/no-serialization synapse-configuration
  (synapse
    (package synapse)
    "The @code{synapse} package to use.")
  (server-name
   (string "localhost")
   "The public-facing domain of the server. This is used by remote servers to look up
 the server address and will appear at the end of usernames and room addresses created on
 this server. The @code{server_name} cannot be changed later so it's important to configure this before
you start Synapse. It should be all lowercase and may contain an explicit port.")
  (public-base-url
   (string "")
   "The public-facing base URL that clients use to access this homeserver.")
  (enable-registration?
   (boolean #f)
   "Whether to enable registration for new users.")
  (shared-secret
   (string "")
   "If set, it allows registration of standard or admin accounts by anyone who has the shared secret,
even if registration is otherwise disabled.")
  (secret-key
   (maybe-string #f)
   "A secret which is used to sign access tokens. If none is specified, the @code{registration_shared_secret}
is used, if one is given; otherwise, a secret key is derived from the signing key.")
  (postgresql-db?
   (boolean #f)
   "Whether to use a PostgreSQL database for storage.")
  (postgresql-db-password
   (maybe-string "")
   "The password for the PostgreSQL database user. Do note this will be exposed in a file under /gnu/store
in plain sight.")
  (max-upload-size
   (string "50M")
   "The largest allowed upload size in bytes.")
  (data-directory
   (string "/var/lib/matrix-synapse")
   "Indicates the path where data such as the media store and database files should be stored.")
  (config-directory
   (string "/var/lib/matrix-synapse")
   "Specifies where additional configuration files such as signing keys and log
configuration should be stored.")
  (trusted-key-servers
   (yaml-config '())
   "The trusted servers to download signing keys from.")
  (report-stats?
   (boolean #f)
   "Indicates whether or not to report anonymized homeserver usage statistics.")
  (extra-config
   (yaml-config '())
   "Alist, vector, gexp, or file-like objects to write to a Synapse homeserver
configuration to be placed under @file{homeserver.yaml}. See more settings in
@url{https://matrix-org.github.io/synapse/latest/usage/configuration/homeserver_sample_config.html}."))

(define (synapse-shepherd-service config)
  "Returns a Shepherd service for Synapse."
  (list
   (shepherd-service
    (provision '(synapse))
    (requirement (if (synapse-configuration-postgresql-db? config)
                     '(postgres)
                     '()))
    (start #~(make-forkexec-constructor
              (list (string-append #$(synapse-configuration-synapse config)
                                   "/bin/synapse_homeserver")
                    "-c"
                    #$(synapse-file config)
                    "--config-directory"
                    #$(synapse-configuration-config-directory config)
                    "--data-directory"
                    #$(synapse-configuration-data-directory config))
              #:log-file "/var/log/matrix-synapse.log"
              #:environment-variables
              (append (list "SSL_CERT_DIR=/etc/ssl/certs"
                            "SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt")
                      (remove (lambda (str)
                                (or (string-prefix? "SSL_CERT_DIR=" str)
                                    (string-prefix? "SSL_CERT_FILE=" str)))
                              (environ)))))
    (stop #~(make-kill-destructor)))))

(define (synapse-fill-defaults config)
  "Returns a list of configuration strings from the fields that need to be filled in
@code{synapse-configuration}. Useful for later serialization."
  (let ((data-directory (synapse-configuration-data-directory config))
        (config-directory (synapse-configuration-config-directory config))
        (server-name (synapse-configuration-server-name config)))
    (serialize-yaml-config
     `((pid-file . ,(string-append data-directory "/homeserver.pid"))
       (server-name . ,server-name)
       (public-base-url . ,(synapse-configuration-public-base-url config))
       (media-store-path . ,(string-append data-directory "/media_store"))
       (max-upload-size . ,(synapse-configuration-max-upload-size config))
       (registration-shared-secret . ,(synapse-configuration-shared-secret config))
       (macaroon-secret-key . ,(if (synapse-configuration-secret-key config)
                                   (synapse-configuration-secret-key config)
                                   (synapse-configuration-shared-secret config)))
       (signing-key-path . ,(string-append config-directory "/homeserver.signing.key"))
       (enable-registration . ,(synapse-configuration-enable-registration? config))
       (report-stats . ,(synapse-configuration-report-stats? config))
       (database . ,(if (synapse-configuration-postgresql-db? config)
                        `((name . psycopg2)
                          (allow-unsafe-locale . #t)
                          (args . ((user . "matrix-synapse")
                                   (database . "matrix-synapse")
                                   (host . localhost)
                                   (password . ,(synapse-configuration-postgresql-db-password config))
                                   (port . 5432)
                                   (cp-min . 5)
                                   (cp-max . 10))))
                        `((name . sqlite3)
                          (args . ((database . ,(string-append data-directory "/homeserver.db")))))))
       (listeners . #(((port . 8008)
                       (tls . #f)
                       (type . http)
                       (x-forwarded . true)
                       (bind-addresses . #("::1" "127.0.0.1"))
                       (resources . #(((names . (client federation))
                                       (compress . #f)))))))
       (trusted-key-servers . ,(if (null? (synapse-configuration-trusted-key-servers config))
                                   #(((server-name . "matrix.org")))
                                   (synapse-configuration-trusted-key-servers config)))))))

(define %synapse-accounts
  (list
   (user-group
    (name "matrix-synapse")
    (system? #t))
   (user-account
    (name "matrix-synapse")
    (group "matrix-synapse")
    (system? #t)
    (home-directory "/var/empty")
    (shell (file-append shadow "/sbin/nologin")))))

(define (synapse-file config)
  (mixed-text-file
   "homeserver.yaml"
   #~(string-append
      #$@(synapse-fill-defaults config)
      #$@(serialize-yaml-config (synapse-configuration-extra-config config)))))

(define (synapse-etc-service config)
  "Serializes Synapse's configuration."
  (filter
   (compose not null?)
   `(("matrix-synapse/homeserver.yaml"
      ,(synapse-file config)))))

(define (synapse-profile-service config)
  (list (synapse-configuration-synapse config)))

(define (synapse-postgresql-service config)
  (if (synapse-configuration-postgresql-db? config)
    (list
     (postgresql-role
      (name "matrix-synapse")
      (create-database? #t)
      (password (synapse-configuration-postgresql-db-password config))
      (collation "C")
      (ctype "C")))
    '()))

(define (synapse-activation-service config)
  #~(begin
      (use-modules (guix build utils))

      (define %user (getpw "matrix-synapse"))
      (define data-dir #$(synapse-configuration-data-directory config))
      (define config-dir #$(synapse-configuration-config-directory config))
      (define signing-key-path #$(string-append (synapse-configuration-config-directory config)
                                                "/homeserver.signing.key"))
      (define (generate-signing-key)
        (unless (stat signing-key-path #f)
          (system* #$(file-append (synapse-configuration-synapse config) "/bin/generate_signing_key")
                   "-o"
                   signing-key-path)))

      (mkdir-p data-dir)
      (chown data-dir (passwd:uid %user) (passwd:gid %user))
      (chmod data-dir #o700)
      (mkdir-p config-dir)
      (chown config-dir (passwd:uid %user) (passwd:gid %user))
      (chmod config-dir #o700)
      (generate-signing-key)
      (chown signing-key-path (passwd:uid %user) (passwd:gid %user))
      (chmod signing-key-path #o600)))

(define-configuration/no-serialization synapse-extension
  (extra-config
   (yaml-config '())
   "See @code{synapse-service-type} for more information."))

(define (synapse-extension-service original-config extension-configs)
  (synapse-configuration
   (inherit original-config)
   (extra-config
    (append (synapse-configuration-extra-config original-config)
            (append-map synapse-extension-extra-config extension-configs)))))

(define (generate-synapse-documentation)
  (generate-documentation
   `((synapse-configuration
      ,synapse-configuration-fields))
   'synapse-configuration))

(define synapse-service-type
  (service-type
   (name 'synapse)
   (extensions
    (list
     (service-extension
      postgresql-role-service-type
      synapse-postgresql-service)
     (service-extension
      etc-service-type
      synapse-etc-service)
     (service-extension
      profile-service-type
      synapse-profile-service)
     (service-extension
      account-service-type
      (const %synapse-accounts))
     (service-extension
      activation-service-type
      synapse-activation-service)
     (service-extension
      shepherd-root-service-type
      synapse-shepherd-service)))
   (compose identity)
   (extend synapse-extension-service)
   (default-value (synapse-configuration))
   (description "System service for Synapse, the Matrix flagship implementation.")))

(define-configuration/no-serialization mautrix-whatsapp-configuration
  (mautrix-whatsapp
    (package mautrix-whatsapp)
    "The @code{mautrix-whatsapp} package to use.")
  (address
   (string "http://localhost:8008")
   "The address that this appservice can use to connect to the homeserver.")
  (domain
   (string "")
   "The domain of the homeserver (for MXIDs, etc).")
  (postgresql-db?
   (boolean #f)
   "Whether to use PostgreSQL as the database type.")
  (postgresql-db-password
   (maybe-string "")
   "The password for the PostgreSQL database user. Do note this will be exposed in a file under /gnu/store
in plain sight.")
  (encryption?
   (boolean #f)
   "Whether to add end-to-bridge encryption support.")
  (data-directory
   (string "/var/lib/mautrix-whatsapp")
   "Indicates the path where data such as the registration and database files should be stored.")
  (log-directory
   (string "/var/log/mautrix-whatsapp")
   "The directory for @code{mautrix-whatsapp} log files.")
  (permissions
   (alist '())
   "The permissions for using the bridge. Permitted values include:
@itemize
@item relay - Talk through the relaybot (if enabled), no access otherwise
@item user - Access to use the bridge to chat with a WhatsApp account
@item admin - User level and some additional administration tools
@end itemize
Permitted keys include:
@itemize
@item * - All Matrix users
@item domain - All users on that homeserver
@item mxid - Specific user
@end itemize")
  (extra-config
   (yaml-config '())
   "Alist, vector, gexp, or file-like objects to write to a @code{mautrix-whatsapp} bridge
configuration to be placed under @file{config.yaml}. See more settings in
@url{https://github.com/mautrix/whatsapp/blob/master/example-config.yaml}."))

(define %mautrix-whatsapp-accounts
  (list
   (user-group
    (name "mautrix-whatsapp")
    (system? #t))
   (user-account
    (name "mautrix-whatsapp")
    (group "mautrix-whatsapp")
    (system? #t)
    (home-directory "/var/empty")
    (shell (file-append shadow "/sbin/nologin")))))

(define (mautrix-whatsapp-postgresql-service config)
  (if (mautrix-whatsapp-configuration-postgresql-db? config)
    (list
     (postgresql-role
      (name "mautrix-whatsapp")
      (create-database? #t)
      (password (mautrix-whatsapp-configuration-postgresql-db-password config))
      (collation "C")
      (ctype "C")))
    '()))

(define (mautrix-whatsapp-fill-defaults config)
  "Returns a list of configuration strings from the fields that need to be filled in
@code{mautrix-whatsapp-configuration}. Useful for later serialization."
  (serialize-yaml-config
   `((homeserver . ((address . ,(mautrix-whatsapp-configuration-address config))
                    (domain . ,(mautrix-whatsapp-configuration-domain config))))
     (appservice . ((address . "http://localhost:29318")
                    (hostname . "0.0.0.0")
                    (port . 29318)
                    (database . ,(if (mautrix-whatsapp-configuration-postgresql-db? config)
                                     `((type . postgres)
                                       (uri . ,(string-append "postgres://mautrix-whatsapp:"
                                                              (mautrix-whatsapp-configuration-postgresql-db-password config)
                                                              "@localhost/mautrix-whatsapp?sslmode=disable")))
                                     `((type . sqlite3)
                                       (uri . ,(string-append (mautrix-whatsapp-configuration-data-directory config)
                                                              "/mautrix-whatsapp.db")))))
                    (id . whatsapp)
                    (bot . ((username . whatsappbot)
                            (displayname . "WhatsApp bridge bot")))
                    (as-token . "")
                    (hs-token . "")))
     (bridge . ((username-template . "whatsapp_{{.}}")
                (displayname-template . ,(string-append "{{if .BusinessName}}{{.BusinessName}}"
                                                        "{{else if .PushName}}{{.PushName}}{{else}}{{.JID}}{{end}} (WA)"))
                (command-prefix . "!wa")
                (permissions . (("*" . relay)
                                ,@(mautrix-whatsapp-configuration-permissions config)))
                (relay . ((enabled . #t)
                          (admin-only . #t)))
                (encryption . ((allow . ,(mautrix-whatsapp-configuration-encryption? config))
                               (default . ,(mautrix-whatsapp-configuration-encryption? config))))))
     (logging . ((directory . ,(string-append (mautrix-whatsapp-configuration-log-directory config)
                                              "/logs"))
                 (file-name-format . "{{.Date}}-{{.Index}}.log")
                 (file-date-format . "2006-01-02")
                 (file-mode . 0384)
                 (timestamp-format . "Jan _2, 2006 15:04:05")
                 (print-level . debug))))))

(define (mautrix-whatsapp-file config)
  (mixed-text-file
   "config.yaml"
   #~(string-append
      #$@(mautrix-whatsapp-fill-defaults config)
      #$@(serialize-yaml-config (mautrix-whatsapp-configuration-extra-config config)))))

(define (mautrix-whatsapp-synapse-service config)
  (synapse-extension
   (extra-config
    `((app-service-config-files . #(,(string-append (mautrix-whatsapp-configuration-data-directory config)
                                                    "/registration.yaml")))))))

(define (mautrix-whatsapp-activation-service config)
  #~(begin
      (use-modules (guix build utils))

      (define %user (getpw "mautrix-whatsapp"))
      (define data-dir #$(mautrix-whatsapp-configuration-data-directory config))
      (define log-dir #$(mautrix-whatsapp-configuration-log-directory config))
      (define registration-file (string-append data-dir "/registration.yaml"))
      (define config-file (string-append data-dir "/config.yaml"))
      (define (generate-registration-file)
        (unless (stat registration-file #f)
          (copy-file #$(mautrix-whatsapp-file config) config-file)
          (system* #$(file-append (mautrix-whatsapp-configuration-mautrix-whatsapp config)
                                  "/bin/mautrix-whatsapp")
                   "--generate-registration"
                   "--config=" config-file
                   "--registration=" registration-file)))

      (mkdir-p data-dir)
      (chown data-dir (passwd:uid %user) (passwd:gid %user))
      (chmod data-dir #o700)
      (mkdir-p log-dir)
      (chown log-dir (passwd:uid %user) (passwd:gid %user))
      (chmod log-dir #o700)
      (generate-registration-file)
      (chmod registration-file #o640)
      (chown config-file (passwd:uid %user) (passwd:gid %user))))

(define (mautrix-whatsapp-shepherd-service config)
  (list
   (shepherd-service
    (provision '(mautrix-whatsapp))
    (requirement '(synapse))
    (start #~(make-forkexec-constructor
              (list
               (string-append #$(mautrix-whatsapp-configuration-mautrix-whatsapp config)
                              "/bin/mautrix-whatsapp")
               "--config="
               (string-append #$(mautrix-whatsapp-configuration-data-directory config)
                              "/config.yaml")
               "--registration="
               (string-append #$(mautrix-whatsapp-configuration-data-directory config)
                              "/registration.yaml"))
              #:user "mautrix-whatsapp"
              #:group "mautrix-whatsapp"
              #:log-file "/var/log/mautrix-whatsapp/mautrix-whatsapp.log"))
    (stop #~(make-kill-destructor)))))

(define (mautrix-whatsapp-profile-service config)
  (list (mautrix-whatsapp-configuration-mautrix-whatsapp config)))

(define mautrix-whatsapp-service-type
  (service-type
   (name 'mautrix-whatsapp)
   (extensions
    (list
     (service-extension
      account-service-type
      (const %mautrix-whatsapp-accounts))
     (service-extension
      postgresql-role-service-type
      mautrix-whatsapp-postgresql-service)
     (service-extension
      profile-service-type
      mautrix-whatsapp-profile-service)
     (service-extension
      synapse-service-type
      mautrix-whatsapp-synapse-service)
     (service-extension
      activation-service-type
      mautrix-whatsapp-activation-service)
     (service-extension
      shepherd-root-service-type
      mautrix-whatsapp-shepherd-service)))
   (default-value (mautrix-whatsapp-configuration))
   (description "System service for the Matrix-WhatsApp puppetting bridge. Do note that
changing any of the values in @code{mautrix-whatsapp-configuration} requires regeneration
of the registration, which you may do by removing @file{/var/lib/mautrix-whatsapp/registration.yaml}
and restarting the service.")))
