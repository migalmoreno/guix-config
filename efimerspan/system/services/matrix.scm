(define-module (efimerspan system services matrix)
  #:use-module (efimerspan serializers yaml)
  #:use-module (efimerspan system services databases)
  #:use-module (efimerspan packages matrix)
  #:use-module (gnu packages matrix)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:export (synapse-configuration
            synapse-service-type))

(define (maybe-string? x)
  (or (string? x) (not x)))

(define-maybe/no-serialization maybe-string)

(define-configuration/no-serialization synapse-configuration
  (package
    (package synapse-next)
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
  (postgresql-db?
   (boolean #f)
   "Whether to use a PostgreSQL database for storage.")
  (postgresql-db-password
   (maybe-string "")
   "The password for the PostgreSQL database user. Do note this will be exposed in a file under /gnu/store
in plain sight, so always prefer to set this manually.")
  (max-upload-size
   (string "50M")
   "The largest allowed upload size in bytes.")
  (data-directory
   (string "/var/lib/matrix-synapse")
   "Indicates the path where data such as the media store and database files should be
stored.")
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
configuration to be placed under @file{homeserver.yaml}."))

(define (synapse-shepherd-service config)
  "Returns a Shepherd service for Synapse."
  (list
   (shepherd-service
    (provision '(synapse))
    (requirement (if (synapse-configuration-postgresql-db? config)
                     '(postgres)
                     '()))
    (start #~(make-forkexec-constructor
              (list (string-append #$(synapse-configuration-package config)
                                   "/bin/synapse_homeserver")
                    "-c"
                    #$(synapse-file config)
                    "--config-directory"
                    #$(synapse-configuration-config-directory config)
                    "--data-directory"
                    #$(synapse-configuration-data-directory config))
              #:log-file "/var/log/matrix-synapse"
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
  (list (synapse-configuration-package config)))

(define (synapse-postgresql-service config)
  (when (synapse-configuration-postgresql-db? config)
    (list
     (postgresql-role
      (name "matrix-synapse")
      (create-database? #t)
      (password (synapse-configuration-postgresql-db-password config))
      (collation "C")
      (ctype "C")))))

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
          (system* #$(file-append (synapse-configuration-package config) "/bin/generate_signing_key")
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
   (default-value (synapse-configuration))
   (description "System service for Synapse, the Matrix flagship implementation.")))
