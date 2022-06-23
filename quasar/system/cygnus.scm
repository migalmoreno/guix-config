(define-module (quasar system cygnus)
  #:use-module (quasar home)
  #:use-module (efimerspan packages web)
  #:use-module (efimerspan system services web)
  #:use-module (efimerspan system services matrix)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services ssh)
  #:use-module (gnu services networking)
  #:use-module (gnu services certbot)
  #:use-module (gnu services web)
  #:use-module (gnu services databases)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages certs)
  #:use-module (guix gexp)
  #:export (%system/cygnus))

(define %letsencrypt-dir "/etc/letsencrypt/live")
(define %letsencrypt-acme-challenge
  (nginx-location-configuration
   (uri "/.well-known")
   (body '("root /srv/http;"))))
(define %domain (getenv "DOMAIN"))

(define %nginx-deploy-hook
  (program-file
   "nginx-deploy-hook"
   #~(let ((pid (call-with-input-file "/var/run/nginx/pid" read)))
       (kill pid SIGHUP))))

(define %cygnus-nginx-service
  (service nginx-service-type
           (nginx-configuration
            (nginx nginx-with-dav)
            (server-blocks
             (list
              (nginx-server-configuration
               (listen '("443 ssl http2"))
               (server-name (list %domain (string-append "www." %domain)))
               (root (string-append "/srv/http/" %domain))
               (ssl-certificate (string-append %letsencrypt-dir "/" %domain "/fullchain.pem"))
               (ssl-certificate-key (string-append %letsencrypt-dir "/" %domain "/privkey.pem"))
               (locations
                (list
                 (nginx-location-configuration
                  (uri "/webdav")
                  (body
                   (list "root /srv/http/dav;"
                         "client_body_temp_path /srv/client_temp;"
                         "dav_methods PUT DELETE MKCOL COPY MOVE;"
                         "create_full_put_path on;"
                         "dav_access group:rw all:r;")))
                 %letsencrypt-acme-challenge
                 (nginx-location-configuration
                  (uri "/.well-known/matrix/server")
                  (body
                   (list "default_type application/json;"
                         (format #f "return 200 '{\"m.server\": \"matrix.~a:443\"}';" %domain)
                         "add_header Access-Control-Allow-Origin *;"))))))
              (nginx-server-configuration
               (listen '("443 ssl http2"))
               (server-name (list (string-append "whoogle." %domain)))
               (ssl-certificate (string-append %letsencrypt-dir "/" %domain "/fullchain.pem"))
               (ssl-certificate-key (string-append %letsencrypt-dir "/" %domain "/privkey.pem"))
               (locations
                (list
                 (nginx-location-configuration
                  (uri "/")
                  (body
                   (list "proxy_pass http://localhost:5000/;"
                         "proxy_set_header X-Forwarded-For $remote_addr;"
                         "proxy_set_header HOST $http_host;")))
                 %letsencrypt-acme-challenge)))
              (nginx-server-configuration
               (listen '("443 ssl http2"
                         "[::]:443 ssl http2"
                         "8448 ssl http2 default_server"
                         "[::]:8448 ssl http2 default_server"))
               (server-name (list (string-append "matrix." %domain)))
               (ssl-certificate (string-append %letsencrypt-dir "/" %domain "/fullchain.pem"))
               (ssl-certificate-key (string-append %letsencrypt-dir "/" %domain "/privkey.pem"))
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
                 %letsencrypt-acme-challenge))))))))

(define %cygnus-services
  (append
   (list
    %cygnus-nginx-service
    (service postgresql-service-type)
    (service synapse-service-type
             (synapse-configuration
              (server-name %domain)
              (enable-registration? #f)
              (public-base-url (string-append "https://matrix." %domain))
              (shared-secret (getenv "CYGNUS_SYNAPSE_SHARED_SECRET"))
              (postgresql-db? #t)
              (postgresql-db-password (getenv "CYGNUS_SYNAPSE_DB_PASSWORD"))))
    (service dhcp-client-service-type)
    (service certbot-service-type
             (certbot-configuration
              (email (string-append "contact@" %domain))
              (webroot "/srv/http")
              (certificates
               (list
                (certificate-configuration
                 (name %domain)
                 (domains (list %domain
                                (string-append "wwww." %domain)
                                (string-append "whoogle." %domain)
                                (string-append "matrix." %domain)))
                 (deploy-hook %nginx-deploy-hook))))))
    (service openssh-service-type
             (openssh-configuration
              (openssh openssh-sans-x)
              (password-authentication? #f)
              (permit-root-login 'prohibit-password)
              (authorized-keys
               `(("root" ,(local-file "../../keys/ssh/lyra.pub"))))))
    (service whoogle-service-type))
   (modify-services %base-services
     (guix-service-type config =>
                        (guix-configuration
                         (inherit config)
                         (authorized-keys
                          (append
                           (list
                            (local-file "../../keys/signatures/lyra.pub"))
                           %default-authorized-guix-keys)))))))

(define %system/cygnus
  (operating-system
   (host-name "cygnus")
   (timezone (getenv "CYGNUS_TIMEZONE"))
   (keyboard-layout (keyboard-layout "us"))
   (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets '("/dev/vda"))
                (terminal-outputs '(console))
                (keyboard-layout keyboard-layout)))
   (swap-devices
    (list
     (swap-space
      (target (file-system-label "swap")))))
   (file-systems
    (cons*
     (file-system
      (mount-point "/")
      (device (file-system-label "root"))
      (type "ext4"))
     %base-file-systems))
   (packages
    (append
     (list
      nss-certs)
     %base-packages))
   (services %cygnus-services)))

%system/cygnus
