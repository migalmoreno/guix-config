(define-module (migalmoreno hosts cygnus)
  #:use-module (migalmoreno hosts lyra)
  #:use-module (migalmoreno utils)
  #:use-module (rde features base)
  #:use-module (rde features databases)
  #:use-module (rde features docker)
  #:use-module (rde features system)
  #:use-module (rde system services matrix)
  #:use-module (rde packages)
  #:use-module (gnu packages version-control)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services certbot)
  #:use-module (gnu services cgit)
  #:use-module (gnu services docker)
  #:use-module (gnu services networking)
  #:use-module (gnu services rsync)
  #:use-module (gnu services ssh)
  #:use-module (gnu services version-control)
  #:use-module (gnu services web)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-26))

(define cygnus-file-systems
  (list
   (file-system
     (mount-point "/")
     (device (file-system-label "root"))
     (type "ext4"))))

(define cygnus-swap-devices
  (list
   (swap-space
    (target (file-system-label "swap")))))

(define cygnus-extra-packages
  (strings->packages "git" "rsync" "docker-compose"))

(define cygnus-nginx-services
  (list
   (service nginx-service-type
            (nginx-configuration
             (server-blocks
              (list
               (nginx-server-configuration
                (listen '("443 ssl http2"))
                (server-name
                 (list %default-domain
                       (string-append "www." %default-domain)))
                (root (string-append "/srv/http/" %default-domain))
                (ssl-certificate
                 (format #f "/etc/letsencrypt/live/~a/fullchain.pem"
                         %default-domain))
                (ssl-certificate-key
                 (format #f "/etc/letsencrypt/live/~a/privkey.pem"
                         %default-domain))
                (raw-content (list "error_page 404 = /404.html;"))
                (locations
                 (append
                  (list
                   (nginx-location-configuration
                    (uri "/404.html")
                    (body
                     (list "internal;"))))
                  (list %letsencrypt-acme-challenge))))))))
   (simple-service
    'add-extra-nginx-configuration
    nginx-service-type
    (list
     (nginx-server-configuration
      (listen '("443 ssl http2"))
      (server-name (list %tubo-host))
      (ssl-certificate (format #f "/etc/letsencrypt/live/~a/fullchain.pem"
                               %tubo-host))
      (ssl-certificate-key (format #f "/etc/letsencrypt/live/~a/privkey.pem"
                                   %tubo-host ))
      (locations
       (list
        (nginx-location-configuration
         (uri "/")
         (body
          (list "proxy_pass http://localhost:3000;"
                "proxy_set_header X-Forwarded-For $remote_addr;"
                "proxy_set_header HOST $http_host;")))
        %letsencrypt-acme-challenge)))))))

(define cygnus-certbot-services
  (list
   (service certbot-service-type
            (certbot-configuration
             (email %default-email)
             (webroot "/srv/http")
             (certificates
              (list
               (certificate-configuration
                (domains (list %default-domain
                               (string-append "www." %default-domain)))
                (deploy-hook %nginx-deploy-hook))))))
   (simple-service
    'add-extra-ssl-certificates
    certbot-service-type
    (list
     (certificate-configuration
      (domains (list %tubo-host))
      (deploy-hook %nginx-deploy-hook))))))

(define cygnus-matrix-services
  (list
   (service synapse-service-type
            (synapse-configuration
             (server-name "conses.eu")
             (enable-registration? #f)
             (public-base-url "https://matrix.conses.eu")
             (shared-secret (getenv "CYGNUS_SYNAPSE_SHARED_SECRET"))
             (postgresql-db? #t)
             (postgresql-db-password (getenv "CYGNUS_SYNAPSE_DB_PASSWORD"))))
   (service mautrix-whatsapp-service-type
            (mautrix-whatsapp-configuration
             (domain "conses.eu")
             (postgresql-db? #t)
             (postgresql-db-password
              (getenv "CYGNUS_MAUTRIX_WHATSAPP_DB_PASSWORD"))
             (encryption? #t)
             (permissions `(("conses.eu" . user)
                            ("@admin:conses.eu" . admin)))))
   (simple-service
    'add-matrix-nginx-configuration
    nginx-service-type
    (list
     (nginx-server-configuration
      (listen '("443 ssl http2"))
      (server-name (list "conses.eu" "www.conses.eu"))
      (ssl-certificate "/etc/letsencrypt/live/conses.eu/fullchain.pem")
      (ssl-certificate-key "/etc/letsencrypt/live/conses.eu/privkey.pem")
      (locations
       (list
        (nginx-location-configuration
         (uri "/.well-known/matrix/server")
         (body
          (list "default_type application/json;"
                "return 200 '{\"m.server\": \"matrix.conses.eu:443\"}';"
                "add_header Access-Control-Allow-Origin *;"))))))
     (nginx-server-configuration
      (listen '("443 ssl http2"
                "[::]:443 ssl http2"
                "8448 ssl http2 default_server"
                "[::]:8448 ssl http2 default_server"))
      (server-name (list "matrix.conses.eu"))
      (ssl-certificate
       "/etc/letsencrypt/live/matrix.conses.eu/fullchain.pem")
      (ssl-certificate-key
       "/etc/letsencrypt/live/matrix.conses.eu/privkey.pem")
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
        %letsencrypt-acme-challenge)))
     (nginx-server-configuration
      (listen '("443 ssl http2"
                "[::]:443 ssl http2"))
      (server-name (list "pantalaimon.conses.eu"))
      (ssl-certificate
       "/etc/letsencrypt/live/pantalaimon.conses.eu/fullchain.pem")
      (ssl-certificate-key
       "/etc/letsencrypt/live/pantalaimon.conses.eu/privkey.pem")
      (locations
       (list
        (nginx-location-configuration
         (uri "/")
         (body
          (list "proxy_pass http://localhost:8009;"
                "proxy_set_header X-Forwarded-For $remote_addr;"
                "proxy_set_header HOST $http_host;")))
        %letsencrypt-acme-challenge)))))
   (simple-service
    'add-matrix-ssl-certificates
    certbot-service-type
    (map
     (lambda (domain)
       (certificate-configuration
        (domains (list domain))
        (deploy-hook %nginx-deploy-hook)))
     (append
      (list "conses.eu")
      (map (cut string-append <> ".conses.eu")
           (list "pantalaimon" "matrix")))))))

(define cygnus-whoogle-services
  (list
   (service whoogle-service-type)
   (simple-service
    'add-whoogle-nginx-configuration
    nginx-service-type
    (list
     (nginx-server-configuration
      (listen '("443 ssl http2"))
      (server-name (list (string-append "whoogle." %default-domain)))
      (ssl-certificate
       (format #f "/etc/letsencrypt/live/whoogle.~a/fullchain.pem"
               %default-domain))
      (ssl-certificate-key
       (format #f "/etc/letsencrypt/live/whoogle.~a/privkey.pem"
               %default-domain))
      (locations
       (list
        (nginx-location-configuration
         (uri "/")
         (body
          (list "proxy_pass http://localhost:5000/;"
                "proxy_set_header X-Forwarded-For $remote_addr;"
                "proxy_set_header HOST $http_host;")))
        %letsencrypt-acme-challenge)))))
   (simple-service
    'add-whoogle-ssl-certificate
    certbot-service-type
    (list
     (certificate-configuration
      (domains (list (string-append "whoogle." %default-domain)))
      (deploy-hook %nginx-deploy-hook))))))

(define cygnus-file-server-services
  (list
   (simple-service
    'add-file-server-nginx-configuration
    nginx-service-type
    (list
     (nginx-server-configuration
      (listen '("443 ssl http2"))
      (server-name (list (string-append "files." %default-domain)))
      (root "/srv/http/files")
      (ssl-certificate
       (format #f "/etc/letsencrypt/live/files.~a/fullchain.pem"
               %default-domain))
      (ssl-certificate-key
       (format #f "/etc/letsencrypt/live/files.~a/privkey.pem"
               %default-domain))
      (raw-content '("autoindex on;"))
      (locations
       (list %letsencrypt-acme-challenge)))))
   (simple-service
    'add-file-server-ssl-certificate
    certbot-service-type
    (list
     (certificate-configuration
      (domains (list (string-append "files." %default-domain)))
      (deploy-hook %nginx-deploy-hook))))))

(define cygnus-version-control-services
  (list
   (service fcgiwrap-service-type
            (fcgiwrap-configuration
             (group "git")))
   (service gitolite-service-type
            (gitolite-configuration
             (admin-pubkey %default-ssh-key)
             (rc-file
              (gitolite-rc-file
               (umask #o0027)
               (git-config-keys "gitweb\\..*")))))
   (service
    cgit-service-type
    (cgit-configuration
     (project-list "/var/lib/gitolite/projects.list")
     (section-from-path 1)
     (repository-directory "/var/lib/gitolite/repositories")
     (repository-sort "name")
     (case-sensitive-sort? #f)
     (root-desc (string-append %default-fullname "'s Git repositories"))
     (enable-git-config? #t)
     (enable-index-links? #t)
     (enable-index-owner? #f)
     (enable-commit-graph? #t)
     (enable-log-filecount? #t)
     (enable-log-linecount? #t)
     (readme ":README.md")
     (remove-suffix? #t)
     (clone-url
      (list (format #f "https://git.~a/$CGIT_REPO_URL " %default-domain)))
     (about-filter
      (program-file
       "cgit-about-formatting"
       #~(apply execl (string-append
                       #$cgit "/lib/cgit/filters/about-formatting.sh")
                (command-line))))
     (source-filter
      (program-file
       "cgit-syntax-highlighting"
       #~(apply execl (string-append
                       #$cgit "/lib/cgit/filters/syntax-highlighting.py")
                (command-line))))
     (nginx
      (list
       (nginx-server-configuration
        (listen '("443 ssl http2"))
        (server-name (list (string-append "git." %default-domain)))
        (root cgit)
        (ssl-certificate
         (format #f "/etc/letsencrypt/live/git.~a/fullchain.pem"
                 %default-domain))
        (ssl-certificate-key
         (format #f "/etc/letsencrypt/live/git.~a/privkey.pem"
                 %default-domain))
        (try-files (list "$uri" "@cgit"))
        (locations
         (list
          (nginx-location-configuration
           (uri "@cgit")
           (body
            (list
             "fastcgi_param SCRIPT_FILENAME $document_root/lib/cgit/cgit.cgi;"
             "fastcgi_param PATH_INFO $uri;"
             "fastcgi_param QUERY_STRING $args;"
             "fastcgi_param HTTP_HOST $server_name;"
             "fastcgi_pass 127.0.0.1:9000;"))))))))))
   (simple-service
    'add-cgit-ssl-certificate
    certbot-service-type
    (list
     (certificate-configuration
      (domains (list (string-append "git." %default-domain)))
      (deploy-hook %nginx-deploy-hook))))))

(define cygnus-extra-services
  (append
   (list
    (service dhcp-client-service-type)
    (service rsync-service-type
             (rsync-configuration
              (modules
               (list
                (rsync-module
                 (name "site")
                 (file-name (string-append "/srv/http/" %default-domain)))))))
    (service openssh-service-type
             (openssh-configuration
              (openssh (@ (gnu packages ssh) openssh-sans-x))
              (password-authentication? #f)
              (permit-root-login 'prohibit-password)
              (authorized-keys `(("root" ,%lyra-ssh-key ,%default-ssh-key)
                                 ("deneb" ,%default-ssh-key))))))
   cygnus-nginx-services
   cygnus-certbot-services
   cygnus-matrix-services
   cygnus-whoogle-services
   cygnus-file-server-services
   cygnus-version-control-services))

(define-public features
  (list
   (feature-host-info
    #:host-name "cygnus"
    #:timezone %default-timezone)
   (feature-base-services
    #:guix-authorized-keys
    (list %lyra-signing-key))
   (feature-bootloader
    #:bootloader-configuration
    (bootloader-configuration
     (bootloader grub-bootloader)
     (targets '("/dev/vda"))
     (terminal-outputs '(console))))
   (feature-base-packages
    #:base-home-packages '()
    #:system-packages cygnus-extra-packages)
   (feature-file-systems
    #:file-systems cygnus-file-systems
    #:swap-devices cygnus-swap-devices)
   (feature-custom-services
    #:system-services cygnus-extra-services)
   (feature-docker)
   (feature-postgresql)))
