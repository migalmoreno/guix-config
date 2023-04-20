(define-module (dotfiles hosts cygnus)
  #:use-module (dotfiles utils)
  #:use-module (dotfiles hosts lyra)
  #:use-module (rde features base)
  #:use-module (rde features databases)
  #:use-module (rde features matrix)
  #:use-module (rde features system)
  #:use-module (rde features web)
  #:use-module (rde system services matrix)
  #:use-module (rde packages)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services certbot)
  #:use-module (gnu services networking)
  #:use-module (gnu services rsync)
  #:use-module (gnu services ssh)
  #:use-module (gnu services web)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (guix gexp))


;;; Host-specific utilities

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


;;; Service extensions

(define extra-nginx-config-service
  (simple-service
   'add-extra-nginx-configuration
   nginx-service-type
   (list
    (nginx-server-configuration
     (listen '("443 ssl http2"))
     (server-name (list %tubo-host))
     (ssl-certificate (string-append "/etc/letsencrypt/live/"
                                     %tubo-host "/fullchain.pem"))
     (ssl-certificate-key (string-append "/etc/letsencrypt/live/"
                                         %tubo-host "/privkey.pem"))
     (locations
      (list
       (nginx-location-configuration
        (uri "/")
        (body
         (list "proxy_pass http://localhost:3000;"
               "proxy_set_header X-Forwarded-For $remote_addr;"
               "proxy_set_header HOST $http_host;")))
       %letsencrypt-acme-challenge))))))

(define extra-system-packages
  (strings->packages "git" "rsync"))

(define extra-system-services
  (list
   extra-nginx-config-service
   (service rsync-service-type
            (rsync-configuration
             (modules
              (list
               (rsync-module
                (name "site")
                (file-name (string-append "/srv/http/" %default-domain)))))))
   (service dhcp-client-service-type)
   (service openssh-service-type
            (openssh-configuration
             (openssh (@ (gnu packages ssh) openssh-sans-x))
             (password-authentication? #f)
             (permit-root-login 'prohibit-password)
             (authorized-keys `(("root" ,%lyra-ssh-key ,%default-ssh-key)
                                ("deneb" ,%default-ssh-key)))))))



;;; Host-specific features

(define-public %host-features
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
    #:system-packages extra-system-packages)
   (feature-file-systems
    #:file-systems cygnus-file-systems
    #:swap-devices cygnus-swap-devices)
   (feature-custom-services
    #:system-services extra-system-services)
   (feature-web-settings
    #:primary-domain %default-domain)
   (feature-postgresql)
   (feature-nginx)
   (feature-certbot
    #:email %default-email)
   (feature-matrix-settings
    #:homeserver (string-append "https://matrix." %default-domain)
    #:synapse-configuration
    (synapse-configuration
     (server-name %default-domain)
     (enable-registration? #f)
     (public-base-url (string-append "https://matrix." %default-domain))
     (shared-secret (getenv "CYGNUS_SYNAPSE_SHARED_SECRET"))
     (postgresql-db? #t)
     (postgresql-db-password (getenv "CYGNUS_SYNAPSE_DB_PASSWORD")))
    #:mautrix-whatsapp-configuration
    (mautrix-whatsapp-configuration
     (domain %default-domain)
     (postgresql-db? #t)
     (postgresql-db-password (getenv "CYGNUS_MAUTRIX_WHATSAPP_DB_PASSWORD"))
     (encryption? #t)
     (permissions `((,%default-domain . user)
                    (,(string-append "@admin:" %default-domain) . admin)))))
   (feature-synapse
    #:whatsapp-bridge? #t)))
