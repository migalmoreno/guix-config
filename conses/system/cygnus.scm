(define-module (conses system cygnus)
  #:use-module (conses utils)
  #:use-module (conses system)
  #:use-module (conses system lyra)
  #:use-module (conses system services matrix)
  #:use-module (conses packages web)
  #:use-module (conses packages matrix)
  #:use-module (conses features web)
  #:use-module (conses features matrix)
  #:use-module (conses features databases)
  #:use-module (rde features base)
  #:use-module (rde features system)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services ssh)
  #:use-module (gnu services networking)
  #:use-module (gnu services certbot)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages ssh)
  #:use-module (guix gexp))

(define %domain (getenv "DOMAIN"))

(define-public %system-features
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
   (feature-base-packages)
   (feature-file-systems
    #:file-systems
    (list
     (file-system
       (mount-point "/")
       (device (file-system-label "root"))
       (type "ext4")))
    #:swap-devices
    (list
     (swap-space
      (target (file-system-label "swap")))))
   (feature-custom-services
    #:system-services
    (list
     (service dhcp-client-service-type)
     (service openssh-service-type
              (openssh-configuration
               (openssh openssh-sans-x)
               (password-authentication? #f)
               (permit-root-login 'prohibit-password)
               (authorized-keys `(("root" ,%lyra-ssh-key ,%default-ssh-key)
                                  ("deneb" ,%default-ssh-key)))))))
   (feature-web-settings
    #:domain %domain)
   (feature-postgresql)
   (feature-nginx
    #:nginx nginx-with-dav)
   (feature-certbot
    #:email (getenv "MAIL_PERSONAL_MAIL"))
   (feature-whoogle)
   (feature-matrix-settings
    #:homeserver (string-append "https://matrix." %domain)
    #:synapse-configuration
    (synapse-configuration
     (synapse synapse-next)
     (server-name %domain)
     (enable-registration? #f)
     (public-base-url (string-append "https://matrix." %domain))
     (shared-secret (getenv "CYGNUS_SYNAPSE_SHARED_SECRET"))
     (postgresql-db? #t)
     (postgresql-db-password (getenv "CYGNUS_SYNAPSE_DB_PASSWORD")))
    #:mautrix-whatsapp-configuration
    (mautrix-whatsapp-configuration
     (domain %domain)
     (postgresql-db? #t)
     (postgresql-db-password (getenv "CYGNUS_MAUTRIX_WHATSAPP_DB_PASSWORD"))
     (encryption? #t)
     (permissions `((,%domain . user)
                    (,(string-append "@admin:" %domain) . admin)))))
   (feature-synapse
    #:whatsapp-bridge? #t)))
