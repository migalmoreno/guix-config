(define-module (conses system cygnus)
  #:use-module (conses utils)
  #:use-module (conses system)
  #:use-module (conses system lyra)
  #:use-module (conses packages web)
  #:use-module (conses features matrix)
  #:use-module (conses features postgresql)
  #:use-module (rde features base)
  #:use-module (rde features system)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu services)
  #:use-module (gnu services ssh)
  #:use-module (gnu services networking)
  #:use-module (gnu services certbot)
  #:use-module (gnu bootloader)
  #:use-module (gnu packages ssh)
  #:use-module (guix gexp)
  #:export (%system-features))

(define %system-features
  (define %domain (getenv "DOMAIN"))

  (list
   (feature-kernel
    #:kernel %default-kernel
    #:firmware (list linux-firmware))
   (feature-host-info
    #:host-info "cygnus"
    #:timezone %default-timezone)
   (feature-base-services
    #:guix-authorized-keys
    (list %lyra-signing-key))
   (feature-postgresql)
   (feature-custom-services
    #:system-services
    (list
     (service dhcp-client-service-type)
     (service openssh-service-type
              (openssh-configuration
               (openssh openssh-sans-x)
               (password-authentication? #f)
               (permit-root-login 'prohibit-password)
               (authorized-keys `(("root" %lyra-ssh-key)))))))
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
   (feature-nginx
    #:nginx nginx-with-dav
    #:domain %domain)
   (feature-certbot
    #:email (string-append "contact@" %domain))
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
