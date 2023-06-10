(define-module (dotfiles users deneb)
  #:use-module (dotfiles utils)
  #:use-module (gnu services)
  #:use-module (gnu services networking)
  #:use-module (rde features base)
  #:use-module (rde features matrix))

(define-public %deneb-features
  (list
   (feature-user-info
    #:user-name "deneb"
    #:full-name %default-fullname
    #:email %default-email)
   (feature-desktop-services
    #:default-desktop-system-services
    (modify-services (@@ (rde features base) %rde-desktop-system-services)
      (delete network-manager-service-type)))
   (feature-matrix-settings
    #:homeserver "https://matrix.conses.eu")
   (feature-pantalaimon
    #:ignore-device-verification? #t
    #:ssl? #f)))
