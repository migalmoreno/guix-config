(define-module (conses users deneb)
  #:use-module (conses features matrix)
  #:use-module (conses features scheme)
  #:use-module (gnu services)
  #:use-module (gnu services networking)
  #:use-module (rde features base))


;;; User-specific features

(define-public %user-features
  (list
   (feature-user-info
    #:user-name "deneb"
    #:full-name (getenv "MAIL_PERSONAL_FULLNAME")
    #:email (getenv "MAIL_PERSONAL_EMAIL"))
   (feature-desktop-services
    #:default-desktop-system-services
    (modify-services (@@ (rde features base) %rde-desktop-system-services)
      (delete network-manager-service-type)))
   (feature-pantalaimon
    #:pantalaimon pantalaimon-next
    #:ignore-device-verification? #t
    #:ssl? #f)
   (feature-guix
    #:authorized-directories
    (list "/home/deneb/src/tubo"))))
