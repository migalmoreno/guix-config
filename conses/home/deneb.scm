(define-module (conses home deneb)
  #:use-module (conses features matrix)
  #:use-module (conses features scheme)
  #:use-module (conses packages matrix)
  #:use-module (rde features base)
  #:use-module (gnu services)
  #:use-module (gnu services networking))

(define-public %home-features
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
    '("~/src/projects/fdroid.el"
      "~/src/projects/nyxt.el"
      "~/src/projects/dotfiles"
      "~/src/projects/tau"))))
