(define-module (conses users deneb)
  #:use-module (conses hosts base)
  #:use-module (gnu services)
  #:use-module (gnu services networking)
  #:use-module (rde features base)
  #:use-module (rde features matrix)
  #:use-module (rde features scheme))


;;; User-specific features

(define-public %user-features
  (list
   (feature-user-info
    #:user-name "deneb"
    #:full-name %default-fullname
    #:email %default-email)
   (feature-desktop-services
    #:default-desktop-system-services
    (modify-services (@@ (rde features base) %rde-desktop-system-services)
      (delete network-manager-service-type)))
   (feature-pantalaimon
    #:pantalaimon pantalaimon-next
    #:ignore-device-verification? #t
    #:ssl? #f)
   (feature-guix
    #:shell-authorized-directories
    (list "/home/deneb/src/tubo"))))
