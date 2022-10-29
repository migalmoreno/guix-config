(define-module (conses system iso)
  #:use-module (conses utils)
  #:use-module (rde packages)
  #:use-module (rde features base)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (nongnu packages linux)
  #:export (%system-features))

(define %system-features
  (list
   (feature-base-packages
    #:system-packages
    (strings->packages
     "make" "gnupg"
     "emacs-no-x" "parted" "git"))
   (feature-custom-services
    #:system-services
    (cons*
     (simple-service
      'channels-etc-service
      etc-service-type
      `(("channels.scm"
         ,(project-file "channels"))))
     (operating-system-user-services installation-os)))))
