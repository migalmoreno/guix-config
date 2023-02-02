(define-module (conses hosts live)
  #:use-module (conses utils)
  #:use-module (rde packages)
  #:use-module (rde features base)
  #:use-module (gnu system)
  #:use-module (gnu system install)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (nongnu packages linux))


;;; Host-specific features

(define-public %host-features
  (list
   (feature-base-packages
    #:system-packages
    (strings->packages
     "make" "gnupg" "curl"
     "emacs-no-x" "parted" "git"))
   (feature-custom-services
    #:system-services
    (list
     (operating-system-user-services installation-os)))))
