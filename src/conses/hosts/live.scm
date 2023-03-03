(define-module (conses hosts live)
  #:use-module (rde packages)
  #:use-module (rde features base)
  #:use-module (gnu system)
  #:use-module (gnu system install))


;;; Host-specific features

(define-public %host-features
  (list
   (feature-base-packages
    #:system-packages
    (strings->packages
     "make" "gnupg" "curl" "cryptsetup"
     "emacs-no-x" "parted" "git" "dosfstools"))
   (feature-custom-services
    #:system-services
    (list
     (operating-system-user-services installation-os)))))
