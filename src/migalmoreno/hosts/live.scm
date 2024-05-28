(define-module (migalmoreno hosts live)
  #:use-module (migalmoreno utils)
  #:use-module (rde features base)
  #:use-module (rde features system)
  #:use-module (rde packages)
  #:use-module (rde system services admin)
  #:use-module (rde system services guix)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system install))

(define live-extra-services
  (list
   (simple-service
    'sudoers-extra-service
    sudoers-service-type
    (list "%wheel ALL=(ALL) NOPASSWD: ALL"))
   (service cow-store-service-type)))

(define-public features
  (list
   (feature-host-info
    #:host-name "live"
    #:timezone %default-timezone)
   (feature-base-packages
    #:base-home-packages '()
    #:system-packages
    (strings->packages
     "make" "gnupg" "curl" "cryptsetup"
     "emacs-no-x" "parted" "git" "dosfstools"))
   (feature-file-systems
    #:file-systems (operating-system-file-systems installation-os))
   (feature-custom-services
    #:system-services live-extra-services)))
