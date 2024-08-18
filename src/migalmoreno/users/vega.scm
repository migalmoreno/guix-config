(define-module (migalmoreno users vega)
  #:use-module ((migalmoreno users vega appearance) #:prefix appearance:)
  #:use-module ((migalmoreno users vega emacs-xyz) #:prefix emacs-xyz:)
  #:use-module ((migalmoreno users vega gtk) #:prefix gtk:)
  #:use-module ((migalmoreno users vega mail) #:prefix mail:)
  #:use-module ((migalmoreno users vega markup) #:prefix markup:)
  #:use-module ((migalmoreno users vega messaging) #:prefix messaging:)
  #:use-module ((migalmoreno users vega multimedia) #:prefix multimedia:)
  #:use-module ((migalmoreno users vega programming) #:prefix programming:)
  #:use-module ((migalmoreno users vega wayland) #:prefix wayland:)
  #:use-module ((migalmoreno users vega web-browsers) #:prefix web-browsers:)
  #:use-module (migalmoreno utils)
  #:use-module (rde features base))

(define-public (features source palette)
  (append
   (list
    (feature-user-info
     #:user-name "vega"
     #:full-name %default-fullname
     #:email %default-email
     #:user-groups
     '("wheel" "netdev" "audio" "video" "libvirt" "spice" "docker")
     #:rde-advanced-user? #t
     #:emacs-advanced-user? #t))
   emacs-xyz:features
   (wayland:features source palette)
   (appearance:features source palette)
   (gtk:features source palette)
   mail:features
   multimedia:features
   programming:features
   messaging:features
   markup:features
   (web-browsers:features source palette)))
