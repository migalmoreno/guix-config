(define-module (migalmoreno users vega)
  #:use-module (migalmoreno users vega desktop)
  #:use-module (migalmoreno users vega development)
  #:use-module (migalmoreno users vega emacs-xyz)
  #:use-module (migalmoreno users vega mail)
  #:use-module (migalmoreno users vega markup)
  #:use-module (migalmoreno users vega messaging)
  #:use-module (migalmoreno users vega multimedia)
  #:use-module (migalmoreno users vega web-browsers)
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
   emacs-features
   (desktop-features source palette)
   mail-features
   multimedia-features
   development-features
   messaging-features
   markup-features
   (nyxt-features source palette)))
