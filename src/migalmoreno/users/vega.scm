(define-module (migalmoreno users vega)
  #:use-module (migalmoreno utils)
  #:use-module (rde features base))

(define-public (features . r)
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
   (apply user-features "vega" r)))
