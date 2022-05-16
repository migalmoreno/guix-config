(define-module (efimerspan home-services emacs)
  #:use-module (gnu services)
  #:use-module (gnu home-services emacs)
  #:export (elisp-configuration-service))

(define* (elisp-configuration-service #:optional (elisp-expressions '())
                                      #:key
                                      (elisp-packages '())
                                      (early-init '()))
  (simple-service
   (gensym "elisp-configuration-service")
   home-emacs-service-type
   (home-emacs-extension
    (init-el `(,@elisp-expressions))
    (early-init-el `(,@early-init))
    (elisp-packages elisp-packages))))
