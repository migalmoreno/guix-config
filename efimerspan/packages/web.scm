(define-module (efimerspan packages web)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages web))

(define-public nginx-with-dav
  (package
    (inherit nginx)
    (name "nginx-with-dav")
    (arguments
     (substitute-keyword-arguments (package-arguments nginx)
         ((#:configure-flags configure-flags)
          #~(cons* "--with-http_dav_module"
                   #$configure-flags))))))
