(define-module (conses packages web)
  #:use-module (conses packages python-xyz)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages web)
  #:use-module (gnu packages time)
  #:use-module (gnu packages check)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:))

(define-public nginx-with-dav
  (package
    (inherit nginx)
    (name "nginx-with-dav")
    (arguments
     (substitute-keyword-arguments (package-arguments nginx)
         ((#:configure-flags configure-flags)
          #~(cons* "--with-http_dav_module"
                   #$configure-flags))))))
