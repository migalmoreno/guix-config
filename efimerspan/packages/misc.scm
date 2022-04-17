(define-module (efimerspan packages misc)
  #:use-module (gnu build-system trivial))

(define-public emacs-meditate-logo
  (package
    (name "emacs-meditate-logo")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://www.gnu.org/graphics/meditate.png")
       (sha256 (base32 "1i8cb417hzm0ly5mi2m7ccf0qdpamqdgbrs7h33xhblfjx282pvg"))))
    (build-system trivial-build-system)))
