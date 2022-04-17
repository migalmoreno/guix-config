(define-module (franesio packages hardware)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages linux)
  #:use-module (guix packages))

(define-public i2c-tools-4.3
  (package
    (inherit i2c-tools)
    (version "4.3")
    (source
     (origin
       (inherit (package-source i2c-tools))
       (uri
        (string-append "http://jdelvare.nerim.net/mirror/i2c-tools/i2c-tools-" version ".tar.xz"))
       (sha256
        (base32 "1y0fphjd5ah2j886x8i175r7viq0hmx666hyca0wi4dzrm290qxk"))))))

(define-public ddcutil-1.2.0
  (package
    (inherit ddcutil)
    (version "1.2.0")
    (source
     (origin
       (inherit (package-source ddcutil))
       (uri (string-append "https://www.ddcutil.com/tarballs/"
                           "ddcutil-" version ".tar.gz"))
       (sha256
        (base32 "1d3ig2iqj4xf8ry8b3xz06f73jwa1akbr0mxvmynklj0lvvnwgdv"))))
    (inputs
        (modify-inputs (package-inputs ddcutil)
            (append i2c-tools)))))
