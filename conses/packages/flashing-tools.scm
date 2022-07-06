(define-module (conses packages flashing-tools)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:))

(define-public pmbootstrap
  (package
    (name "pmbootstrap")
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pmbootstrap" version))
       (sha256
        (base32 "1cxycx0wcgs5lhsfisdrvbvhd46i2baz0r7iixzngqv98h8lvzvg"))))
    (build-system python-build-system)
    (home-page "https://gitlab.com/postmarketOS/pmbootstrap")
    (synopsis
     "A sophisticated chroot / build / flash tool to develop and install postmarketOS.")
    (description
     "This package provides a sophisticated chroot / build / flash tool to develop and
install postmarketOS.")
    (license license:asl2.0)))
