(define-module (conses packages flashing-tools)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages check)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:))

(define-public pmbootstrap
  (package
    (name "pmbootstrap")
    (version "1.49.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pmbootstrap" version))
       (sha256
        (base32 "07hj7srwwach00l7fkra53s66dsxl6l2264jqv3d0681h113fmc7"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f))
    (native-inputs
     (list python-wheel
           python-pytest))
    (inputs
     (list sudo openssl procps))
    (home-page "https://gitlab.com/postmarketOS/pmbootstrap")
    (synopsis
     "A sophisticated chroot / build / flash tool to develop and install postmarketOS.")
    (description
     "This package provides a sophisticated chroot / build / flash tool to develop and
install postmarketOS.")
    (license license:asl2.0)))
