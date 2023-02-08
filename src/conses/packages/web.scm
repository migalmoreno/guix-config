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

(define-public jellyfin
  (package
    (name "jellyfin")
    (version "10.8.7")
    (source
     (origin
       (method git-fetch)
       (uri "https://github.com/jellyfin/jellyfin")
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pcq06m9kjd4ycr8prhdihma26hk3vdz5m6j020bf4pcv5jpg2hf"))))
    (build-system trivial-build-system)
    (build-inputs
     (list dotnet))
    (home-page "https://github.com/jellyfin/jellyfin")
    (synopsis "Free Software Media System")
    (description "Jellyfin is a Free Software Media System that
puts you in control of managing and streaming your media.")
    (license license:gpl2)))
