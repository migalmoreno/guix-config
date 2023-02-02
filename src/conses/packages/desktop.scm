(define-module (conses packages desktop)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:))

(define-public bibata-cursor-theme
  (package
    (name "bibata-cursor-theme")
    (version "2.0.2")
    (source (origin
              (method url-fetch/tarbomb)
              (uri (string-append
                    "https://github.com/ful1e5/Bibata_Cursor/releases/download/v"
                    version "/Bibata.tar.gz"))
              (file-name "Bibata.tar.gz")
              (sha256
               (base32
                "0a6dr10hgd4g8cxi8ixbbkfzbf91hzds19yiffxdyyi2a6ic9pvh"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan `(("." "share/icons"))))
    (home-page "https://github.com/ful1e5/Bibata_Cursor")
    (synopsis "Open-source, compact, and material-designed cursor set")
    (description
     "Bibata is a open-source, compact, and material designed cursor set.  This project
aims at improving the cursor experience.")
    (license license:gpl3)))
