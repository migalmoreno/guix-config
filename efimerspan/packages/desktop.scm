(define-module (efimerspan packages desktop)
  #:use-module (efimerspan packages python-xyz)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:))

(define-public bibata-cursor-theme
  (package
    (name "bibata-cursor-theme")
    (version "1.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ful1e5/Bibata_Cursor")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qksaxph5jnp1wsv54ad1n4rc6xwai3rxpkhhby7vpimk2qbiaf0"))))
    (build-system gnu-build-system)
    (inputs
     (list libxcursor libpng))
    (native-inputs
     (list python-clickgen))
    (home-page "https://github.com/ful1e5/Bibata_Cursor")
    (synopsis "Open-source, compact, and material-designed cursor set.")
    (description "Bibata is a open-source, compact, and material designed cursor set. This project
aims at improving the cursor experience.")
    (license license:gpl3)))
