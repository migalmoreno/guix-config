(define-module (conses packages desktop)
  #:use-module (conses packages python-xyz)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages image)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module ((guix licenses) #:prefix license:))

(define-public bibata-cursor-theme
  (let ((commit "8faa0631307c014e544bc857feed8c0300564d48")
        (revision "0"))
    (package
      (name "bibata-cursor-theme")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ful1e5/Bibata_Cursor")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1n6y8frpp0zayw8ya7y53wr0mnvam16y9hailx61v4jdij4krmkr"))))
      (build-system gnu-build-system)
      (inputs
       (list libxcursor
             libpng
             libx11))
      (native-inputs
       (list
        python
        python-setuptools
        python-wheel
        python-clickgen))
      (home-page "https://github.com/ful1e5/Bibata_Cursor")
      (synopsis "Open-source, compact, and material-designed cursor set.")
      (description "Bibata is a open-source, compact, and material designed cursor set. This project
aims at improving the cursor experience.")
      (license license:gpl3))))
