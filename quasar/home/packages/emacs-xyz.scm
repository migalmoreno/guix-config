(define-module (quasar home packages emacs-xyz)
  #:use-module (conses packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix build-system)
  #:use-module (guix build-system emacs)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-al-file
  (package
    (name "emacs-al-file")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://raw.githubusercontent.com/alezost/emacs-config/master/utils/al-file.el")
       (sha256
        (base32 "01gj6z2d92li9l77h25ag7njgvalrdj2184fhjhxpbn7xg8z68h4"))))
    (build-system emacs-build-system)
    (home-page "https://raw.githubusercontent.com/alezost/emacs-config/master/utils/al-file.el")
    (synopsis "Additional functionality for working with files")
    (description #f)
    (license license:gpl3+)))

(define-public emacs-al-autoload
  (package
    (name "emacs-al-autoload")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://raw.githubusercontent.com/alezost/emacs-config/master/utils/al-autoload.el")
       (sha256
        (base32 "0f2r9g4k4fx9i55msd1d5fnnrhbjllx2nj2bg996yz9gc5vxs94w"))))
    (build-system emacs-build-system)
    (home-page "https://raw.githubusercontent.com/alezost/emacs-config/master/utils/al-autoload.el")
    (synopsis "Additional functionality to autoload Emacs packages")
    (description #f)
    (license license:gpl3+)))

(define-public emacs-al-guix-autoload
  (package
    (name "emacs-al-guix-autoload")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://raw.githubusercontent.com/alezost/emacs-config/master/utils/al-guix-autoload.el")
       (sha256
        (base32 "1qwhciqzybdd8acsvpi2b5ypqhn3l15nbnd1q04liqwainxhkyzf"))))
    (build-system emacs-build-system)
    (propagated-inputs (list emacs-al-autoload emacs-al-file))
    (home-page "https://github.com/alezost/emacs-config/blob/master/utils/al-guix-autoload.el")
    (synopsis "Additional functionality to autoload Guix packages")
    (description #f)
    (license license:gpl3+)))

(define-public emacs-elibs
  (package
    (name "emacs-elibs")
    (version "0.1.0")
    (source
     (local-file "../../../etc/elibs" #:recursive? #t))
    (build-system emacs-build-system)
    (propagated-inputs
     (list emacs-consult
           emacs-org
           emacs-org-roam
           emacs-org-superstar
           emacs-geiser
           emacs-geiser-guile
           emacs-all-the-icons
           emacs-sly
           emacs-modus-themes
           emacs-dashboard
           emacs-pdf-tools
           emacs-erc-status-sidebar
           emacs-emms
           emacs-ytdl
           emacs-mpv-next
           emacs-tuareg
           ocaml-merlin
           emacs-ednc
           emacs-exwm
           emacs-webpaste
           emacs-ement-next
           emacs-telega
           emacs-password-store
           emacs-dashboard
           emacs-hexrgb))
    (home-page "https://git.sr.ht/~mt08gh/quasar")
    (synopsis "Various Emacs Lisp libraries.")
    (description "A set of Emacs Lisp libraries to enhance the functionality
 of some Emacs packages and built-in features.")
    (license license:gpl3+)))

(define-public emacs-fdroid
  (let ((commit "2b1b198644241d4fb3fa")
        (revision "0"))
    (package
      (name "emacs-fdroid")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://git.sr.ht/~mt08gh/fdroid.el")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0r27pxyaz10v98hskfwfyjvdzfh77n9g667cdsv6cx3vql692w5s"))))
      (build-system emacs-build-system)
      (propagated-inputs
       (list emacs-consult emacs-embark))
      (home-page "https://git.sr.ht/~mt08gh/fdroid.el")
      (synopsis "An Emacs interface to manage F-Droid repositories.")
      (description "fdroid.el is an Emacs interface to fdroidcl. Its purpose is to aid the
 management of F-Droid repository packages to be installed in an Android device from the comfort of Emacs.")
      (license license:gpl3+))))

(define-public emacs-mpv-next
  (let ((branch "feature-listing-commands")
        (commit "331aeb6e266447e9bc4195e2858123ac76d3fd89"))
    (package
      (inherit emacs-mpv)
      (name "emacs-mpv-next")
      (version (git-version "1" branch commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/efimerspan/mpv.el")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0m8408xpbbbvs3isbws548yxdykz8qgc73hbna0yan694zx4a30s"))))
      (propagated-inputs
       (modify-inputs (package-inputs emacs-mpv)
         (append emacs-consult))))))
