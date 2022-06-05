(define-module (quasar home packages emacs-xyz)
  #:use-module (efimerspan packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix build-system)
  #:use-module (guix build-system emacs)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages emacs-xyz)
  #:use-module ((guix licenses) #:prefix license:))

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
           emacs-ement
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
