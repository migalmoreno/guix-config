(define-module (conses packages gnome-xyz)
  #:use-module (gnu packages web)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages python)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages freedesktop)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module ((guix licenses) #:prefix license:))

(define-public postmarketos-theme
  (package
    (name "postmarketos-theme")
    (version "0.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/postmarketOS/postmarketos-theme")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09in7737cirmw2c0ac40ac29szfgdva6q0zl32mdi12marybd2g5"))))
    (build-system meson-build-system)
    (native-inputs (list sassc))
    (home-page "https://gitlab.com/postmarketOS/postmarketos-theme")
    (synopsis "PostmarketOS themed themes")
    (description
     "@code{postmarketos-theme} contains a GTK3 and GTK4 theme which is
based on Adwaita but replaces the standard blue highlights in the theme with
postmarketOS green.  There's also the oled and paper variants of the theme that are completely
black and completely white.")
    (license license:gpl2)))
