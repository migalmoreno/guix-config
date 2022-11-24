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

(define-public portfolio
  (package
    (name "portfolio")
    (version "0.9.14")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tchx84/Portfolio")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0h09v8lhz3kv6qmwjhx3gr7rp6ccfhrzm54gjnaixl4dcg9zddls"))))
    (arguments
     (list #:glib-or-gtk? #t
           #:imported-modules `(,@%meson-build-system-modules
                                (guix build python-build-system))
           #:modules '((guix build meson-build-system)
                       ((guix build python-build-system)
                        #:prefix python:)
                       (guix build utils))
           #:phases #~(modify-phases %standard-phases
                        (add-after 'install 'rename-executable
                          (lambda _
                            (with-directory-excursion (string-append #$output
                                                                     "/bin")
                              (rename-file "dev.tchx84.Portfolio" "portfolio"))))
                        (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
                          (lambda* (#:key inputs outputs #:allow-other-keys)
                            (wrap-program (search-input-file outputs
                                                             "bin/portfolio")
                              `("GUIX_PYTHONPATH" =
                                (,(getenv "GUIX_PYTHONPATH") ,(python:site-packages
                                                               inputs
                                                               outputs)))
                              `("GI_TYPELIB_PATH" =
                                (,(getenv "GI_TYPELIB_PATH")))))))))
    (build-system meson-build-system)
    (inputs (list bash-minimal python-pygobject gtk+ libhandy))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           `(,gtk+ "bin")
           python))
    (home-page "https://github.com/tchx84/Portfolio")
    (synopsis "Minimalist file manager for Linux mobile devices")
    (description
     "Portfolio is a minimalist file manager for those who want to use Linux
mobile devices.  Tap to activate and long press to select, to browse, open, copy, move,
delete, or edit your files.")
    (license license:gpl3)))
