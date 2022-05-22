(define-module (efimerspan packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix build-system emacs)
  #:use-module (guix download)
  #:use-module (guix build-system)
  #:use-module (guix git-download)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages ocaml)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-elibs
  (let ((commit "86c505cb808c800a59f8080bd8df69a71052f3f9")
        (revision "0"))
    (package
      (name "emacs-elibs")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://git.sr.ht/~mt08gh/elibs")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0kxs78972ivk1v8y8xbwqj3gayp2j30zjrp1qp3wvvswj1pvqlal"))))
      (build-system emacs-build-system)
      (propagated-inputs
       (list emacs-consult
             emacs-org
             emacs-org-roam
             emacs-org-superstar
             emacs-geiser
             emacs-sly
             emacs-modus-themes
             emacs-dashboard
             emacs-pdf-tools
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
      (home-page "https://git.sr.ht/~mt08gh/elibs")
      (synopsis "Various Emacs Lisp libraries.")
      (description "A set of Emacs Lisp libraries to enhance the functionality
 of some Emacs packages and built-in features.")
      (license license:gpl3+))))

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

(define-public emacs-bufler
  (package
    (name "emacs-bufler")
    (version "20210907.1145")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://melpa.org/packages/bufler-" version ".tar"))
       (sha256 (base32 "0p7ql5lw4dgdzg9ad6ms6m53gxs3276l5wld6jqahj4z6asrnj56"))))
    (build-system emacs-build-system)
    (propagated-inputs
     (list
      emacs-dash
      emacs-f
      emacs-magit
      emacs-pretty-hydra))
    (home-page "https://github.com/alphapapa/bufler.el")
    (synopsis "A butler for your buffers.")
    (description "This package allows one to group buffers into workspaces with programmable rules,
and easily switch to and manipulate them.")
    (license license:gpl3+)))

(define-public emacs-mpv
  (package
    (name "emacs-mpv")
    (version "20211228.2043")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://melpa.org/packages/mpv-" version ".el"))
       (sha256 (base32 "0zkacix37y9ypy1g0q62w8fhrzcs18v4sfv6r2dhbcq347v9ir9v"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/kljohann/mpv.el")
    (synopsis "Control mpv for easy note-taking.")
    (description "This package is a potpurri of helper functions to control a mpv process via its
IPC interface.")
    (license license:gpl3+)))

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

(define-public emacs-calc-currency
  (let ((commit "7021d892ef38b01b875082aba4bae2517ce47ae6")
        (revision "0"))
    (package
      (name "emacs-calc-currency")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/jws85/calc-currency")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256 (base32 "0y4m0hasg4ji6zfis3088hq90pm9998lnnh8yg9g8yqqaqpfizp8"))))
      (build-system emacs-build-system)
      (propagated-inputs
       (list emacs-f))
      (home-page "https://github.com/jws85/calc-currency")
      (synopsis "Add currency units to Emacs Calc.")
      (description "This package adds custom units to the Units Table in Emacs Calc by fetching
exchange rates backends.")
      (license license:gpl3+))))

(define-public emacs-hydra-posframe
  (let ((commit "343a269b52d6fb6e5ae6c09d91833ff4620490ec")
        (revision "0"))
    (package
      (name "emacs-hydra-posframe")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/Ladicle/hydra-posframe")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256 (base32 "03f9r8glyjvbnwwy5dmy42643r21dr4vii0js8lzlds7h7qnd9jm"))))
      (build-system emacs-build-system)
      (propagated-inputs
       (list emacs-hydra emacs-posframe))
      (home-page "https://github.com/Ladicle/hydra-posframe")
      (synopsis "Hydra extension which shows hydra hints on posframe.")
      (description "This package adds a Hydra extension that lets the user see their hydras in a
 posframe, allowing them to be centered in the Emacs frame.")
      (license license:gpl3+))))

(define-public emacs-vertico-posframe
  (package
    (name "emacs-vertico-posframe")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/vertico-posframe-" version ".tar"))
       (sha256 (base32 "0xkazpyfdb9h6kpqwh06szy8m3019plkixgk9qsbx6h1h07klq3i"))))
    (build-system emacs-build-system)
    (propagated-inputs
     (list emacs-posframe emacs-vertico))
    (home-page "https://github.com/tumashu/vertico-posframe")
    (synopsis "Vertico extension to show minibuffer candidates in a posframe.")
    (description "vertico-posframe is a Vertico extension, which lets Vertico use posframe show its
candidate menu.")
    (license license:gpl3+)))

(define-public emacs-ednc
  (let ((commit "537e2e165984b53b45cf760ea9e4b86794b8a09d")
        (revision "0"))
    (package
      (name "emacs-ednc")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/sinic/ednc")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256 (base32 "07cnp40rbl2p4mn40cib6mvby1svxqd8kb3dxb3a8idb736nzqrp"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/sinic/ednc")
      (synopsis "Emacs Desktop Notification Center.")
      (description "EDNC is an Emacs package written in pure Lisp that implements a Desktop
Notifications service according to the freedesktop.org specification. EDNC aspires to be a small,
 but flexible drop-in replacement of standalone daemons like Dunst.")
      (license license:gpl3+))))

(define-public emacs-app-launcher
  (let ((commit "80a9ed37892ee6e21fe44487ed11f66a15e3f440")
        (revision "0"))
    (package
      (name "emacs-app-launcher")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/SebastienWae/app-launcher")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256 (base32 "1ywhfx8604ifmvcy2397bmvq2wj03jyqnm0g7lmqqi5p97rjbdgc"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/SebastienWae/app-launcher")
      (synopsis "Launch applications from Emacs.")
      (description "app-launcher defines an app-launcher-run-command which uses the Emacs standard
completion to select an application in your machine and open it.")
      (license license:gpl3+))))

(define-public emacs-capf-autosuggest
  (package
    (name "emacs-capf-autosuggest")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/capf-autosuggest-" version ".tar"))
       (sha256 (base32 "05abnvg84248pbqr2hdkyxr1q1qlgsf4nji23nw41bfly795ikpm"))))
    (build-system emacs-build-system)
    (home-page "https://repo.or.cz/emacs-capf-autosuggest.git")
    (synopsis "History autosuggestions for comint and eshell.")
    (description "This package provides a minor mode, capf-autosuggest-mode that lets one preview
the first completion candidate for in-buffer completion as an overlay. Instead of using the default
 hook completion-at-point-functions, this package uses its own hook capf-autosuggest-capf-functions.")
    (license license:gpl3+)))

(define-public emacs-svg-lib
  (package
    (name "emacs-svg-lib")
    (version "0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://elpa.gnu.org/packages/svg-lib-" version ".tar"))
       (sha256 (base32 "0361w1paqrgqlv8wj5vf9ifssddrk2bwlarp2c2wzlxks3ahdf2x"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/rougier/svg-lib")
    (synopsis "Emacs SVG libraries for creating tags, icons and bars.")
    (description "A small Emacs library to create and display various SVG objects, namely tags, progress bars,
 progress pies and icons. Each object is guaranteed to fit nicely in a text buffer ensuring width is an integer
multiple of character width.")
    (license license:gpl3)))

(define-public emacs-nm
  (let ((commit "0aee81296420a84004b27b99d90f831393b55ed0")
        (revision "0"))
    (package
      (name "emacs-nm")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (commit commit)
               (url "https://github.com/Kodkollektivet/emacs-nm")))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0jnndiw6xzlwzbh963m4y30lr9zxjmi3m3xh481mqqvnl8zpx2k4"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/Kodkollektivet/emacs-nm")
      (synopsis "NetworkManager integration in Emacs.")
      (description "This package provides an Emacs interface for the Gnome NetworkManager to easily
set up WiFi interfaces via nmcli.")
      (license #f))))

(define-public emacs-hexrgb
  (let ((commit "90e5f07f14bdb9966648977965094c75072691d4")
        (revision "0"))
    (package
      (name "emacs-hexrgb")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (commit commit)
               (url "https://github.com/emacsmirror/hexrgb")))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0y5l6hrzm5j2jfrm5jp5zrxhxgvf930m2k4nyvk0rllpx0i1271z"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/emacsmirror/hexrgb")
      (synopsis "Functions to manipulate colors, including RGB hex strings.")
      (description "This library provides functions for converting between RGB
 (red, green, blue) color components and HSV (hue, saturation, value) color components.
 It helps you convert among Emacs color components (whole numbers from 0 to 65535), RGB and HSV
 floating-point components (0.0 through 1.0), Emacs color strings (such as \"blue\"),
and hex color RGB color strings (such as \"#FC43A7912\").")
      (license license:gpl3+))))
