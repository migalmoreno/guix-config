(define-module (conses packages emacs-xyz)
  #:use-module (rrr packages emacs-xyz)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix packages)
  #:use-module (guix build-system emacs)
  #:use-module (guix download)
  #:use-module (guix build-system)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-ob-go
  (let ((commit "2067ed55f4c1d33a43cb3f6948609d240a8915f5")
        (revision "0"))
    (package
      (name "ob-go")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/pope/ob-go")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256 (base32 "069w9dymiv97cvlpzabf193nyw174r38lz5j11x23x956ladvpbw"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/pope/ob-go")
      (synopsis "Org Babel support for evaluating go code.")
      (description "@code{ob-go} enables Org-Babel support for evaluating go code. It
was created based on the usage of ob-C.")
      (license #f))))

(define-public emacs-with-nyxt
  (let ((commit "7e5d14bbfedebc72af5ffdc41912cbc339de59e6")
        (revision "0"))
    (package
      (name "emacs-with-nyxt")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/ag91/emacs-with-nyxt")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256 (base32 "1bkaz7jqlj4w9qx379picyq3di64x66gwwywq7i81yx7x1z9m34z"))))
      (build-system emacs-build-system)
      (propagated-inputs
       (list emacs-s))
      (home-page "https://github.com/ag91/emacs-with-nyxt")
      (synopsis "Some code to make Emacs interact with Nyxt.")
      (description "This is some little hack to make Emacs command Nyxt. In particular
this provides a function to browser URLs via Nyxt run via Slime.")
      (license license:bsd-3))))

(define-public emacs-ement-next
  (let ((commit "d216e049920ccb2839b274b202254842fe762c26")
        (revision "1"))
    (package
      (name "emacs-ement")
      (version (git-version "0.1-pre" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/alphapapa/ement.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "06simqrz32rsf1jiq6dmq7l198iwf9f4rjh46p6jsghlmphzyh2m"))))
      (build-system emacs-build-system)
      (arguments
       `(#:emacs ,emacs))
      (propagated-inputs
       (list emacs-rrr-plz
             emacs-taxy
             emacs-svg-lib
             emacs-ts))
      (home-page "https://github.com/alphapapa/ement.el")
      (synopsis "Matrix client for Emacs")
      (description "Ement.el is a Matrix client for Emacs.")
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

(define-public emacs-clj-deps-new
  (let ((commit "183089e6d4ded90efff491916e1c87411ead0461")
        (revision "0"))
    (package
      (name "emacs-clj-deps-new")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (commit commit)
               (url "https://github.com/jpe90/emacs-clj-deps-new")))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0a4fz838mxcv42z3r5h1i7nlqpl1ipqc6ljlgbhvz5hw6bd0p96w"))))
      (build-system emacs-build-system)
      (propagated-inputs
       (list emacs-transient))
      (home-page "https://github.com/jpe90/emacs-clj-deps-new")
      (synopsis "Emacs interface to deps-new and clj-new.")
      (description "Elisp wrapper around @code{deps-new,https://github.com/seancorfield/deps-new}
 and @url{clj-new,https://github.com/seancorfield/clj-new}. Create Clojure projects from templates
within Emacs.")
      (license license:gpl3))))

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

(define-public emacs-srht
  (package
    (inherit emacs-rrr-srht)
    (arguments
     (substitute-keyword-arguments (package-arguments emacs-rrr-srht)
       ((#:emacs _) emacs)))))
