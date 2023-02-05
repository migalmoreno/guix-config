(define-module (conses packages emacs-xyz)
  #:use-module (conses utils)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build utils)
  #:use-module (guix build-system)
  #:use-module (guix build-system emacs)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-org-recur
  (package
    (name "org-recur")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/m-cat/org-recur")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y7bz2n3qwcy1yzgzwiw18halk28nv03xys9kmcc71g6mk45blfg"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/m-cat/org-recur")
    (synopsis "Simple recurring org-mode tasks")
    (description "This package extends org-mode and org-agenda with support
for defining recurring tasks and easily scheduling them.")
    (license license:gpl3)))

(define-public emacs-helpful-next
  (let ((commit "94c25337b2de2f9da60914a7c0c6cca9584c0231")
        (revision "0"))
    (package
      (inherit emacs-helpful)
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/Wilfred/helpful")
               (commit commit)))
         (file-name (git-file-name (package-name emacs-helpful) version))
         (sha256
          (base32 "09da3d3kx4c8im58kwfv59zpwda70yvwnjk01w7r6lra1ww8d3yx")))))))

(define-public emacs-al-scheme
  (package
    (name "emacs-al-scheme")
    (version "0.1")
    (source
     (origin
       (method url-fetch)
       (uri "https://raw.githubusercontent.com/alezost/emacs-config/master/utils/al-scheme.el")
       (sha256
        (base32
         "1j43cnznfg6zzggdfgnm2cmfz7s4d3g5vj2gvkj5jp1npsaix8qy"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/alezost/emacs-config/blob/master/utils/al-scheme.el")
    (synopsis "Additional functionality for @code{scheme-mode}.")
    (description #f)
    (license license:gpl3+)))

(define-public emacs-ob-go
  (let ((commit "2067ed55f4c1d33a43cb3f6948609d240a8915f5")
        (revision "0"))
    (package
      (name "emacs-ob-go")
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

(define-public emacs-display-wttr
  (let ((commit "7062953d034e27c297d58748cf74dad552aa2873")
        (revision "0"))
    (package
      (name "emacs-display-wttr")
      (version (git-version "2.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/josegpt/display-wttr")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256 (base32 "1yppnxpzpwp3qkxfa9g8c0vzxg2s08qq4djk635hrml57adaya8g"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/josegpt/display-wttr")
      (synopsis "Display wttr (weather) in the mode line")
      (description "This package contains a minor mode that can be toggled.
It fetches weather information based on your location or on a location set in
@code{display-wttr-locations} from @uref{https://wttr.in} and then displays it
on the mode line.
The entry point is @code{display-wttr}.
Heavily inspired by: @code{display-time}.")
      (license license:gpl3+))))

(define-public emacs-ytdl-next
  (let ((commit "5c9330594fc048f1efd64b6a4bf867af35245b62")
        (branch "add-format-selection"))
    (package
      (inherit emacs-ytdl)
      (name "emacs-ytdl-next")
      (version (git-version "0" branch commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/fleetime/ytdl")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "1qryr9jp4p4l3ckpnbms6gy70wc721y0pmd598vm55vfk6fvbnqf")))))))

(define-public emacs-fontaine
  (package
   (name "emacs-fontaine")
   (version "0.3.0")
   (source
    (origin
      (method git-fetch)
      (uri
       (git-reference
        (url "https://git.sr.ht/~protesilaos/fontaine")
        (commit version)))
      (file-name (git-file-name name version))
      (sha256 (base32 "0b3zxnx8wnchiqk02pk2dhgacgcz3g9bri3735m8h3h7p415mvgs"))))
   (build-system emacs-build-system)
   (home-page "https://sr.ht/~protesilaos/fontaine")
   (synopsis "Set Emacs font configurations using presets.")
   (description "Fontaine lets the user specify presets of font configurations and
set them on demand on graphical Emacs frames. The user option @code{fontaine-presets} holds
all such presets.")
   (license license:fdl1.3+)))

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

(define-public emacs-mpv-next
  (let ((commit "bfeb4697fbc1b796671bab2494a116e8e1d30201")
        (branch "feature-add-mode-line-display-modes"))
    (package
     (inherit emacs-mpv)
     (name "emacs-mpv-next")
     (version (git-version "0" branch commit))
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/efimerspan/mpv.el")
                     (commit commit)))
               (file-name (git-file-name name version))
               (sha256
                (base32 "0qcrvsrldcdbwqv13xlhz9zias56n13kx14wsvic9ylgdd60rb5v")))))))

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
  (let ((commit "0e53961bbc997ec00317b1fd143e5e1336bd8754")
        (revision "4")
        (version "0.1"))
    (package
      (name "emacs-srht")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.sr.ht/~akagi/srht.el")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1c9d8ldxbr3f5bxsmfjgmnpx04hd1rnach8p84ns5xq68mjq9vil"))))
      (build-system emacs-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            ;; Move the source files to the top level, which is included in
            ;; the EMACSLOADPATH.
            (add-after 'unpack 'move-source-files
              (lambda _
                (let ((el-files (find-files "./lisp" ".*\\.el$")))
                  (for-each (lambda (f)
                              (rename-file f (basename f)))
                            el-files)))))))
      (propagated-inputs (list emacs-plz))
      (home-page "https://git.sr.ht/~akagi/srht.el")
      (synopsis "Interact with sourcehut")
      (description #f)
      (license license:gpl3+))))

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

(define-public emacs-fdroid
  (let ((commit "fafacb77014d5c796f07c2c1ff501919d9f2f286")
        (revision "0"))
    (package
      (name "emacs-fdroid")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://git.sr.ht/~conses/fdroid.el")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1qw0v92j1d7f7d01y6wzgk1ckx1wz896fks79f36glbp1svassmq"))))
      (build-system emacs-build-system)
      (propagated-inputs
       (list emacs-consult emacs-embark))
      (home-page "https://git.sr.ht/~conses/fdroid.el")
      (synopsis "An Emacs interface to manage F-Droid repositories.")
      (description "fdroid.el is an Emacs interface to fdroidcl. Its purpose is to aid the
 management of F-Droid repository packages to be installed in an Android device from the comfort of Emacs.")
      (license license:gpl3+))))

(define-public emacs-nyxt
  (let ((commit "c39a6645f7a62ea7045e0d0660cab450e5a9f6c9")
        (revision "0"))
    (package
      (name "emacs-nyxt")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://git.sr.ht/~conses/nyxt.el")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1incvxvmxsyl85k8myh1y9ny7vsi11hfqqvi93yppgrcm94rhkgv"))))
      (build-system emacs-build-system)
      (propagated-inputs
       (list emacs-sly))
      (home-page "https://git.sr.ht/~conses/nyxt.el")
      (synopsis "Interact with Nyxt from Emacs.")
      (description "nyxt.el consists of custom logic to interact with Nyxt from Emacs.")
      (license license:gpl3+))))