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
  (package
    (name "emacs-display-wttr")
    (version "20220907.1625")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://melpa.org/packages/display-wttr-" version ".el"))
       (sha256 (base32 "1x397gqgai96r1vls6zjjnzc8p18xd6y7pygmmbdy25lkp6qzblk"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/josegpt/display-wttr")
    (synopsis "Display wttr (weather) in the mode line")
    (description "Display-wttr package contains a minor mode that can be toggled on/off. It
fetches weather information based on your location or on a location set in
@code{display-wttr-locations} from @url{https://wttr.in} and then displays it on the mode line.
The entry point is @code{display-wttr}. Heavily inspired by: @code{display-time}.")
    (license license:gpl3+)))

(define-public emacs-display-wttr-next-local
  (package
    (inherit emacs-display-wttr)
   (source
    (local-file
     (string-append (dirname (dirname %project-root))
                    "/elisp/display-wttr")
     #:recursive? #t))))

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

(define-public emacs-ytdl-next-local
  (package
    (inherit emacs-ytdl)
    (name "emacs-ytdl-next-local")
   (source
    (local-file
     (string-append (dirname (dirname %project-root))
                    "/elisp/ytdl")
     #:recursive? #t))))

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

(define-public emacs-ement-next
  (let ((commit "2626e37b824cb2a2e8fb008991d140ac3351a909")
        (revision "1"))
    (package
      (inherit emacs-ement)
      (version (git-version "0.1-pre" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/alphapapa/ement.el")
               (commit commit)))
         (file-name (git-file-name (package-name emacs-ement) version))
         (sha256
          (base32
           "1wkvx0hrbnlls8dd72g273fx7380jz2gb8ak9cyjbbnwr8a1j03f")))))))

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
    (version "20220801.1917")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://melpa.org/packages/mpv-" version ".el"))
       (sha256 (base32 "0lx1mx4wj97s4s72icl2gjldncyanwqszcckwwpk9h36al6j1hsr"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/kljohann/mpv.el")
    (synopsis "Control mpv for easy note-taking.")
    (description "This package is a potpurri of helper functions to control a mpv process via its
IPC interface.")
    (license license:gpl3+)))

(define-public emacs-mpv-next
  (let ((commit "3c5260fb0151601bd096be7a11079d8d39552f56")
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
                (base32 "1s79nxbb8a26xf013vxxvijg0apnfzhwlbji349jyzdg5d6myp25")))))))

(define-public emacs-mpv-next-local
  (package
   (inherit emacs-mpv)
   (name "emacs-mpv-next-local")
   (version "0.1")
   (source
    (local-file
     (string-append (dirname (dirname %project-root))
                    "/elisp/mpv")
     #:recursive? #t))))

(define-public emacs-pulseaudio-control-next
  (package
    (inherit emacs-pulseaudio-control)
    (name "emacs-pulseaudio-control-next")
    (version "0.1")
    (source
     (local-file
      (string-append (dirname (dirname %project-root))
                     "/elisp/pulseaudio-control")
      #:recursive? #t))))

(define-public emacs-circadian
  (package
    (name "emacs-circadian")
    (version "20181024.1256")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://melpa.org/packages/circadian-" version ".el"))
       (sha256 (base32 "09pkyjb6h83xy6hq9ik98cfm8nfqhfjaa10s6yd81wi12bfaszas"))))
    (build-system emacs-build-system)
    (home-page "https://github.com/guidoschmidt/circadian.el")
    (synopsis "Theme-switching for Emacs based on daytime.")
    (description "Circadian provides automated theme switching based on daytime.")
    (license license:expat)))

(define-public emacs-org-modern-next
  (package
    (inherit emacs-org-modern)
    (version "20220821.1927")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://melpa.org/packages/org-modern-" version ".el"))
       (sha256 (base32 "020smw3ck08v11bb8vackabawahcjazlr0ll00cwmv9bgz3hcqh8"))))
    (build-system emacs-build-system)))

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
  (let ((commit "598549ffbbea4a015dca80fc2d061e6e01d77e40")
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
          (base32 "0hpbi5nh0bkrb2l422kn60giin73fmsq7gsai9qc9aifa7k1anwv"))))
      (build-system emacs-build-system)
      (propagated-inputs
       (list emacs-consult emacs-embark))
      (home-page "https://git.sr.ht/~conses/fdroid.el")
      (synopsis "An Emacs interface to manage F-Droid repositories.")
      (description "fdroid.el is an Emacs interface to fdroidcl. Its purpose is to aid the
 management of F-Droid repository packages to be installed in an Android device from the comfort of Emacs.")
      (license license:gpl3+))))

(define-public emacs-nyxt
  (let ((commit "c2b506ac29084ea543948f5d711b49cc00b017ee")
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
          (base32 "0383bpnfx474jwqla6mn2gn7ayg7xfxf6c4a0b17lq1dnvw80gqa"))))
      (build-system emacs-build-system)
      (propagated-inputs
       (list emacs-sly))
      (home-page "https://git.sr.ht/~conses/nyxt.el")
      (synopsis "Interact with Nyxt from Emacs.")
      (description "nyxt.el consists of custom logic to interact with Nyxt from Emacs.")
      (license license:gpl3+))))

(define-public emacs-dashboard-next
  (let ((commit "554dc6fac1362dd6b66318c8250eea8bd63aa92f")
        (revision "0"))
    (package
      (inherit emacs-dashboard)
      (name "emacs-dashboard-next")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/emacs-dashboard/emacs-dashboard")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "00jbjryi6m6ah7ylf1w965n4zndm5l54jkfkhjd2y3vxyr706hr8")))))))
