(define-module (migalmoreno presets desktop)
  #:use-module (migalmoreno utils)
  #:use-module (rde features bluetooth)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features fontutils)
  #:use-module (rde features keyboard)
  #:use-module (rde features linux)
  #:use-module (rde features networking)
  #:use-module (rde features wm)
  #:use-module (rde features xdg)
  #:use-module (rde features xdisorg))

(define-public (desktop-features _ palette)
  (list
   (feature-networking)
   (feature-pipewire)
   (feature-emacs-pulseaudio-control)
   (feature-emacs-power-menu)
   (feature-emacs-ednc)
   (feature-swaylock
    #:swaylock (@ (gnu packages wm) swaylock-effects)
    #:extra-config
    `((screenshots)
      (clock)
      (indicator)
      (effect-blur . 7x5)
      (effect-vignette . "0.5:0.5")
      (hide-keyboard-layout)
      ,(cons 'indicator-color (substring (palette 'accent-0) 1))
      ,(cons 'inside-color (substring (palette 'fg) 1))
      ,(cons 'ring-color (substring (palette 'accent-0) 1))))
   (feature-swayidle)
   (feature-kanshi
    #:extra-config
    `((profile headless ((output eDP-1 enable)))
      (profile docked ((output eDP-1 disable)
                       (output DP-3 enable)))))
   (feature-bluetooth)
   (feature-keyboard
    #:keyboard-layout %default-keyboard-layout)
   (feature-xdg
    #:xdg-user-directories-configuration
    (home-xdg-user-directories-configuration
     (desktop "$HOME")
     (documents "$HOME/documents")
     (download "$HOME/downloads")
     (music "$HOME/music")
     (pictures "$HOME/pictures")
     (publicshare "$HOME")
     (videos "$HOME/videos")
     (templates "$HOME")))))

(define-public (ui-features _ palette)
  (list
   (feature-emacs-appearance)
   (feature-emacs-modus-themes
    #:dark? (not (palette 'light?))
    #:deuteranopia? #f
    #:headings-scaling? #t
    #:extra-modus-themes-overrides
    '((bg-mode-line-active bg-dim)))
   (feature-fonts
    #:use-serif-for-variable-pitch? #f
    #:font-serif
    (font
     (name "IBM Plex Serif")
     (size 11)
     (package (@ (gnu packages fonts) font-ibm-plex)))
    #:font-sans
    (font
     (name "IBM Plex Sans")
     (size 11)
     (package (@ (gnu packages fonts) font-ibm-plex))
     (weight 'light))
    #:font-unicode
    (font
     (name "Noto Color Emoji")
     (size 11)
     (package (@ (gnu packages fonts) font-google-noto))))))
