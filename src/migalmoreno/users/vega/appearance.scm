(define-module (migalmoreno users vega appearance)
  #:use-module (rde features fontutils)
  #:use-module (rde features emacs-xyz))

(define-public (features _ palette)
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
