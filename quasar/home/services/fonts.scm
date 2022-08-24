(define-module (quasar home services fonts)
  #:use-module (conses home services emacs)
  #:use-module (conses home services fonts)
  #:use-module (conses packages emacs-xyz)
  #:use-module (gnu home-services base)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fonts)
  #:use-module (gnu services)
  #:export (fonts-service))

(define (fonts-service)
  (list
   (elisp-configuration-service
    `((require 'fontaine)
      (custom-set-variables
       '(fontaine-presets '((docked
                             :default-family "Iosevka"
                             :default-height 107
                             :fixed-pitch-family "Iosveka"
                             :fixed-pitch-height 1.0
                             :variable-pitch-family "Noto Sans"
                             :variable-pitch-height 1.0
                             :variable-pitch-weight semi-light)))))
    #:elisp-packages (list emacs-fontaine))
   (home-generic-service 'home-font-packages
                         #:packages (list fontmanager))
   (service home-font-service-type
            (home-font-configuration
             (sans-serif (make-font-spec font-ibm-plex "IBM Plex Sans"))
             (serif (make-font-spec font-google-noto "Noto Color Emoji"))
             (monospace (make-font-spec font-iosevka "Iosevka"))))))
