(define-module (quasar home services fonts)
  #:use-module (gnu home-services base)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fonts)
  #:export (fonts-service))

(define (fonts-service)
  (list
   (home-generic-service 'home-font-packages
                         #:packages (list
                                     fontmanager
                                     font-iosevka
                                     font-abattis-cantarell
                                     font-ibm-plex
                                     font-fira-go
                                     font-google-noto))
   ;; (home-font-service-type
   ;;  (home-font-configuration
   ;;   (sans-serif (make-font-spec font-ibm-plex "IBM Plex Sans"))
   ;;   (serif (make-font-spec font-google-noto "Noto  Color Emoji"))
   ;;   (monospace (make-font-spec font-iosevka "Iosevka"))))
   ))
