(define-module (efimerspan home services xorg)
  #:use-module (efimerspan home services glib)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:export (home-unclutter-service-type
            home-unclutter-configuration))

(define-configuration home-unclutter-configuration
  (package
    (package unclutter)
    "The @code{unclutter} package to use."))

(define (unclutter-shepherd-service config)
  (list
   (shepherd-service
    (provision '(home-unclutter))
    (requirement '(home-dbus))
    (stop #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list
               #$(file-append (home-unclutter-configuration-package config)) "/bin/unclutter"
               "-idle" "2"))))))

(define home-unclutter-service-type
  (service-type
   (name 'home-unclutter)
   (extensions
    (list
     (service-extension home-shepherd-service-type
                        unclutter-shepherd-service)))
   (default-value (home-unclutter-configuration))
   (description "Sets up an unclutter daemon.")))
