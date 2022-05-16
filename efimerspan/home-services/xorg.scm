(define-module (efimerspan home-services unclutter)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu home services shepherd)
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
    (requirement '(dbus-home))
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
                        unclutter-shepher-service)))
   (default-value (home-unclutter-configuration))
   (description "Sets up an unclutter daemon.")))
