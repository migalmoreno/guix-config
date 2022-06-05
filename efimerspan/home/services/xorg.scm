(define-module (efimerspan home services xorg)
  #:use-module (efimerspan home services glib)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu home services)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system setuid)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (home-unclutter-service-type
            home-unclutter-configuration
            xinitrc
            xorg-start-command
            xorg-configuration))

(define-configuration/no-serialization home-unclutter-configuration
  (package
    (package unclutter)
    "The @code{unclutter} package to use.")
  (seconds
   (integer 5)
   "The number of idle seconds to wait for @code{unclutter} to remove the cursor."))

(define (unclutter-shepherd-service config)
  (list
   (shepherd-service
    (provision '(home-unclutter))
    (requirement '(home-dbus))
    (stop #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list
               #$(file-append (home-unclutter-configuration-package config) "/bin/unclutter")
               "-idle"
               #$(number->string (home-unclutter-configuration-seconds config))))))))

(define home-unclutter-service-type
  (service-type
   (name 'home-unclutter)
   (extensions
    (list
     (service-extension home-shepherd-service-type
                        unclutter-shepherd-service)))
   (default-value (home-unclutter-configuration))
   (description "Sets up an unclutter daemon.")))
