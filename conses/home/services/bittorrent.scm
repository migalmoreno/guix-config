(define-module (conses home services bittorrent)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix packages)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu services shepherd)
  #:use-module (gnu home services shepherd)
  #:export (home-transmission-configuration
            home-transmission-service-type))

(define-configuration/no-serialization home-transmission-configuration
  (package
    (package transmission)
    "The transmission package to use."))

(define (home-transmission-shepherd-service config)
  (list
   (shepherd-service
    (provision '(home-transmission))
    (start #~(make-forkexec-constructor
              (list #$(file-append (home-transmission-configuration-package config)
                                   "/bin/transmission-daemon")
                    "--foreground")))
    (stop #~(make-kill-destructor)))))

(define (home-transmission-profile-service config)
  (list (home-transmission-configuration-package config)))

(define home-transmission-service-type
  (service-type
   (name 'home-transmission)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      home-transmission-profile-service)
     (service-extension
      home-shepherd-service-type
      home-transmission-shepherd-service)))
   (description "Launch a transmission daemon from the user local space.")
   (default-value (home-transmission-configuration))))
