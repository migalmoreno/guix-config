(define-module (rde home services matrix)
  #:use-module (rde serializers ini)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages matrix)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:export (home-pantalaimon-configuration
            home-pantalaimon-service-type))

(define-configuration/no-serialization home-pantalaimon-configuration
  (pantalaimon
    (package pantalaimon)
    "The @code{pantalaimon} package to use.")
  (config
   (ini-config '())
   "Alist of pairs to define the daemon configuration."))

(define (home-pantalaimon-shepherd-service config)
  (list
   (shepherd-service
    (documentation "Pantalaimon user service for non-E2EE Matrix clients.")
    (provision '(pantalaimon))
    (requirement '(dbus))
    (start #~(make-forkexec-constructor
              (list #$(file-append
                       (home-pantalaimon-configuration-pantalaimon config)
                       "/bin/pantalaimon"))
              #:log-file (string-append
                          (or (getenv "XDG_LOG_HOME")
                              (format #f "~a/.local/var/log"
                                      (getenv "HOME")))
                          "/pantalaimon.log")))
    (stop #~(make-kill-destructor)))))

(define (home-pantalaimon-files-service config)
  (list
   `("pantalaimon/pantalaimon.conf"
     ,(apply mixed-text-file
             "pantalaimon.conf"
             (serialize-ini-config (home-pantalaimon-configuration-config config))))))

(define (home-pantalaimon-profile-service config)
  (list (home-pantalaimon-configuration-pantalaimon config)))

(define home-pantalaimon-service-type
  (service-type
   (name 'pantalaimon)
   (extensions
    (list
     (service-extension
      home-xdg-configuration-files-service-type
      home-pantalaimon-files-service)
     (service-extension
      home-profile-service-type
      home-pantalaimon-profile-service)
     (service-extension
      home-shepherd-service-type
      home-pantalaimon-shepherd-service)))
   (description "Configure the Pantalaimon E2EE daemon.")
   (default-value (home-pantalaimon-configuration))))

(define (generate-home-pantalaimon-documentation)
  (generate-documentation
   `((home-pantalaimon-configuration
      ,home-pantalaimon-configuration-fields))
   'home-pantalaimon-configuration))
