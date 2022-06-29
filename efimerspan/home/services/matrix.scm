(define-module (efimerspan home services matrix)
  #:use-module (gnu services)
  #:use-module (gnu packages)
  #:use-module (gnu home services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages matrix)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu home-services-utils)
  #:use-module (ice-9 match)
  #:export (home-pantalaimon-service-type
            home-pantalaimon-configuration))

(define-configuration/no-serialization home-pantalaimon-configuration
  (package
    (package pantalaimon)
    "The @code{pantalaimon} package to use.")
  (config
   (ini-config '())
   "Alist of pairs to define the daemon configuration."))

(define (pantalaimon-shepherd-service config)
  (list
   (shepherd-service
    (documentation "Pantalaimon user service for non-E2EE Matrix clients.")
    (provision '(home-pantalaimon))
    (requirement '(home-dbus))
    (start #~(make-forkexec-constructor
              (list #$(file-append (home-pantalaimon-configuration-package config) "/bin/pantalaimon"))
              #:log-file (string-append
                          (or (getenv "XDG_LOG_HOME")
                              (format #f "~a/.local/var/log"
                                      (getenv "HOME")))
                          "/pantalaimon.log")))
    (stop #~(make-kill-destructor)))))

(define (add-pantalaimon-configuration config)
  (define (uglify-term term)
    (let ((str (maybe-object->string term)))
      (apply string-append (map string-capitalize (string-split str #\-)))))

  (define (serialize-term term)
    (match term
      ((? symbol? e) (symbol->string e))
      ((? number? e) (number->string e))
      ((? boolean? e) (boolean->true-or-false e #t))
      (e e)))

  (define (serialize-field key val)
    (let ((name ((compose uglify-term serialize-term) key))
          (value (serialize-term val)))
      (format #f "~a = ~a\n" name value)))

  (list
   `("pantalaimon/pantalaimon.conf"
     ,(mixed-text-file
       "pantalaimon.conf"
       (generic-serialize-ini-config
        #:serialize-field serialize-field
        #:fields (home-pantalaimon-configuration-config config))))))

(define (pantalaimon-profile-service config)
  (list (home-pantalaimon-configuration-package config)))

(define home-pantalaimon-service-type
  (service-type
   (name 'pantalaimon)
   (extensions
    (list
     (service-extension
      home-xdg-configuration-files-service-type
      add-pantalaimon-configuration)
      (service-extension
       home-profile-service-type
       pantalaimon-profile-service)
      (service-extension
       home-shepherd-service-type
       pantalaimon-shepherd-service)))
   (description "Configure the Pantalaimon E2EE daemon.")
   (default-value (home-pantalaimon-configuration))))
