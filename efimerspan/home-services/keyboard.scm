(define-module (efimerspan home-services keyboard)
  #:use-module (gnu services)
  #:use-module (gnu packages)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:export (home-qmk-service-type
            home-xmodmap-service-type
            home-xmodmap-configuration))

(define-configuration/no-serialization home-xmodmap-configuration
  (package
    (package xmodmap)
    "The xmodmap package to use.")
  (config
   (alist '())
   "Association list of key and value pairs for xmodmap configuration file."))

(define (xmodmap-shepherd-service config)
  (list
   (shepherd-service
    (provision '(home-xmodmap))
    (start #~(make-system-constructor
              (string-join
               (list #$(file-append (home-xmodmap-configuration-package config) "/bin/xmodmap")
                     #$(home-xmodmap-file config)))))
    (one-shot? #t))))

(define (serialize-xmodmap-config field-name val)
  (define serialize-term
    (match-lambda
      ((? symbol? e) (symbol->string e))
      ((? number? e) (number->string e))
      (e e)))

  (define (serialize-field entry)
    (match entry
      ((key . value)
       (format #f "~a = ~a"
               (serialize-term key)
               (serialize-term value)))
      (key (string-append (serialize-term key)))))

  #~(string-append
         #$@(interpose (map serialize-field val) "\n" 'suffix)))

(define (home-xmodmap-file config)
  (mixed-text-file
       "config"
       (serialize-xmodmap-config #f (home-xmodmap-configuration-config config))))

(define (add-xmodmap-configuration config)
  (filter (compose not null?)
          `(("config/xmodmap/config"
             ,(home-xmodmap-file config)))))

(define (xmodmap-profile-service config)
  (list (home-xmodmap-configuration-package config)))

(define home-xmodmap-service-type
  (service-type
   (name 'home-xmodmap)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      xmodmap-profile-service)
     (service-extension
      home-shepherd-service-type
      xmodmap-shepherd-service)
     (service-extension
      home-files-service-type
      add-xmodmap-configuration)))
   (description "Configure xmodmap bindings and rules.")
   (default-value (home-xmodmap-configuration))))
