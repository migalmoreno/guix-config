(define-module (conses home services keyboard)
  #:use-module (rde serializers ini)
  #:use-module (gnu packages)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home-services-utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:export (home-xmodmap-service-type
            home-xmodmap-configuration
            home-qmk-service-type
            home-qmk-configuration))


;;;
;;; qmk
;;;

(define-configuration/no-serialization home-qmk-configuration
  (settings
   (ini-config '())
   "Alist of key and value pairs for the @code{QMK} configuration file."))

(define (home-qmk-files-service config)
  `(("qmk/qmk.ini"
     ,(apply mixed-text-file
             "qmk.ini"
             (serialize-ini-config
              (home-qmk-configuration-settings config))))))

(define home-qmk-service-type
  (service-type
   (name 'home-qmk)
   (extensions
    (list
     (service-extension
      home-xdg-configuration-files-service-type
      home-qmk-files-service)))
   (default-value (home-qmk-configuration))
   (description "Set up the QMK Firmware configuration.")))

(define (generate-home-qmk-documentation)
  (generate-documentation
   `((home-qmk-configuration
      ,home-qmk-configuration-fields))
   'home-qmk-configuration))


;;;
;;; xmodmap
;;;

(define-configuration/no-serialization home-xmodmap-configuration
  (xmodmap
   (package xmodmap)
   "The xmodmap package to use.")
  (config
   (alist '())
   "Association list of key and value pairs for the
 @code{xmodmap} configuration file."))

(define (home-xmodmap-shepherd-service config)
  (list
   (shepherd-service
    (provision '(xmodmap))
    (start #~(make-system-constructor
              (string-join
               (list #$(file-append
                        (home-xmodmap-configuration-xmodmap config)
                        "/bin/xmodmap")
                     #$(home-xmodmap-file config)))))
    (one-shot? #t))))

(define (get-xmodmap-configuration field-name val)
  (define serialize-term
    (match-lambda
      ((? vector? e)
       (string-join
        (vector-fold
         (lambda (i acc e)
           (append
             acc
             (list (serialize-term e))))
         '() e) " "))
      ((? symbol? e) (symbol->string e))
      ((? number? e) (number->string e))
      (e e)))

  (define serialize-field
    (match-lambda
      ((? list? e)
       (string-join
        (map
         (lambda (x)
           (serialize-term x))
         e)
        " "))
      ((key . value)
       (format #f "~a = ~a"
               (serialize-term key)
               (serialize-term value)))
      (key (string-append (serialize-term key)))))

  #~(string-append
     #$@(interpose
         (map serialize-field val)
         "\n" 'suffix)))

(define (home-xmodmap-files-service config)
  (filter (compose not null?)
          `(("xmodmap/config"
             ,(home-xmodmap-file config)))))

(define (home-xmodmap-file config)
  (mixed-text-file
       "config"
       (get-xmodmap-configuration #f (home-xmodmap-configuration-config config))))

(define (home-xmodmap-profile-service config)
  (list (home-xmodmap-configuration-xmodmap config)))

(define home-xmodmap-service-type
  (service-type
   (name 'home-xmodmap)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      home-xmodmap-profile-service)
     (service-extension
      home-xdg-configuration-files-service-type
      home-xmodmap-files-service)
     (service-extension
      home-shepherd-service-type
      home-xmodmap-shepherd-service)))
   (description "Configure @code{xmodmap} bindings and rules.")
   (default-value (home-xmodmap-configuration))))

(define (generate-home-xmodmap-documentation)
  (generate-documentation
   `((home-xmodmap-configuration
      ,home-xmodmap-configuration-fields))
   'home-xmodmap-configuration))
