(define-module (conses home services linux)
  #:use-module (conses packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:export (home-udiskie-service-type))

(define-configuration/no-serialization home-udiskie-configuration
  (udiskie
    (package udiskie)
    "The udiskie package to use.")
  (notify?
   (boolean #f)
   "Whether to enable pop-up notifications."))

(define (home-udiskie-profile-service config)
  (list (home-udiskie-configuration-udiskie config)))

(define (home-udiskie-shepherd-service config)
  (list
   (shepherd-service
    (provision '(udiskie))
    (stop #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list
               #$(file-append (home-udiskie-configuration-udiskie config) "/bin/udiskie")
               (if #$(home-udiskie-configuration-notify? config)
                   "-n"
                   "-N")))))))

(define home-udiskie-service-type
  (service-type
   (name 'udiskie)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      home-udiskie-profile-service)
     (service-extension
      home-shepherd-service-type
      home-udiskie-shepherd-service)))
   (description "Sets a udiskie daemon to automount removable media.")
   (default-value (home-udiskie-configuration))))

(define (generate-home-udiskie-documentation)
  (generate-documentation
   `((home-udiskie-configuration
      ,home-udiskie-configuration-fields))
   'home-udiskie-configuration))
