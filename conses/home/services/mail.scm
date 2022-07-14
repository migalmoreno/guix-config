(define-module (conses home services mail)
  #:use-module (gnu packages mail)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home-services-utils)
  #:use-module (rde serializers json)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:export (home-goimapnotify-configuration
            home-goimapnotify-service-type))

(define-configuration/no-serialization home-goimapnotify-configuration
  (package
    (package go-gitlab.com-shackra-goimapnotify)
    "The @code{goimapnotify} package to use.")
  (config
   (json-config '())
   "Alist of pairs that make up the @code{goimapnotify} configuration."))

(define (home-goimapnotify-file config)
  (apply mixed-text-file "imapnotify.conf"
         (serialize-json-config
          (home-goimapnotify-configuration-config config))))

(define (home-goimapnotify-shepherd-service config)
  (list
   (shepherd-service
    (provision '(home-goimapnotify))
    (start #~(make-forkexec-constructor
              (list
               #$(file-append (home-goimapnotify-configuration-package config)
                              "/bin/goimapnotify")
               "-conf"
               #$(home-goimapnotify-file config))
              #:log-file (string-append
                          (or (getenv "XDG_LOG_HOME")
                              (format #f "~a/.local/var/log"
                                      (getenv "HOME")))
                          "/goimapnotify.log")))
    (stop #~(make-kill-destructor)))))

(define (home-goimapnotify-profile-service config)
  (list (home-goimapnotify-configuration-package config)))

(define home-goimapnotify-service-type
  (service-type
   (name 'home-goimapnotify)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      home-goimapnotify-profile-service)
     (service-extension
      home-shepherd-service-type
      home-goimapnotify-shepherd-service)))
   (default-value (home-goimapnotify-configuration))
   (description "Configures the @code{goimapnotify} IMAP Mailbox notifier.")))

(define (generate-home-goimapnotify-documentation)
  (generate-documentation
   `((home-goimapnotify-configuration
      ,home-goimapnotify-configuration-fields))
   'home-goimapnotify-configuration))
