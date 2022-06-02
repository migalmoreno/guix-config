(define-module (efimerspan system-services web)
  #:use-module (efimerspan packages python-xyz)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:export (whoogle-service-type
            whoogle-configuration))

(define-configuration/no-serialization whoogle-configuration
  (package
    (package python-whoogle-search)
    "The @code{whoogle-search} package to use."))

(define (whoogle-shepherd-service config)
  (list
   (shepherd-service
    (provision '(whoogle-search))
    (start #~(make-forkexec-constructor
              (list (string-append #$python-whoogle-search "/bin/whoogle-search")
                    #:environment-variables
                    (append (list "CONFIG_VOLUME=/var/cache/whoogle")
                            (default-environment-variables)))))
    (stop #~(make-kill-destructor))
    (documentation "Run a whoogle-search instance."))))

(define whoogle-service-type
  (service-type
   (name 'whoogle-search)
   (extensions
    (list
     (service-extension
      shepherd-root-service-type
      whoogle-shepherd-service)))
   (default-value (whoogle-configuration))
   (description "Whoogle-search system service.")))
