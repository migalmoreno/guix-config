(define-module (migalmoreno dispatcher)
  #:use-module (migalmoreno config)
  #:use-module (gnu home)
  #:use-module (gnu machine)
  #:use-module (gnu system)
  #:use-module (ice-9 match)
  #:use-module (rde features)
  #:use-module (srfi srfi-1)
  #:export (dispatcher))

(define* (dispatcher
          configs
          #:key
          (user (or (getenv "RDE_USER") (getlogin)))
          (host (or (getenv "RDE_HOST") (gethostname)))
          (target (getenv "RDE_TARGET"))
          (initial-os %initial-os)
          (he-in-os? (not (nil? (getenv "RDE_HE_IN_OS"))))
          (pretty-print? #f))
  (define %host
    (and=>
     (find (lambda (config)
             (equal? (and=> (config-host config) host-name) host))
           (filter (lambda (config)
                     (not (null? (config-host config))))
                   configs))
     config-host))
  (define %machine
    (and=>
     (find (lambda (config)
             (and %host (equal? (config-host config) %host)))
           configs)
     config-machine))
  (define %user-features
    (or (and=>
         (find (lambda (user_)
                 (and user (equal? (user-name user_) user)))
               (append-map config-users
                           (filter (lambda (config)
                                     (not (null? (config-users config))))
                                   configs)))
         user-features)
        '()))
  (define %host-features (or (and=> %host host-features) '()))

  (define %config
    (rde-config
     (initial-os initial-os)
     (integrate-he-in-os? he-in-os?)
     (features (append %host-features %user-features))))

  (define %he (rde-config-home-environment %config))

  (define %os
    (operating-system
     (inherit (rde-config-operating-system %config))
     (kernel-arguments
      (append
       (get-value 'kernel-arguments %config '())
       (operating-system-user-kernel-arguments initial-os)))
     (issue (operating-system-issue initial-os))))

  (define %machines
    (list
     (machine
      (inherit (or %machine %initial-machine))
      (operating-system %os))))

  (when pretty-print?
    (pretty-print-rde-config %config))

  (match target
    ("home" %he)
    ("system" %os)
    ("deploy" %machines)
    (_ %he)))
