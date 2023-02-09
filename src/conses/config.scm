(define-module (conses config)
  #:use-module (conses hosts base)
  #:use-module (conses machines base)
  #:use-module (gnu system)
  #:use-module (gnu machine)
  #:use-module (gnu machine ssh)
  #:use-module (guix records)
  #:use-module (rde features)
  #:use-module (ice-9 match)
  #:use-module (nongnu packages linux)
  #:export (dispatcher))

(define* (dispatcher
          #:key
          (user (or (getenv "RDE_USER") (getlogin)))
          (host (or (getenv "RDE_HOST") (gethostname)))
          (target (getenv "RDE_TARGET"))
          (users-submodule '(conses users))
          (hosts-submodule '(conses hosts))
          (machines-submodule '(conses machines))
          (initial-os %initial-os)
          (he-in-os? (not (nil? (getenv "RDE_HE_IN_OS"))))
          (pretty-print? #f))
  "Dispatch a configuration based on a set of inputs."
  (ensure-pred string? user)
  (ensure-pred string? host)
  (ensure-pred string? target)
  (ensure-pred list? users-submodule)
  (ensure-pred list? hosts-submodule)
  (ensure-pred list? machines-submodule)
  (ensure-pred operating-system? initial-os)
  (ensure-pred boolean? he-in-os?)
  (ensure-pred boolean? pretty-print?)

  (define* (mod-ref sub mod var-name #:optional default-value)
    (let ((var (module-variable (resolve-module `(,@sub ,(string->symbol mod))) var-name)))
      (if (and var (not (unspecified? var)))
          (variable-ref var)
          default-value)))

  (define %user-features (mod-ref users-submodule user '%user-features '()))
  (define %host-features (mod-ref hosts-submodule host '%host-features '()))
  (define %machine (mod-ref machines-submodule host '%machine %initial-machine))

  (define %config
    (rde-config
     (initial-os initial-os)
     (integrate-he-in-os? he-in-os?)
     (features (append %host-features %user-features))))

  (define %he
    (rde-config-home-environment %config))

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
      (inherit %machine)
      (operating-system %os))))

  (when pretty-print?
    (pretty-print-rde-config %config))

  (match target
    ("home" %he)
    ("system" %os)
    ("deploy" %machines)
    (_ %he)))

(dispatcher)
