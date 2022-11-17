(define-module (conses config)
  #:use-module (conses system)
  #:use-module (conses deploy)
  #:use-module (rde features)
  #:use-module (gnu system)
  #:use-module (gnu machine)
  #:use-module (gnu machine ssh)
  #:use-module (nongnu packages linux)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (dispatcher))

(define* (dispatcher
          #:key
          (user (or (getenv "RDE_USER") (getlogin)))
          (system (or (getenv "RDE_SYSTEM") (gethostname)))
          (target (getenv "RDE_TARGET"))
          (home-submodule '(conses home))
          (system-submodule '(conses system))
          (deploy-submodule '(conses deploy))
          (initial-os %initial-os)
          (he-in-os? (not (nil? (getenv "RDE_HE_IN_OS"))))
          (pretty-print? #f))
  "Dispatch a configuration based on a set of targets."
  (ensure-pred string? user)
  (ensure-pred string? system)
  (ensure-pred string? target)
  (ensure-pred list? system-submodule)
  (ensure-pred list? deploy-submodule)
  (ensure-pred operating-system? initial-os)
  (ensure-pred boolean? he-in-os?)
  (ensure-pred boolean? pretty-print?)

  (define* (mod-ref sub mod var-name #:optional default-value)
    (let ((var (module-variable (resolve-module `(,@sub ,(string->symbol mod))) var-name)))
      (if (and var (not (unspecified? var)))
          (variable-ref var)
          default-value)))

  (define %home-features (mod-ref home-submodule user '%home-features '()))
  (define %system-features (mod-ref system-submodule system '%system-features '()))
  (define %deploy-machine (mod-ref deploy-submodule system '%machine %initial-machine))

  (define %config
    (rde-config
     (initial-os initial-os)
     (integrate-he-in-os? he-in-os?)
     (features (append %system-features %home-features))))

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

  (define %machine
    (list
     (machine
      (inherit %deploy-machine)
      (operating-system %os))))

  (when pretty-print?
    (pretty-print-rde-config %config))

  (match target
    ("home" %he)
    ("system" %os)
    ("deploy" %machine)
    (_ %he)))

(dispatcher)
