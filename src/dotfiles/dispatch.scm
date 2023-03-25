(define-module (dotfiles dispatch)
  #:use-module (dotfiles common)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu machine)
  #:use-module (gnu machine ssh)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (rde features)
  #:export (dispatcher))

(define %initial-os
  (operating-system
    (host-name "guix")
    (locale "en_US.utf8")
    (timezone %default-timezone)
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets '("/boot/efi"))))
    (kernel-arguments %default-kernel-arguments)
    (keyboard-layout %default-keyboard-layout)
    (file-systems %base-file-systems)
    (issue "This is the GNU system. Welcome.\n")
    (sudoers-file #f)))

(define-public %initial-machine
  (machine
   (operating-system %initial-os)
   (environment managed-host-environment-type)))

(define* (dispatcher
          #:key
          (user (or (getenv "RDE_USER") (getlogin)))
          (host (or (getenv "RDE_HOST") (gethostname)))
          (target (getenv "RDE_TARGET"))
          (users-submodule '(dotfiles users))
          (hosts-submodule '(dotfiles hosts))
          (machines-submodule '(dotfiles machines))
          (initial-os %initial-os)
          (he-in-os? (not (nil? (getenv "RDE_HE_IN_OS"))))
          (pretty-print? #f))

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
