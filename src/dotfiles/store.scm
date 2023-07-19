(define-module (dotfiles store)
  #:use-module (dotfiles utils)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services symlink-manager)
  #:use-module (gnu home services xdg)
  #:use-module (gnu machine)
  #:use-module (gnu machine ssh)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (rde features)
  #:use-module (srfi srfi-1)
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

(define-record-type* <config> config make-config
  config?
  this-config
  (host config-host (default #f))
  (users config-users (default '()))
  (machine config-machine (default %initial-machine)))

(define-record-type* <user> user make-user
  user?
  this-user
  (name user-name)
  (features user-features))

(define-record-type* <host> host make-host
  host?
  this-host
  (name host-name)
  (features host-features))

(define %configs
  (list
   (config
    (host (host (name "cygnus")
                (features (@ (dotfiles hosts cygnus) %cygnus-features))))
    (users (list (user
                  (name "deneb")
                  (features (@ (dotfiles users deneb) %deneb-features)))))
    (machine (@ (dotfiles machines cygnus) %cygnus-machine)))
   (config
    (host (host (name "lyra")
                (features (@ (dotfiles hosts lyra) %lyra-features))))
    (users (list (user
                  (name "vega")
                  (features (@ (dotfiles users vega) %vega-features))))))
   (config
    (host (host (name "live")
                (features (@ (dotfiles hosts live) %live-features)))))))

(define* (dispatcher
          #:key
          (user (or (getenv "RDE_USER") (getlogin)))
          (host (or (getenv "RDE_HOST") (gethostname)))
          (target (getenv "RDE_TARGET"))
          (initial-os %initial-os)
          (he-in-os? (not (nil? (getenv "RDE_HE_IN_OS"))))
          (pretty-print? #f)
          (configs %configs))

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

(dispatcher)
