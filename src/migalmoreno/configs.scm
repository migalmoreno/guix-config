(define-module (migalmoreno configs)
  #:use-module (migalmoreno utils)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu machine)
  #:use-module (gnu machine ssh)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (guix records)
  #:export (config
            config-host
            config-users
            config-machine
            user
            user-name
            user-features
            host
            host-name
            host-features))

(define-public %initial-os
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

(define-public %configs
  (list
   (config
    (host (host (name "cygnus")
                (features (@ (migalmoreno hosts cygnus) %cygnus-features))))
    (users (list (user
                  (name "deneb")
                  (features (@ (migalmoreno users deneb) %deneb-features)))))
    (machine (@ (migalmoreno machines cygnus) %cygnus-machine)))
   (config
    (host (host (name "lyra")
                (features (@ (migalmoreno hosts lyra) %lyra-features))))
    (users (list (user
                  (name "vega")
                  (features (@ (migalmoreno users vega) %vega-features))))))
   (config
    (host (host (name "live")
                (features (@ (migalmoreno hosts live) %live-features)))))))
