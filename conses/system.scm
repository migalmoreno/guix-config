(define-module (conses system)
  #:use-module (conses utils)
  #:use-module (gnu system)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system file-systems)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (nongnu packages linux)
  #:use-module (guix gexp)
  #:use-module (ice-9 pretty-print))

(define-public %nonguix-signing-key
  (project-file "conses/keys/nonguix.pub"))

(define-public %default-ssh-key
  (plain-file
   "conses.pub"
   "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJgHrggw/+ZcncBvWeRmSf/PfaiGVmU2xnuh9C3mfbLN (none)\n"))

(define-public %default-kernel linux)

(define-public %default-timezone (getenv "TIMEZONE"))

(define-public %default-kernel-arguments
  (list "quiet" "net.ifnames=0"))

(define-public %default-keyboard-layout
  (keyboard-layout
   "us,es"
   #:options '("grp:shifts_toggle"
               "caps:ctrl_modifier"
               "altwin:prtsc_rwin")))

(define-public %initial-os
  (operating-system
    (host-name "conses")
    (locale "en_US.utf8")
    (timezone %default-timezone)
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets '("/boot/efi"))))
    (kernel-arguments %default-kernel-arguments)
    (keyboard-layout %default-keyboard-layout)
    (file-systems %base-file-systems)
    (issue "This is the GNU system. Welcome.\n")))
