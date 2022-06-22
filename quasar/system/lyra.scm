(define-module (quasar system lyra)
  #:use-module (quasar home)
  #:use-module (efimerspan serializers lisp)
  #:use-module (efimerspan system services web)
  #:use-module (efimerspan system services matrix)
  #:use-module (guix packages)
  #:use-module (nongnu packages linux)
  #:use-module (flat packages emacs)
  #:use-module (srfi srfi-1)
  #:use-module (gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services cups)
  #:use-module (gnu services networking)
  #:use-module (gnu services desktop)
  #:use-module (gnu services xorg)
  #:use-module (gnu services linux)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services sddm)
  #:use-module (gnu services spice)
  #:use-module (gnu services file-sharing)
  #:use-module (gnu services ssh)
  #:use-module (gnu services databases)
  #:export (%system/lyra))

(define %ddcci-config
  (plain-file "ddcci.conf"
              "options ddcci dyndbg delay=120"))

(define %backlight-udev-rule
  (udev-rule
   "90-backlight.rules"
   (string-append
    "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
    "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
    "\n"
    "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
    "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

(define %spice-udev-rule
  (udev-rule
   "50-spice.rules"
   (string-append
    "SUBSYSTEM==\"usb\", GROUP=\"spice\", MODE=\"0660\""
    "\n"
    "SUBSYSTEM==\"usb_device\", GROUP=\"spice\", MODE=\"0660\"")))

(define %lyra-groups
  (cons
   (user-group
    (name "spice")
    (system? #t))
   %base-groups))

(define %lyra-users
  (cons*
   (user-account
    (name "vega")
    (comment "vega")
    (group "users")
    (home-directory "/home/vega")
    (supplementary-groups
     (append
      (map user-group-name %lyra-groups)
      '("libvirt"))))
   %base-user-accounts))

(define %lyra-packages
  (append
    (list
     emacs-no-x
     nss-certs
     cryptsetup
     git
     curl
     gnu-make
     password-store)
    %base-packages))

(define %lyra-services
  (cons* (screen-locker-service slock #:allow-empty-passwords? #f)
         (service kernel-module-loader-service-type
                  '("ddcci" "ddcci_backlight"))
         (simple-service 'ddcci-config etc-service-type
                         (list `("modprobe.d/ddcci.conf"
                                 ,%ddcci-config)))
         (bluetooth-service #:auto-enable? #t)
         (service virtlog-service-type)
         (service libvirt-service-type
                  (libvirt-configuration
                   (unix-sock-group "libvirt")
                   (tls-port "16555")))
         (extra-special-file "/usr/bin/env"
                             (file-append coreutils "/bin/env"))
         (extra-special-file "/bin/bash"
                             (file-append bash "/bin/bash"))
         (service whoogle-service-type)
         (service postgresql-service-type)
         (service postgresql-role-service-type
                  (postgresql-role-configuration
                   (roles
                    (list
                     (postgresql-role
                      (name "vega")
                      (create-database? #t))))))
         (modify-services %desktop-services
           (udev-service-type config =>
                              (udev-configuration
                               (inherit config)
                               (rules (append
                                        (udev-configuration-rules config)
                                        (list %backlight-udev-rule
                                              %spice-udev-rule)))))
           (guix-service-type config =>
                              (guix-configuration
                               (inherit config)
                               (substitute-urls
                                (append (list "https://substitutes.nonguix.org")
                                        %default-substitute-urls))
                               (authorized-keys
                                (append
                                  (list
                                   (local-file "../../keys/signatures/nonguix.pub"))
                                  %default-authorized-guix-keys))))
           (delete gdm-service-type))))

(define %lyra-mapped-devices
  (list (mapped-device
         (source
          (uuid "0f74821b-da48-4f0c-9f94-f39e646da1bf"))
         (target "system-root")
         (type luks-device-mapping))))

(define %lyra-file-systems
  (cons*
   (file-system
     (mount-point "/")
     (device "/dev/mapper/system-root")
     (type "ext4")
     (dependencies %lyra-mapped-devices))
   (file-system
     (device "/dev/nvme0n1p1")
     (mount-point "/boot/efi")
     (type "vfat"))
   %base-file-systems))

(define %system/lyra
  (operating-system
    (kernel linux)
    (kernel-loadable-modules (list ddcci-driver-linux))
    (firmware (list linux-firmware sof-firmware))
    (locale "en_US.utf8")
    (timezone (getenv "LYRA_TIMEZONE"))
    (keyboard-layout (keyboard-layout "us"))
    (host-name "lyra")
    (groups %lyra-groups)
    (users %lyra-users)
    (packages %lyra-packages)
    (services %lyra-services)
    (bootloader
     (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets (list "/boot/efi"))
      (keyboard-layout keyboard-layout)))
    (mapped-devices %lyra-mapped-devices)
    (file-systems %lyra-file-systems)))

%system/lyra
