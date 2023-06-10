(define-module (dotfiles hosts lyra)
  #:use-module (dotfiles common)
  #:use-module (dotfiles utils)
  #:use-module (dotfiles users hydri)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services databases)
  #:use-module (gnu services linux)
  #:use-module (gnu services spice)
  #:use-module (gnu services ssh)
  #:use-module (gnu services syncthing)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services web)
  #:use-module (gnu services xorg)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu system uuid)
  #:use-module (gnu system shadow)
  #:use-module (gnu system accounts)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system mapped-devices)
  #:use-module (guix gexp)
  #:use-module (rde packages)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features databases)
  #:use-module (rde features system)
  #:use-module (rde features web))


;;; Service extensions

(define extra-etc-files-service
  (simple-service
   'add-etc-files
   etc-service-type
   `(("modprobe.d/ddcci.conf"
      ,(plain-file
        "ddcci.conf"
        (string-append "options ddcci dyndbg delay=120" "\n"
                       "options ddcci-backlight dyndbg")))
     ("modprobe.d/v4l2loopback.conf"
      ,(plain-file
        "v4l2loopback.conf"
        "options v4l2loopback exclusive_caps=1 max_buffers=2 video_nr=-1")))))

(define extra-user-groups-service
  (simple-service
   'add-spice-user-group
   account-service-type
   (list
    (user-group
     (name "spice")
     (system? #t)))))


;;; Host-specific utilities

(define-public %lyra-ssh-key
  (plain-file
   "lyra.pub"
   "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBN0KzBIzlhPkr3BhcuKt9ki6iYyMS97hpAEFIrNCa9O root@lyra\n"))

(define-public %lyra-signing-key
  (project-file "src/dotfiles/keys/lyra.pub"))

(define lyra-mapped-devices
  (list
   (mapped-device
    (source
     (uuid "0f74821b-da48-4f0c-9f94-f39e646da1bf"))
    (target "system-root")
    (type luks-device-mapping))))

(define lyra-file-systems
  (list
   (file-system
     (mount-point "/")
     (device "/dev/mapper/system-root")
     (type "ext4")
     (dependencies lyra-mapped-devices))
   (file-system
     (device "/dev/nvme0n1p1")
     (mount-point "/boot/efi")
     (type "vfat"))))

(define lyra-udev-rules
  (list
   (udev-rule
    "50-spice.rules"
    (string-append
     "SUBSYSTEM==\"usb\", GROUP=\"spice\", MODE=\"0660\""
     "\n"
     "SUBSYSTEM==\"usb_device\", GROUP=\"spice\", MODE=\"0660\""))
   (udev-rule
    "90-backlight.rules"
    (string-append
     "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
     "RUN+=\"/run/current-system/profile/bin/chgrp "
     "video /sys/class/backlight/%k/brightness\""
     "\n"
     "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
     "RUN+=\"/run/current-system/profile/bin/chmod "
     "g+w /sys/class/backlight/%k/brightness\""))))

(define lyra-custom-services
  (list
   extra-user-groups-service
   extra-etc-files-service
   (service syncthing-service-type
            (syncthing-configuration (user "vega")))
   (service spice-vdagent-service-type)
   (service virtlog-service-type)
   (service whoogle-service-type)
   (service libvirt-service-type
            (libvirt-configuration
             (unix-sock-group "libvirt")
             (tls-port "16555")))
   (service openssh-service-type
            (openssh-configuration
             (openssh (@ (gnu packages ssh) openssh-sans-x))
             (password-authentication? #f)
             (permit-root-login 'prohibit-password)
             (authorized-keys
              `(("root" ,%hydri-ssh-key)
                ("vega" ,%hydri-ssh-key ,%default-ssh-key)))))
   (service qemu-binfmt-service-type
            (qemu-binfmt-configuration
             (platforms (lookup-qemu-platforms "aarch64"))))
   (service screen-locker-service-type
                  (screen-locker-configuration
                   "slock" (file-append
                            (@ (gnu packages suckless)
                               slock) "/bin/slock") #f))
   (service kernel-module-loader-service-type
            '("ddcci" "ddcci_backlight"))))


;;; Host-specific features

(define-public %lyra-features
  (list
   (feature-kernel
    #:kernel %default-kernel
    #:firmware (list (@ (nongnu packages linux) linux-firmware)
                     (@ (nongnu packages linux) sof-firmware))
    #:kernel-loadable-modules
    (strings->packages "ddcci-driver-linux" "v4l2loopback-linux-module"))
   (feature-host-info
    #:host-name "lyra"
    #:timezone %default-timezone)
   (feature-postgresql
    #:postgresql-roles
    (list
     (postgresql-role
      (name "vega")
      (create-database? #t))))
   (feature-custom-services
    #:system-services lyra-custom-services)
   (feature-base-services
    #:udev-rules lyra-udev-rules
    #:guix-substitute-urls
    (list "https://substitutes.nonguix.org")
    #:guix-authorized-keys
    (list %nonguix-signing-key %hydri-signing-key))
   (feature-bootloader
    #:bootloader-configuration
    (bootloader-configuration
     (bootloader grub-efi-bootloader)
     (targets '("/boot/efi"))
     (timeout 3)))
   (feature-file-systems
    #:mapped-devices lyra-mapped-devices
    #:file-systems lyra-file-systems)
   (feature-base-packages
    #:base-home-packages '()
    #:system-packages
    (strings->packages
     "emacs-no-x" "git" "curl"))))
