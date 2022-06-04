(define-module (quasar system lyra)
  #:use-module (quasar home)
  #:use-module (efimerspan serializers lisp)
  #:use-module (efimerspan system services web)
  #:use-module (guix packages)
  #:use-module (nongnu packages linux)
  #:use-module (flat packages emacs)
  #:use-module (srfi srfi-1)
  #:use-module (gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages certs)
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

(define %xorg-libinput-configuration
  "Section \"InputClass\"
  Identifier \"Touchpads\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsTouchpad \"on\"

  Option \"Tapping\" \"on\"
  Option \"TappingDrag\" \"on\"
  Option \"DisableWhileTyping\" \"on\"
  Option \"MiddleEmulation\" \"on\"
  Option \"ScrollMethdod\" \"twofinger\"
EndSection
Section \"InputClass\"
  Identifier \"Keyboards\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsKeyboard \"on\"
EndSection
")

(define %system/lyra
  (operating-system
    (kernel linux)
    (kernel-loadable-modules (list ddcci-driver-linux))
    (firmware (list linux-firmware sof-firmware))
    (locale "en_US.utf8")
    (timezone (getenv "LYRA_TIMEZONE"))
    (keyboard-layout (keyboard-layout "us"))
    (host-name "lyra")
    (groups
     (cons
      (user-group
       (name "spice")
       (system? #t))
      %base-groups))
    (users (cons* (user-account
                   (name "vega")
                   (comment "vega")
                   (group "users")
                   (home-directory "/home/vega")
                   (supplementary-groups
                    '("wheel"
                      "netdev"
                      "lp"
                      "audio"
                      "kvm"
                      "spice"
                      "libvirt"
                      "video")))
                  %base-user-accounts))
    (packages (append (list
                       emacs-exwm
                       emacs-no-x
                       nss-certs
                       cryptsetup
                       guix-simplyblack-sddm-theme
                       git
                       curl
                       gnu-make)
                      %base-packages))
    (services (cons* (service sddm-service-type
                              (sddm-configuration
                               (xorg-configuration
                                (xorg-configuration
                                 (keyboard-layout keyboard-layout)
                                 (extra-config (list
                                                %xorg-libinput-configuration))))

                               (theme "guix-simplyblack-sddm")))
                     (service kernel-module-loader-service-type
                              '("ddcci" "ddcci_backlight"))
                     (simple-service 'ddcci-config etc-service-type
                                     (list `("modprobe.d/ddcci.conf"
                                             ,%ddcci-config)))
                     (bluetooth-service #:auto-enable? #t)
                     (service virtlog-service-type)
                     (service whoogle-service-type)
                     (service libvirt-service-type
                              (libvirt-configuration
                               (unix-sock-group "libvirt")
                               (tls-port "16555")))
                     (extra-special-file "/usr/bin/env"
                                         (file-append coreutils "/bin/env"))
                     (extra-special-file "/bin/bash"
                                         (file-append bash "/bin/bash"))
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
    (bootloader
     (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets (list "/boot/efi"))
      (keyboard-layout keyboard-layout)))
    (mapped-devices
     (list (mapped-device
            (source
             (uuid "0f74821b-da48-4f0c-9f94-f39e646da1bf"))
            (target "system-root")
            (type luks-device-mapping))))
    (file-systems
     (cons* (file-system
              (mount-point "/")
              (device "/dev/mapper/system-root")
              (type "ext4")
              (dependencies mapped-devices))
            (file-system
              (device "/dev/nvme0n1p1")
              (mount-point "/boot/efi")
              (type "vfat"))
            %base-file-systems))))

%system/lyra
