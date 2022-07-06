(define-module (quasar home services keyboard)
  #:use-module (conses home services keyboard)
  #:use-module (conses home services emacs)
  #:use-module (gnu services)
  #:use-module (gnu system keyboard)
  #:use-module (gnu home-services keyboard)
  #:use-module (gnu packages flashing-tools)
  #:use-module (gnu packages embedded)
  #:use-module (gnu packages avr)
  #:use-module (gnu packages xorg)
  #:use-module (gnu home-services base)
  #:export (qmk-service
            keyboard-service))

(define (qmk-service)
  (list
   (home-generic-service 'home-qmk-packages
                         #:packages (list avrdude
                                          arm-none-eabi-toolchain-6
                                          avr-toolchain
                                          dfu-util
                                          dfu-programmer))
   (service home-qmk-service-type
            (home-qmk-configuration
             (config
              '((user
                 ((qmk-home . "~/.local/share/qmk_firmware")
                  (keyboard . dztech/dz65rgb/v1)
                  (keymap . custom)))
                (general
                 ((verbose . #f)
                  (datetime-fmt . "%Y-%m-%d %H:%M:%S")
                  (log-fmt . "%(levelname)s %(message)s")
                  (log-file-fmt . "[%(asctime)s] [file:%(pathname)s] [line:%(lineno)d] %(message)s")
                  (color . #t)))))))))

(define (keyboard-service)
  (list
   (home-generic-service 'home-keyboard-packages #:packages (list setxkbmap))
   (elisp-configuration-service
    `((define-key global-map (kbd "<print>") 'ignore)
      (with-eval-after-load 'mule-cmds
       (custom-set-variables
        '(default-input-method "spanish-keyboard")))))
   (service home-keyboard-service-type
            (keyboard-layout "us,es"
                             #:options '("grp:rctl_rshift_toggle"
                                         "caps:ctrl_modifier"
                                         "altwin:prtsc_rwin")))
   (service home-xmodmap-service-type
            (home-xmodmap-configuration
             (config
              '((#(add mod4) . Print)
                (clear lock)
                (clear control)
                (#(keycode 66) . Control_L)
                (#(add control) . #(Control_L Control_R))))))))
