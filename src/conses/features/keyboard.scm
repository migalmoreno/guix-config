(define-module (conses features keyboard)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (rde home services keyboard)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages avr)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages embedded)
  #:use-module (gnu packages flashing-tools)
  #:use-module (gnu system keyboard)
  #:use-module (gnu home-services keyboard)
  #:export (feature-qmk
            feature-keyboard))

(define* (feature-qmk #:key keyboard keymap)
  "Configure QMK, the open-source keyboard firmware."
  (ensure-pred maybe-string? keyboard)
  (ensure-pred maybe-string? keymap)

  (define (get-home-services config)
    "Return home services related to QMK."
    (list
     (simple-service
      'home-qmk-profile-service
      home-profile-service-type
      (list avrdude
            arm-none-eabi-toolchain-6
            avr-toolchain
            dfu-util
            dfu-programmer))
     (service
      home-qmk-service-type
      (home-qmk-configuration
       (settings
        `((user
           ((qmk-home . "~/.local/share/qmk_firmware")
            (keyboard . ,keyboard)
            (keymap . ,keymap)))
          (general
           ((verbose . #f)
            (datetime-fmt . "%Y-%m-%d %H:%M:%S")
            (log-fmt . "%(levelname)s %(message)s")
            ,(cons 'log-file-fmt
                   "[%(asctime)s] [file:%(pathname)s] [line:%(lineno)d] %(message)s")
            (color . #t)))))))))

  (feature
   (name 'qmk)
   (values
    (append
      `((qmk . #t))
      (make-feature-values keyboard keymap)))
   (home-services-getter get-home-services)))

(define* (feature-keyboard
          #:key
          keyboard-layout
          default-input-method
          (laptop-keyboard-remap? #t))
  "Configure the keyboard layout and input method."
  (ensure-pred keyboard-layout? keyboard-layout)
  (ensure-pred string? default-input-method)
  (ensure-pred boolean? laptop-keyboard-remap?)

  (define f-name 'keyboard)

  (define (get-home-services config)
    "Return home services related to keyboard settings."
    (append
     (list
      (simple-service
       'home-keyboard-packages
       home-profile-service-type
       (list setxkbmap xkeyboard-config))
      (rde-elisp-configuration-service
       f-name
       config
       `((define-key global-map (kbd "<print>") 'ignore)
         (with-eval-after-load 'mule-cmds
           (setq default-input-method ,default-input-method))))
      (service home-keyboard-service-type keyboard-layout))
     (if laptop-keyboard-remap?
         (list
          (service home-xmodmap-service-type
                   (home-xmodmap-configuration
                    (config
                     '((#(add mod4) . Print)
                       (clear lock)
                       (clear control)
                       (#(keycode 66) . Control_L)
                       (#(add control) . #(Control_L Control_R)))))))
         '())))

  (feature
   (name f-name)
   (values (make-feature-values keyboard-layout
                                default-input-method
                                laptop-keyboard-remap?))
   (home-services-getter get-home-services)))
