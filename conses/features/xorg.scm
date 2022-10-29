(define-module (conses features xorg)
  #:use-module (conses home services xorg)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home-services xorg)
  #:use-module (gnu home-services base)
  #:use-module (guix gexp)
  #:export (feature-cursor
            feature-xorg))

(define* (feature-cursor
          #:key
          (unclutter unclutter))
  "Configure the graphical cursor."
  (ensure-pred file-like? unclutter)

  (define f-name 'cursor)

  (define (get-home-services config)
    "Return home services related to the cursor."
    (list
     (service home-unclutter-service-type
              (home-unclutter-configuration
               (unclutter unclutter)
               (seconds 2)))
     (rde-elisp-configuration-service
      f-name
      config
      `((pixel-scroll-mode)
        (with-eval-after-load 'mouse
          (setq mouse-yank-at-point nil))
        (with-eval-after-load 'mwheel
          (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
          (setq mouse-wheel-progressive-speed nil)
          (setq mouse-wheel-follow-mouse t)
          (setq scroll-conservatively 100)
          (setq mouse-autoselect-window nil)
          (setq what-cursor-show-names t)
          (setq focus-follows-mouse t))
        (with-eval-after-load 'frame
          (setq-default cursor-in-non-selected-windows nil)
          (blink-cursor-mode 0))))))

  (feature
   (name f-name)
   (values `((cursor . #t)))
   (home-services-getter get-home-services)))

(define* (feature-xorg
          #:key
          (xrdb xrdb))
  "Configure the Xorg display server."
  (ensure-pred file-like? xrdb)

  (define (get-home-services config)
    "Return home services related to Xorg."
    (define theme-name (let ((cursor (get-value 'gtk-cursor config)))
                         (if cursor
                             (theme-name cursor)
                             "")))
    (define font-sans-serif (get-value 'font-sans-serif config))

    (list
     (simple-service
      'home-xorg-profile-service
      home-profile-service-type
      (list xinit
            xev
            xprop
            xset
            xsetroot
            xorg-server
            xss-lock
            xsecurelock
            xf86-input-libinput))
     (simple-service
      'home-xorg-shepherd-service
      home-shepherd-service-type
      (list
       (shepherd-service
        (provision '(screensaver))
        (requirement '(dbus))
        (one-shot? #t)
        (start #~(make-system-constructor "xset -dpms s off")))
       (shepherd-service
        (provision '(xsetroot))
        (requirement '(dbus))
        (one-shot? #t)
        (start #~(make-forkexec-constructor (list "xsetroot" "-cursor_name" "left_ptr"))))))
     (service home-xresources-service-type
              (home-xresources-configuration
               (package xrdb)
               (config
                `((Xcursor.theme . ,theme-name)
                  (Emacs.font . ,font-sans-serif)
                  (Xcursor.size . 16)
                  (Xft.autohint . #t)
                  (Xft.antialias . #t)
                  (Xft.hinting . #t)
                  (Xft.hintstyle . hintfull)
                  (Xft.rgba . none)
                  (Xft.lcdfilter . lcddefault)
                  (Xft.dpi . 110)))))))

  (feature
   (name 'xorg)
   (values `((xorg . #t)))
   (home-services-getter get-home-services)))
