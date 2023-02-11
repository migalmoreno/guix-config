(define-module (rde features android)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (conses packages android)
  #:use-module (conses packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages android)
  #:use-module (gnu packages check)
  #:use-module (guix gexp)
  #:export (feature-android))

(define* (feature-android
          #:key
          (emacs-fdroid emacs-fdroid))
  "Set up Android tooling."
  (ensure-pred file-like? emacs-fdroid)

  (define f-name 'android)

  (define (get-home-services config)
    "Return home services related to Android tools."
    (list
     (simple-service
      'add-android-packages
      home-profile-service-type
      (list adb fastboot payload-dumper fdroidcl))
     (simple-service
      'add-android-envs
      home-environment-variables-service-type
      `(("ADB_VENDOR_KEYS" . "$XDG_CONFIG_HOME/android/adb")))
     (rde-elisp-configuration-service
      f-name
      config
      `((with-eval-after-load 'fdroid-autoloads
          (fdroid-default-keybindings))
        (with-eval-after-load 'fdroid
          (setq fdroid-log-events t)
          (setq fdroid-sans-device t)))
      #:elisp-packages (list emacs-fdroid))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
