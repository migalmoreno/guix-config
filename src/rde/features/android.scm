(define-module (rde features android)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (conses packages android)
  #:use-module (conses packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages android)
  #:use-module (gnu packages check)
  #:export (feature-emacs-fdroid
            feature-android))

(define* (feature-emacs-fdroid
          #:key
          (emacs-fdroid emacs-fdroid))
  "Configure F-Droid integration for Emacs."
  (ensure-pred any-package? emacs-fdroid)

  (define emacs-f-name 'fdroid)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to F-Droid."
    (list
     (simple-service
      'home-fdroid-packages
      home-profile-service-type
      (list fdroidcl))
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load 'fdroid-autoloads
          (fdroid-default-keybindings))
        (with-eval-after-load 'fdroid
          (setq fdroid-log-events t)
          (setq fdroid-sans-device t)))
      #:elisp-packages (list emacs-fdroid))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-fdroid)))
   (home-services-getter get-home-services)))

(define (feature-android)
  "Set up Android tooling."

  (define (get-home-services config)
    "Return home services related to Android tooling."
    (list
     (simple-service
      'add-android-tools
      home-profile-service-type
      (list adb fastboot payload-dumper))
     (simple-service
      'add-android-envs
      home-environment-variables-service-type
      `(("ADB_VENDOR_KEYS" . "$XDG_CONFIG_HOME/android/adb")))))

  (feature
   (name 'android)
   (values `((android . #t)))
   (home-services-getter get-home-services)))
