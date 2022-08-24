(define-module (quasar home services android)
  #:use-module (conses home services emacs)
  #:use-module (conses packages emacs-xyz)
  #:use-module (conses packages android)
  #:use-module (gnu home services)
  #:use-module (gnu home-services base)
  #:use-module (gnu packages android)
  #:use-module (gnu packages check)
  #:export (fdroid-service
            android-service))

(define (fdroid-service)
  (list
   (home-generic-service 'home-fdroid-packages #:packages (list fdroidcl))
   (elisp-configuration-service
    `((fdroid-default-keybindings)
      (with-eval-after-load 'fdroid
        (custom-set-variables
         '(fdroid-log-events t)
         '(fdroid-sans-device t))))
    #:elisp-packages (list emacs-fdroid))))

(define (android-service)
  (list
   (home-generic-service
    'home-android-packages
    #:packages (list adb fastboot payload-dumper))
   (simple-service 'add-android-envs
                   home-environment-variables-service-type
                   `(("ANDROID_EMULATOR_HOME"
                      . "$XDG_DATA_HOME/android/sdk/emulator")
                     ("ANDROID_SDK_ROOT"
                      . "$XDG_DATA_HOME/android/sdk")
                     ("ANDROID_AVD_HOME"
                      . "$XDG_DATA_HOME/android/avd")
                     ("ADB_VENDOR_KEYS"
                      . "$XDG_CONFIG_HOME/android/adb")))))
