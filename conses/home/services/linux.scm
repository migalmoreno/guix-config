(define-module (conses home services linux)
  #:use-module (conses home services glib)
  #:use-module (conses packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix packages)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:use-module (gnu services shepherd)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages freedesktop)
  #:export (home-pipewire-service-type
            home-udiskie-service-type))

(define-configuration/no-serialization home-pipewire-configuration
  (package
   (package pipewire-0.3)
   "Pipewire package to use."))

(define (pipewire-shepherd-service config)
  (list
   (shepherd-service
    (provision '(home-pipewire))
    (requirement '(home-dbus))
    (stop #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list #$(file-append (home-pipewire-configuration-package config) "/bin/pipewire"))
              #:environment-variables
              (append (list "DISABLE_RTKIT=1")
                      (default-environment-variables)))))
   (shepherd-service
    (provision '(home-wireplumber))
    (requirement '(home-pipewire))
    (stop #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list #$(file-append wireplumber "/bin/wireplumber"))
              #:log-file (string-append
                          (or (getenv "XDG_LOG_HOME")
                              (format #f "~a/.local/var/log"
                                      (getenv "HOME")))
                          "/wireplumber.log")
              #:environment-variables
              (append (list "DISABLE_RTKIT=1")
                      (default-environment-variables)))))
   (shepherd-service
    (provision '(home-pipewire-pulse))
    (requirement '(home-pipewire))
    (stop #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list #$(file-append (home-pipewire-configuration-package config) "/bin/pipewire-pulse"))
              #:log-file (string-append
                          (or (getenv "XDG_LOG_HOME")
                              (format #f "~a/.local/var/log"
                                      (getenv "HOME")))
                          "/pipewire-pulse.log")
              #:environment-variables
              (append (list "DISABLE_RTKIT=1")
                      (default-environment-variables)))))))

(define (pipewire-profile-service config)
  (list (home-pipewire-configuration-package config)
        wireplumber))

(define home-pipewire-service-type
  (service-type
   (name 'home-pipewire)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      pipewire-profile-service)
     (service-extension
      home-shepherd-service-type
      pipewire-shepherd-service)))
   (description "Add and configure the Pipewire daemon.")
   (default-value (home-pipewire-configuration))))

(define-configuration/no-serialization home-udiskie-configuration
  (package
    (package udiskie)
    "The udiskie package to use.")
  (notify?
   (boolean #f)
   "Whether to enable pop-up notifications."))

(define (udiskie-profile-service config)
  (list (home-udiskie-configuration-package config)))

(define (udiskie-shepherd-service config)
  (list
   (shepherd-service
    (provision '(home-udiskie))
    (stop #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list
               #$(file-append (home-udiskie-configuration-package config) "/bin/udiskie")
               (if #$(home-udiskie-configuration-notify? config)
                   "-n"
                   "-N")))))))

(define home-udiskie-service-type
  (service-type
   (name 'udiskie)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      udiskie-profile-service)
     (service-extension
      home-shepherd-service-type
      udiskie-shepherd-service)))
   (description "Sets a udiskie daemon to automount removable media.")
   (default-value (home-udiskie-configuration))))
