(define-module (efimerspan home-services glib)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (gnu packages glib)
  #:export (home-dbus
            home-dbus-service-type))

(define (dbus-shepherd-service config)
  (list
   (shepherd-service
    (provision '(home-dbus))
    (stop #~(make-kill-destructor))
    (start #~(make-forkexec-constructor (list
                                         #$(file-append dbus "/bin/dbus-daemon")
                                         "--nofork"
                                         "--session"
                                         (string-append
                                          "--address=" "unix:path="
                                          (getenv "XDG_RUNTIME_DIR") "/bus")))))))

(define (dbus-environment-variables-service config)
  `(("DBUS_SESSION_BUS_ADDRESS" . "unix:path=$XDG_RUNTIME_DIR/bus")))

(define home-dbus-service-type
  (service-type
   (name 'home-dbus)
   (extensions
    (list
     (service-extension
      home-shepherd-service-type
      dbus-shepherd-service)
     (service-extension
      home-environment-variables-service-type
      dbus-environment-variables-service)))
   (default-value #f)
   (description "Set up a user-level D-Bus daemon.")))
