(define-module (quasar deployment cygnus)
  #:use-module (quasar home)
  #:use-module (quasar system cygnus)
  #:use-module (gnu machine)
  #:use-module (gnu machine ssh))

(list
 (machine
  (operating-system %system/cygnus)
  (environment managed-host-environment-type)
  (configuration
   (machine-ssh-configuration
    (host-name (getenv "CYGNUS_IP"))
    (system "x86_64-linux")
    (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN42jJl2okXYMpbZUZ0gjfJi3OtRRdAYIQH73zVoW7D+")
    (allow-downgrades? #t)
    (user "root")
    (port 22)))))
