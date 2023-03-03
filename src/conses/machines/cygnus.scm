(define-module (conses machines cygnus)
  #:use-module (conses hosts cygnus)
  #:use-module (gnu machine)
  #:use-module (gnu machine ssh)
  #:use-module (gnu system install))

(define-public %machine
  (machine
   (operating-system installation-os)
   (environment managed-host-environment-type)
   (configuration
    (machine-ssh-configuration
     (host-name (getenv "CYGNUS_HOST"))
     (system "x86_64-linux")
     (host-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN42jJl2okXYMpbZUZ0gjfJi3OtRRdAYIQH73zVoW7D+")
     (allow-downgrades? #t)
     (user "root")
     (port 22)))))
