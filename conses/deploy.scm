(define-module (conses deploy)
  #:use-module (conses system)
  #:use-module (gnu machine)
  #:use-module (gnu machine ssh))

(define-public %initial-machine
  (machine
   (operating-system %initial-os)
   (environment managed-host-environment-type)))
