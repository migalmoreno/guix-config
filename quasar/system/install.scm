(define-module (quasar system install)
  #:use-module (gnu system)
  #:use-module (gnu services)
  #:use-module (gnu system install)
  #:use-module (gnu packages base)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages version-control)
  #:use-module (guix gexp)
  #:use-module (nongnu packages linux)
  #:export (installation-os-nonfree))

(define installation-os-nonfree
  (operating-system
    (inherit installation-os)
    (kernel linux)
    (kernel-arguments '("quiet" "modprobe.blacklist=radeon" "net.ifnames=0"))
    (firmware (list linux-firmware))
    (packages
     (append
       (list
        gnu-make
        gnupg
        emacs-no-x
        git)
       (operating-system-packages installation-os)))
    (services
     (append
       (list
        (simple-service 'channels
                        etc-service-type
                        `(("channels.scm"
                           ,(local-file "../../channels")))))
       (operating-system-user-services installation-os)))))

installation-os-nonfree
