(define-module (quasar home services virtualization)
  #:use-module (gnu home-services base)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages vpn)
  #:export (virtualization-service))

(define (virtualization-service)
  (list
   (home-generic-service 'home-virtualization-packages
                         #:packages (list virt-manager
                                          bridge-utils
                                          parted
                                          dosfstools
                                          wireguard-tools))))
