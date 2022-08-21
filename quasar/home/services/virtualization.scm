(define-module (quasar home services virtualization)
  #:use-module (conses home services emacs)
  #:use-module (gnu home-services base)
  #:use-module (gnu home services xdg)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages emacs-xyz)
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
                                          wireguard-tools))
   (simple-service 'home-virtualization-xdg
                   home-xdg-mime-applications-service-type
                   (home-xdg-mime-applications-configuration
                    (desktop-entries
                     (list
                      (xdg-desktop-entry
                       (file "virt-manager")
                       (name "Virtual Machine Manager")
                       (type 'application)
                       (config
                        '((exec . "virt-manager --connect qemu:///session"))))))))
   (elisp-configuration-service
    '((add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
      (put 'dockerfile-image-name 'safe-local-variable 'stringp))
    #:elisp-packages (list emacs-dockerfile-mode))))
