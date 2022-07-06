(define-module (quasar home services bittorrent)
  #:use-module (conses home services emacs)
  #:use-module (conses home services bittorrent)
  #:use-module (gnu services)
  #:use-module (gnu packages emacs-xyz)
  #:export (transmission-service))

(define (transmission-service)
  (list
   (service home-transmission-service-type)
   (elisp-configuration-service
    `((let ((map mode-specific-map))
        (define-key map "st" 'transmission)
        (define-key map "sa" 'transmission-add)))
    #:elisp-packages (list emacs-transmission))))
