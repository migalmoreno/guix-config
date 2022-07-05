(define-module (quasar home services xdg)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home-services base)
  #:export (xdg-service))

(define (xdg-service)
  (list
   (home-generic-service 'home-xdg-packages #:packages (list xdg-utils xdg-user-dirs))
   (service home-xdg-mime-applications-service-type
            (home-xdg-mime-applications-configuration
             (default
               '((application/msword . libreoffice.desktop)
                 (application/vnd.openxmlformats-officedocument.wordprocessingml.document
                  . libreoffice.desktop)
                 (application/vnd.ms-excel . libreoffice.desktop)
                 (application/vnd.ms-powerpoint . libreoffice.desktop)
                 (application/vnd.openxmlformats-officedocument.presentationml.presentation
                  . libreoffice.desktop)))))
   (service home-xdg-user-directories-service-type
            (home-xdg-user-directories-configuration
             (desktop "$HOME")
             (documents "$HOME/documents")
             (download "$HOME/downloads")
             (music "$HOME/music")
             (pictures "$HOME/pictures")
             (publicshare "$HOME")
             (videos "$HOME/videos")
             (templates "$HOME")))
   (service home-xdg-base-directories-service-type)))
