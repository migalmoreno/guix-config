(define-module (quasar home services xdg)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home-services base)
  #:use-module (srfi srfi-28)
  #:export (xdg-service))

(define (xdg-service)
  (list
   (home-generic-service 'home-xdg-packages #:packages (list xdg-utils xdg-user-dirs))
   (service home-xdg-mime-applications-service-type
            (home-xdg-mime-applications-configuration
             (default
               '((x-scheme-handler/http . nyxt.desktop)
                 (x-scheme-handler/https . nyxt.desktop)
                 (x-scheme-handler/about . nyxt.desktop)
                 (x-scheme-handler/unknown . emacsclient.desktop)
                 (image/png . emacsclient.desktop)
                 (image/jpg . emacsclient.desktop)
                 (image/jpeg . emacsclient.desktop)
                 (application/pdf . emacsclient.desktop)
                 (application/desktop . emacsclient.desktop)
                 (application/json . emacsclient.desktop)
                 (text/javascript . emacsclient.desktop)
                 (application/msword . libreoffice.desktop)
                 (application/vnd.openxmlformats-officedocument.wordprocessingml.document
                  . libreoffice.desktop)
                 (application/vnd.ms-excel . libreoffice.desktop)
                 (application/vnd.ms-powerpoint . libreoffice.desktop)
                 (application/vnd.openxmlformats-officedocument.presentationml.presentation
                  . libreoffice.desktop)
                 (text/html . emacsclient.desktop)
                 (text/css . emacsclient.desktop)
                 (video/mp4 . mpv.desktop)
                 (video/mkv . mpv.desktop)
                 (audio/mp3 . mpv.desktop)))
             (desktop-entries
              (list
               (xdg-desktop-entry
                (file "virt-manager")
                (name "Virtual Machine Manager")
                (type 'application)
                (config
                 `((exec . "virt-manager --connect qemu:///session"))))
               (let ((nyxt-file (format "~a/src/nyxt/build-scripts/nyxt.scm" (getenv "HOME"))))
                 (when (file-exists? nyxt-file)
                 (xdg-desktop-entry
                  (file "nyxt-dev")
                  (name "Nyxt Development")
                  (type 'application)
                  (config
                   `((exec . ,(format "guix shell -D -f ~a -- ~a" nyxt-file
                                      (string-append ((compose dirname dirname) nyxt-file) "/nyxt"))))))))))))
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
