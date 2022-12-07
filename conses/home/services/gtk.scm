(define-module (conses home services gtk)
  #:use-module (rde serializers css)
  #:use-module (rde serializers ini)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services utils)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:export (home-gtk3-service-type
            home-gtk3-configuration))

(define (maybe-string? x)
  (or (string? x) (not x)))

(define (maybe-alist? x)
  (or (alist? x) (not x)))

(define-maybe/no-serialization maybe-string)
(define-maybe/no-serialization maybe-alist)

(define-configuration/no-serialization home-gtk3-configuration
  (settings
   (alist '())
   "Alist of pairs that set GTK global settings.")
  (theme
   (maybe-alist '())
   "List of CSS rules which set the GTK theme.")
  (default-cursor
   (maybe-string #f)
   "Name of the default cursor theme to use."))

(define (home-gtk3-files-service config)
  (append
   (if (home-gtk3-configuration-default-cursor config)
       (list
        `(".icons/default/index.theme"
          ,(apply mixed-text-file
                  "index.theme"
                  (serialize-ini-config
                   `((#{Icon Theme}#
                      ((Inherits . ,#~(format #f "~a" #$(home-gtk3-configuration-default-cursor config))))))))))
       '())
   (if (home-gtk3-configuration-theme config)
       (list
        `(".config/gtk-3.0/gtk.css"
          ,(apply mixed-text-file
                  "gtk.css"
                  (serialize-css-config (home-gtk3-configuration-theme config)))))
       '())
   (list
    `(".config/gtk-3.0/settings.ini"
      ,(apply mixed-text-file
              "settings.ini"
              (serialize-ini-config
               (home-gtk3-configuration-settings config)))))))

(define home-gtk3-service-type
  (service-type
   (name 'home-gtk)
   (extensions
    (list
     (service-extension
      home-files-service-type
      home-gtk3-files-service)))
   (description "Configure GTK settings and theme.")
   (default-value (home-gtk3-configuration))))
