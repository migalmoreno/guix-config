(define-module (conses home services gtk)
  #:use-module (rde serializers css)
  #:use-module (rde serializers ini)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services utils)
  #:use-module (gnu home-services-utils)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:export (home-gtk-service-type
            home-gtk-configuration))

(define (maybe-string? x)
  (or (string? x) (not x)))

(define (maybe-alist? x)
  (or (alist? x) (not x)))

(define-maybe/no-serialization maybe-string)
(define-maybe/no-serialization maybe-alist)

(define-configuration/no-serialization home-gtk-configuration
  (settings
   (alist '())
   "Alist of pairs that set GTK global settings.")
  (theme
   (maybe-alist '())
   "List of CSS rules which set the GTK theme.")
  (default-cursor
   (maybe-string #f)
   "Name of the default cursor theme to use."))

(define (home-gtk-files-service config)
  (define (boolean->one-or-zero bool)
    (if (eq? bool #t) 1 0))

  (define (serialize-field key val)
    (let ((value (cond
                  ((boolean? val) (boolean->one-or-zero val))
                  (else val))))
      (format #f "~a = ~a\n" key value)))

  (append
   (if (home-gtk-configuration-default-cursor config)
       (list
        `(".icons/default/index.theme"
          ,(mixed-text-file
            "index.theme"
            (generic-serialize-ini-config
             #:serialize-field serialize-field
             #:fields `(("Icon Theme"
                         ((Inherits . ,(home-gtk-configuration-default-cursor config)))))))))
       '())
   (append
    (list
     `(".config/gtk-3.0/settings.ini"
       ,(mixed-text-file
         "settings.ini"
         (generic-serialize-ini-config
          #:serialize-field serialize-field
          #:fields (home-gtk-configuration-settings config)))))
    (if (home-gtk-configuration-theme config)
        (list
         `(".config/gtk-3.0/gtk.css"
           ,(apply mixed-text-file
                   "gtk.css"
                   (serialize-css-config (home-gtk-configuration-theme config)))))
        '()))))

(define home-gtk-service-type
  (service-type
   (name 'home-gtk)
   (extensions
    (list
     (service-extension
      home-files-service-type
      home-gtk-files-service)))
   (description "Configure GTK settings and theme.")
   (default-value (home-gtk-configuration))))
