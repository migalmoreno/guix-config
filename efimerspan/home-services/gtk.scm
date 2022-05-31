(define-module (efimerspan home-services gtk)
  #:use-module (rde serializers css)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home-services-utils)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:export (home-gtk-service-type
            home-gtk-configuration
            gtk-theme-light-p))

(define (gtk-theme-light-p)
  (if (getenv "GTK_THEME")
      (string= (getenv "GTK_THEME") ":light")
      #t))

(define-configuration/no-serialization home-gtk-configuration
  (settings
   (alist '())
   "Alist of pairs that set GTK global settings.")
  (theme
   (alist '())
   "List of CSS rules which set the GTK theme."))

(define (add-gtk-settings config)
  (define (boolean->one-or-zero bool)
    (if (eq? bool #t) 1 0))

  (define (serialize-term term)
    (match term
      ((? symbol? e) (symbol->string e))
      ((? number? e) (number->string e))
      ((? boolean? e) (boolean->one-or-zero e))
      (e e)))

  (define (serialize-field key val)
    (let ((name (serialize-term key))
          (value (serialize-term val)))
      (format #f "~a = ~a\n" name value)))

  (list
   `("gtk-3.0/settings.ini"
     ,(mixed-text-file
       "settings.ini"
       (generic-serialize-ini-config
        #:serialize-field serialize-field
        #:fields (home-gtk-configuration-settings config))))
   `("gtk-3.0/gtk.css"
     ,(apply mixed-text-file
             "gtk.css"
             (serialize-css-config (home-gtk-configuration-theme config))))))

(define home-gtk-service-type
  (service-type
   (name 'home-gtk)
   (extensions
    (list
     (service-extension
      home-xdg-configuration-files-service-type
      add-gtk-settings)))
   (description "Configure GTK settings and theme.")
   (default-value (home-gtk-configuration))))
