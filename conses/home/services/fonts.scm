(define-module (conses home services fonts)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (home-font-service-type
            home-font-configuration
            font-spec
            make-font-spec
            font-spec?
            font-spec-package
            font-spec-family))

(define-record-type* <font-spec>
  font-spec make-font-spec
  font-spec?
  (package font-spec-package)
  (family font-spec-family))

(define (serialize-font-spec field-name val)
  (string-append "<alias>
<family>" (symbol->string field-name) "</family>
  <prefer>
    <family>" (font-spec-family val) "</family>
  </prefer>
</alias>
"))

(define-maybe font-spec)

(define-configuration home-font-configuration
  (sans-serif
   (maybe-font-spec)
   "Sans serif font.")
  (serif
   (maybe-font-spec)
   "Serif font.")
  (monospace
   (maybe-font-spec)
   "Monospace font."))

(define (home-font-files-service config)
  `(("fontconfig/conf.d/50-default-fonts.conf"
     ,(mixed-text-file
       "50-user.conf"
       "<?xml version='1.0'?>
<!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>
<fontconfig>
" (serialize-configuration config home-font-configuration-fields) "
</fontconfig>"))))

(define (home-font-profile-service config)
  (fold (lambda (field res)
          (let ((val ((configuration-field-getter field) config)))
            (if (eq? 'disabled val)
                res
                (cons (font-spec-package val) res))))
        '()
        home-font-configuration-fields))

(define home-font-service-type
  (service-type
   (name 'home-fonts)
   (extensions
    (list
     (service-extension
      home-xdg-configuration-files-service-type
      home-font-files-service)
     (service-extension
      home-profile-service-type
      home-font-profile-service)))
   (description "Set up font configuration for the current user.")
   (default-value (home-font-configuration))))
