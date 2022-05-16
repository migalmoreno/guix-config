(define-module (efimerspan home-services lisp)
  #:use-module (gnu services configuration)
  #:use-module (gnu services)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (gnu home services)
  #:use-module (gnu packages lisp)
  #:use-module (efimerspan serializers lisp)
  #:export (home-lisp-configuration
            home-lisp-service-type))

(define-configuration home-lisp-configuration
  (package
    (package sbcl)
    "The Lisp implementation to use.")
  (slynk-lisp
   (lisp-config '())
   "List of expressions, where each expression can be Sexp or a Gexp. Sexp is a Lisp form.
Strings don't require any conversion, but booleans do.
Sexps can contain file-like objects, a string with path to a corresponding file in the store.
Gexps should be strings and their value will be appended to the resulting Lisp file.

The list of expressions will be interposed with \n and everything will end up in the
@file{slynk.lisp}.")
  (sbclrc-lisp
   (lisp-config '())
   "As per @code{slynk-config}, but everything will be placed in @file{sbclrc.lisp}."))

(define (add-lisp-configuration-files config)
  (define (filter-fields field)
    (filter-configuration-fields home-lisp-configuration-fields
                                 (list field)))

  (define (serialize-field field)
    (serialize-configuration
     config
     (filter-fields field)))

  (define (file-if-not-empty field)
    (let ((filename (string-append
                     (string-drop-right (symbol->string field) 5)
                     ".lisp"))
          (field-obj (car (filter-fields field))))
      (when (not (null? ((configuration-field-getter field-obj) config)))
        `(,filename
          ,(mixed-text-file
            filename
            (serialize-field field))))))

  (filter
   (compose not null?)
   (list
    (file-if-not-empty 'sbclrc-lisp)
    (file-if-not-empty 'slynk-lisp))))

(define (lisp-profile-service config)
  (list (home-lisp-configuration-package config)))

(define home-lisp-service-type
  (service-type
   (name 'home-lisp)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      lisp-profile-service)
     (service-extension
      home-files-service-type
      add-lisp-configuration-files)))
   (default-value (home-lisp-configuration))
   (description "Configures Lisp-related settings.")))

(define (generate-home-lisp-documentation)
  (generate-documentation
   `((home-lisp-configuration
      ,home-lisp-configuration-fields))
   'home-lisp-configuration))
