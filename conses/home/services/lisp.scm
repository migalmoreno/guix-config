(define-module (conses home services lisp)
  #:use-module (conses serializers base)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu packages lisp)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:export (home-lisp-configuration
            home-lisp-service-type))

(define-public lisp-config? sexp-config?)
(define-public serialize-lisp-config serialize-sexp-config)

(define-configuration home-lisp-configuration
  (lisp
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

(define (home-lisp-files-service config)
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
      (optional (not (null? ((configuration-field-getter field-obj) config)))
                `(,(string-append "." filename)
                  ,(mixed-text-file
                    filename
                    (serialize-field field))))))

  (filter
   (compose not null?)
   (list
    (file-if-not-empty 'sbclrc-lisp)
    (file-if-not-empty 'slynk-lisp))))

(define (home-lisp-profile-service config)
  (list (home-lisp-configuration-lisp config)))

(define home-lisp-service-type
  (service-type
   (name 'home-lisp)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      home-lisp-profile-service)
     (service-extension
      home-files-service-type
      home-lisp-files-service)))
   (default-value (home-lisp-configuration))
   (description "Configures Lisp-related settings.")))

(define (generate-home-lisp-documentation)
  (generate-documentation
   `((home-lisp-configuration
      ,home-lisp-configuration-fields))
   'home-lisp-configuration))
