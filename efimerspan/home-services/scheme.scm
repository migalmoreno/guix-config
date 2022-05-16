(define-module (efimerspan home-services scheme)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home-services-utils)
  #:use-module (gnu home services utils)
  #:use-module (guix packages)
  #:use-module (gnu packages guile)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (efimerspan serializers lisp)
  #:export (home-guix-service-type
            home-guix-configuration
            home-guile-service-type
            home-guile-configuration))

(define serialize-alist empty-serializer)

(define-configuration home-guix-configuration
  (channels
   (lisp-config '())
   "List of expressions as per @{lisp-config} that will place Guix's channel file
in @file{channels.scm}.")
  (envs
   (alist '())
   "Association list of environment variables to set for Guix. The key of the pair will
automatically be prepended with ``@code{GUIX_}'', meaning that @code{profile} will result
in ``@code{GUIX_PROFILE}''."))

(define (add-guix-environment-variables config)
  (define (serialize-field field-name val)
    (cons
     (string-append "GUIX_"
                    (object->snake-case-string field-name 'upper))
     (match val
       ((? list? v) (string-join v " "))
       (v (maybe-object->string v)))))

  (map (match-lambda
         ((car . cdr) (serialize-field car cdr)))
       (home-guix-configuration-envs config)))

(define (add-guix-configuration-files config)
  (define (filter-fields field)
    (filter-configuration-fields home-guix-configuration-fields
                                 (list field)))

  (define (serialize-field field)
    (serialize-configuration
     config
     (filter-fields field)))

  (filter
   (compose not null?)
   `(("guix/channels.scm"
      ,(mixed-text-file
        "channels.scm"
        (serialize-field 'channels))))))

(define home-guix-service-type
  (service-type
   (name 'home-guix)
   (extensions
    (list
     (service-extension
      home-xdg-configuration-files-service-type
      add-guix-configuration-files)
     (service-extension
      home-environment-variables-service-type
      add-guix-environment-variables)))
   (default-value (home-guix-configuration))
   (description "Adds configuration files for Guix.")))

(define-configuration home-guile-configuration
  (package
    (package guile-3.0)
    "The @code{guile} package to use")
  (config
   (lisp-config '())
   "List of expressions, where each expression can be Sexp or a Gexp. Sexp is a Lisp form.
Strings don't require any conversion, but booleans do.
Sexps can contain file-like objects, a string with path to a corresponding file in the store.
Gexps should be strings and their value will be appended to the resulting Lisp file.

The list of expressions will be interposed with \n and everything will end up in the
@file{.guile} file.")
  (envs
   (alist '())
   "Association list of environment variables to set for Guile. The key of the pair will
automatically be prepended with ``@code{GUILE_}'', meaning that @code{load-path} will result
in ``@code{GUILE_LOAD_PATH}''."))

(define (add-guile-environment-variables config)
  (define (serialize-field field-name val)
    (cons
     (string-append "GUILE_"
                    (object->snake-case-string field-name 'upper))
     (match val
       ((? list? v) (string-join v " "))
       (v (maybe-object->string v)))))

  (map (match-lambda
         ((car . cdr) (serialize-field car cdr)))
       (home-guile-configuration-envs config)))

(define (add-guile-configuration-files config)
  (define (filter-fields field)
    (filter-configuration-fields home-guile-configuration-fields
                                 (list field)))

  (define (serialize-field field)
    (serialize-configuration
     config
     (filter-fields field)))

  (filter
   (compose not null?)
   `((".guile"
      ,(mixed-text-file
        ".guile"
        (serialize-field 'config))))))

(define (guile-profile-service config)
  (list (home-guile-configuration-package config)))

(define home-guile-service-type
  (service-type
   (name 'home-guile)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      guile-profile-service)
     (service-extension
      home-files-service-type
      add-guile-configuration-files)
     (service-extension
      home-environment-variables-service-type
      add-guile-environment-variables)))
   (default-value (home-guile-configuration))
   (description "Sets up Guile user settings.")))
