(define-module (rde home services scheme)
  #:use-module (rde serializers lisp)
  #:use-module (gnu home services)
  #:use-module (gnu home services utils)
  #:use-module (guix packages)
  #:use-module (gnu packages guile)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:export (home-guix-service-type
            home-guix-configuration
            home-guile-service-type
            home-guile-configuration))

(define serialize-alist empty-serializer)
(define (serialize-list field-name val)
  #~(string-append
     #$@(interpose
         (map maybe-object->string val)
         "\n" 'suffix)))

(define-public guile-config? sexp-config?)
(define-public serialize-guile-config serialize-sexp-config)

(define-configuration home-guix-configuration
  (channels
   (guile-config '())
   "List of expressions as per @{guile-config} that will place Guix's channel file
in @file{channels.scm}.")
  (shell-authorized-directories
   (list '())
   "List of directory entries where a guix shell command can load a @file{guix.scm} or
@file{manifest.scm}.")
  (envs
   (alist '())
   "Association list of environment variables to set for Guix. The key of the pair will
automatically be prepended with ``@code{GUIX_}'', meaning that @code{profile} will result
in ``@code{GUIX_PROFILE}''."))

(define (home-guix-environment-variables-service config)
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

(define (home-guix-files-service config)
  (define (filter-fields field)
    (filter-configuration-fields home-guix-configuration-fields
                                 (list field)))

  (define (serialize-field field)
    (serialize-configuration
     config
     (filter-fields field)))

  (append
   (if (null? (home-guix-configuration-channels config))
       '()
       (list
        `("guix/channels.scm"
          ,(mixed-text-file
            "channels.scm"
            (serialize-field 'channels)))))
   (if (null? (home-guix-configuration-shell-authorized-directories config))
       '()
       (list
        `("guix/shell-authorized-directories"
          ,(mixed-text-file
            "shell-authorized-directories"
            (serialize-field 'shell-authorized-directories)))))))

(define home-guix-service-type
  (service-type
   (name 'home-guix)
   (extensions
    (list
     (service-extension
      home-xdg-configuration-files-service-type
      home-guix-files-service)
     (service-extension
      home-environment-variables-service-type
      home-guix-environment-variables-service)))
   (default-value (home-guix-configuration))
   (description "Add user configuration files for Guix.")))

(define (generate-home-guix-documentation)
  (generate-documentation
   `((home-guix-configuration
      ,home-guix-configuration-fields))
   'home-guix-configuration))

(define-configuration home-guile-configuration
  (guile
    (package guile-3.0)
    "The @code{guile} package to use")
  (config
   (guile-config '())
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

(define (home-guile-environment-variables-service config)
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

(define (home-guile-files-service config)
  (define (filter-fields field)
    (filter-configuration-fields home-guile-configuration-fields
                                 (list field)))

  (define (serialize-field field)
    (serialize-configuration
     config
     (filter-fields field)))

  (filter
   (compose not null?)
   (list
    (if (null? (home-guile-configuration-config config))
        '()
        `(".guile"
          ,(mixed-text-file
            "guile"
            (serialize-field 'config)))))))

(define (home-guile-profile-service config)
  (list (home-guile-configuration-guile config)))

(define home-guile-service-type
  (service-type
   (name 'home-guile)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      home-guile-profile-service)
     (service-extension
      home-files-service-type
      home-guile-files-service)
     (service-extension
      home-environment-variables-service-type
      home-guile-environment-variables-service)))
   (default-value (home-guile-configuration))
   (description "Set up Guile user settings.")))

(define (generate-home-guile-documentation)
  (generate-documentation
   `((home-guile-configuration
      ,home-guile-configuration-fields))
   'home-guile-configuration))
