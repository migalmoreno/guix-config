(define-module (conses home services web-browsers)
  #:use-module (conses serializers base)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages web-browsers)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:export (home-nyxt-configuration
            home-nyxt-service-type
            home-nyxt-extension))

(define packages? (list-of package?))
(define serialize-packages empty-serializer)
(define lisp-config? sexp-config?)
(define serialize-lisp-config serialize-sexp-config)

(define-configuration home-nyxt-configuration
  (nyxt
   (package nyxt)
   "The nyxt package to use.")
  (lisp-packages
   (packages '())
   "List of Lisp packages to install.")
  (config-lisp
   (lisp-config '())
   "List of expressions, where each expression can be Sexp or a Gexp. Sexp is a Lisp form.
Strings don't require any conversion, but booleans do.
Sexps can contain file-like objects, a string with path to a corresponding file in the store.
Gexps should be strings and their value will be appended to the resulting Lisp file.

The list of expressions will be interposed with \n and everything will end up in the
@file{config.lisp}.")
  (auto-mode-rules-lisp
   (lisp-config '())
   "List of @code{auto-mode} rules. See @code{config-lisp} for information on the format."))

(define-configuration home-nyxt-extension
  (lisp-packages
   (packages '())
   "List of additional Lisp packages.")
  (config-lisp
   (lisp-config '())
   "List of expressions to add to @file{config-lisp}. See
home-nyxt-service-type for more information."))

(define (home-nyxt-files-service config)
  (define (filter-fields field)
    (filter-configuration-fields home-nyxt-configuration-fields
                                 (list field)))

  (define (serialize-field field)
    (serialize-configuration
     config
     (filter-fields field)))

  (filter
   (compose not null?)
   `((".config/nyxt/config.lisp"
      ,(mixed-text-file
        "config.lisp"
        (serialize-field 'config-lisp)))
     (".local/share/nyxt/auto-mode-rules.lisp"
      ,(mixed-text-file
        "auto-mode-rules.lisp"
        (serialize-field 'auto-mode-rules-lisp))))))

(define (home-nyxt-extensions original-config extension-configs)
  (let ((extensions (reverse extension-configs)))
    (home-nyxt-configuration
     (inherit original-config)
     (lisp-packages
      (append (home-nyxt-configuration-lisp-packages original-config)
              (append-map
               home-nyxt-extension-lisp-packages extensions)))
     (config-lisp
      (append (home-nyxt-configuration-config-lisp original-config)
              (append-map
               home-nyxt-extension-config-lisp extensions))))))

(define (home-nyxt-profile-service config)
  (list (home-nyxt-configuration-nyxt config)))

(define home-nyxt-service-type
  (service-type
   (name 'home-nyxt)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      home-nyxt-profile-service)
     (service-extension
      home-files-service-type
      home-nyxt-files-service)))
   (compose identity)
   (extend home-nyxt-extensions)
   (default-value (home-nyxt-configuration))
   (description "Install and configure Nyxt, the hacker's power-browser.")))

(define (generate-home-nyxt-documentation)
  (generate-documentation
   `((home-nyxt-configuration
      ,home-nyxt-configuration-fields))
   'home-nyxt-configuration))
