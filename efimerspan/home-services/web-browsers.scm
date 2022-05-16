(define-module (efimerspan home-services web-browsers)
  #:use-module (efimerspan serializers lisp)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix packages)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:use-module (gnu services shepherd)
  #:use-module (gnu home services shepherd)
  #:use-module (ice-9 pretty-print)
  #:export (home-nyxt-configuration
            home-nyxt-service-type
            nyxt-configuration-service))

(define* (nyxt-configuration-service #:optional (lisp-expressions '())
                                     #:key (lisp-packages '()))
  (simple-service
   (gensym "nyxt-configuration-service")
   home-nyxt-service-type
   (home-nyxt-extension
    (init-lisp `(,@lisp-expressions))
    (lisp-packages lisp-packages))))

(define-configuration home-nyxt-configuration
  (package
    (package nyxt)
    "The nyxt package to use.")
  (lisp-packages
   (packages '())
   "List of Lisp packages to install.")
  (init-lisp
   (lisp-config '())
   "List of expressions, where each expression can be Sexp or a Gexp. Sexp is a Lisp form.
Strings don't require any conversion, but booleans do.
Sexps can contain file-like objects, a string with path to a corresponding file in the store.
Gexps should be strings and their value will be appended to the resulting Lisp file.

The list of expressions will be interposed with \n and everything will end up in the
@file{init.lisp}.")
  (auto-mode-rules-lisp
   (lisp-config '())))

(define-configuration home-nyxt-extension
  (lisp-packages
   (packages '())
   "List of additional Lisp packages.")
  (init-lisp
   (lisp-config '())
   "List of expressions to add to @file{init-lisp}. See
home-nyxt-service-type for more information."))

(define (get-nyxt-configuration-files config files)
  (define (filter-fields field)
    (filter-configuration-fields home-nyxt-configuration-fields
                                 (list field)))

  (define (serialize-field field)
    (serialize-configuration
     config
     (filter-fields field)))

  (define (file-if-not-empty field)
    (let ((filename (string-append
                     "nyxt/"
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
   (map (lamda (file)
               (file-if-not-empty file))
        files)))

(define (add-nyxt-config-configuration config)
  (get-nyxt-configuration-files config (list 'init-lisp)))

(define (add-nyxt-data-configuration config)
  (get-nyxt-configuration-files config (list 'auto-mode-rules-lisp)))

(define (home-nyxt-extensions original-config extension-configs)
  (home-nyxt-configuration
   (inherit original-config)
   (lisp-packages
    (append (home-nyxt-configuration-lisp-packages original-config)
            (append-map
             home-nyxt-extension-lisp-packages extension-configs)))
   (init-lisp
    (append (home-nyxt-configuration-init-lisp original-config)
            (append-map
             home-nyxt-extension-lisp-packages extension-configs)))))

(define (nyxt-profile-service config)
  (list (home-nyxt-configuration-package config)))

(define home-nyxt-service-type
  (service-type
   (name 'home-nyxt)
   (extensions
    (list
     (service-extension
      home-profile-service-type
      nyxt-profile-service)
     (service-extension
      home-xdg-configuration-files-service-type
      add-nyxt-configuration)
     (service-extension
      home-xdg-data-files-service-type
      add-nyxt-data-configuration)))
   (compose identity)
   (extend home-nyxt-extensions)
   (default-value (home-nyxt-configuration))
   (description "Installs and configures Nyxt, the hacker's power-browser.")))

(define (generate-home-nyxt-documentation)
  (generate-documentation
   `((home-nyxt-configuration
      ,home-nyxt-configuration-fields))
   'home-nyxt-configuration))
