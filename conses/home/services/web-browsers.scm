(define-module (conses home services web-browsers)
  #:use-module (conses serializers base)
  #:use-module (conses home services lisp)
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

(define-configuration home-nyxt-configuration
  (nyxt
   (package nyxt)
   "The nyxt package to use.")
  (lisp-packages
   (packages '())
   "List of Lisp packages to install alongside the configuration.")
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
   "List of @code{auto-mode} rules. See the
@uref{https://github.com/atlas-engineer/nyxt/blob/master/source/auto-rules.lisp#L393,
auto-rules serialization method} for information on how to write these rules."))

(define-configuration home-nyxt-extension
  (lisp-packages
   (packages '())
   "List of additional Lisp packages to install alongside the service extension.")
  (config-lisp
   (lisp-config '())
   "List of expressions to add to @file{config-lisp}. See
@code{home-nyxt-configuration-config-lisp} for more information."))

(define (home-nyxt-files-service config)
  (define (filter-fields field)
    (filter-configuration-fields home-nyxt-configuration-fields
                                 (list field)))

  (define (serialize-field field)
    (serialize-configuration
     config
     (filter-fields field)))

  (append
   (if (not (null? (home-nyxt-configuration-config-lisp config)))
       (list
        `(".config/nyxt/config.lisp"
          ,(mixed-text-file
            "config.lisp"
            (serialize-field 'config-lisp))))
       '())
   (if (not (null? (home-nyxt-configuration-auto-mode-rules-lisp config)))
       (list
        `(".local/share/nyxt/auto-mode-rules.lisp"
          ,(mixed-text-file
            "auto-mode-rules.lisp"
            (serialize-lisp-config 'auto-mode-rules-lisp
                                   `(,#~"("
                                     ,@(home-nyxt-configuration-auto-mode-rules-lisp config)
                                     ,#~")")))))
       '())))

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
