(define-module (conses home services lisp)
  #:use-module (rde serializers lisp)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu packages lisp)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix ui)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (home-lisp-configuration
            home-lisp-service-type))

(define lisp-config? sexp-config?)
(define lisp-serialize sexp-serialize)
(define serialize-lisp-config serialize-sexp-config)

(define file-likes? (list-of file-like?))
(define serialize-file-likes empty-serializer)

(define-configuration home-lisp-configuration
  (lisp
    (package sbcl)
    "The Lisp implementation to use.")
  (slynk-lisp
   (lisp-config '())
   "List of expressions, where each expression can be a Sexp or a Gexp. Sexp is
a Lisp form.  Strings don't require any conversion, but booleans do.
Sexps can contain file-like objects, paths to corresponding files in the
store that will be serialized as strings.  Gexps should be string-valued and
their value will be appended to the resulting Lisp file.

The list of expressions will be interposed with \n and everything will end up
in the @file{slynk.lisp}.")
  (sbclrc-lisp
   (lisp-config '())
   "As per @code{slynk-lisp}, but everything will be placed in
@file{sbclrc.lisp}.")
  (extra-source-registry-files
   (file-likes '())
   "List of file-likes to add under
@file{.config/common-lisp/source-registry.conf.d} to allow the ASDF
source-registry mechanism to find new Lisp systems in custom filesystem
locations."))

(define (home-lisp-files-service config)
  (define (extra-source-registry-files->file-union files)
    "Return a G-exp obtained by processing EXTRA-SOURCE-REGISTRY-FILES
with FILE-UNION."

    (define (file-like->name file)
      (match file
        ((? local-file?)
         (local-file-name file))
        ((? plain-file?)
         (plain-file-name file))
        ((? computed-file?)
         (computed-file-name file))
        (_ (leave (G_ "~a is not a local-file, plain-file or \
computed-file object~%") file))))

    (define (assert-conf-file-name name)
      (unless (string-suffix? ".conf" name)
        (display "Not a conf file")
        (leave (G_ "`~a' lacks the required `.conf' file name extension~%")
               name))
      name)

    (let ((labels (map (compose assert-conf-file-name
                                file-like->name)
                       files)))
      (file-union "source-registry.conf.d" (zip labels files))))

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
      (if (null? ((configuration-field-getter field-obj) config))
          '()
          `(,(string-append "." filename)
            ,(mixed-text-file
              filename
              (serialize-field field))))))

  (filter
   (compose not null?)
   (list
    (file-if-not-empty 'sbclrc-lisp)
    (file-if-not-empty 'slynk-lisp)
    (if (null? (home-lisp-configuration-extra-source-registry-files config))
        '()
        `(".config/common-lisp/source-registry.conf.d"
          ,(extra-source-registry-files->file-union
            (home-lisp-configuration-extra-source-registry-files config)))))))

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
   (description "Configure Common Lisp related tooling.")))

(define (generate-home-lisp-documentation)
  (generate-documentation
   `((home-lisp-configuration
      ,home-lisp-configuration-fields))
   'home-lisp-configuration))
