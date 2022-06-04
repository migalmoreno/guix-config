(define-module (efimerspan home services tex)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services utils)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages tex)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:export (home-tex-service-type
            home-tex-configuration))

(define packages? (list-of package?))

(define-configuration/no-serialization home-tex-configuration
  (packages
   (packages '())
   "The list of @code{texlive} packages to use.")
  (envs
   (alist '())
   "Association list of environment variables to set for TeX. The key of the pair will
automatically be prepended with ``@code{TEX}'', meaning that @code{mfhome} will result
in ``@code{TEXMFHOME}''."))

(define (add-tex-environment-variables config)
  (define (serialize-field field-name val)
    (cons
     (string-append "TEX"
                    (object->snake-case-string field-name 'upper))
     (match val
       ((? list? v) (string-join v " "))
       (v (maybe-object->string v)))))

  (map (match-lambda
         ((car . cdr) (serialize-field car cdr)))
       (home-tex-configuration-envs config)))

(define (add-tex-packages config)
  (append (list texlive-base)
          (home-tex-configuration-packages config)))

(define home-tex-service-type
  (service-type
   (name 'home-tex)
   (extensions
    (list
     (service-extension
      home-environment-variables-service-type
      add-tex-environment-variables)
     (service-extension
      home-profile-service-type
      add-tex-packages)))
   (default-value (home-tex-configuration))
   (description "Sets up @code{TeX} packages and environment.")))
