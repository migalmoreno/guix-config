(define-module (conses features tex)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (rde serializers elisp)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages emacs-xyz)
  #:export (feature-tex))

(define* (feature-tex
          #:key
          extra-tex-packages
          (biblatex? #t)
          (listings? #t)
          (listings-options '()))
  "Configure the TeX typesetting system."
  (ensure-pred list-of-file-likes? extra-tex-packages)
  (ensure-pred boolean? biblatex?)
  (ensure-pred boolean? listings?)
  (ensure-pred elisp-config? listings-options)

  (define f-name 'tex)

  (define (get-home-services config)
    "Return home services related to TeX."
    (list
     (simple-service
      'add-tex-home-packages
      home-profile-service-type
      (append
        `(,texlive-base
          ,@extra-tex-packages)
        (if biblatex?
            (list texlive-biblatex biber)
            '())))
     (simple-service
      'add-tex-home-envs
      home-environment-variables-service-type
      '(("TEX_MFHOME" . "$XDG_DATA_HOME/texmf")
        ("TEX_MFVAR" . "$XDG_CACHE_HOME/texlive/texmf-var")
        ("TEX_MFCONFIG" . "$XDG_CONFIG_HOME/texlive/texmf-config")))
     (rde-elisp-configuration-service
      f-name
      config
      `((with-eval-after-load 'ox-latex
          ,@(if listings?
                `((setq org-latex-listings t)
                  (setq org-latex-listings-options ',listings-options))))
        (setq bibtex-user-optional-fields
              '(("keywords" "Keywords to describe the entry" "")
                ("file" "Link to document file" ":"))
              bibtex-align-at-equal-sign t)
        ,@(if biblatex?
              '((setq bibtex-dialect 'biblatex))
              '())))))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             (tex-biblatex . ,biblatex?)))
   (home-services-getter get-home-services)))
