(define-module (quasar home services tex)
  #:use-module (efimerspan home services emacs)
  #:use-module (efimerspan home services tex)
  #:use-module (gnu services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages tex)
  #:use-module (gnu home-services base)
  #:export (tex-service))

(define (tex-service)
  (list
   (service home-tex-service-type
            (home-tex-configuration
             (packages
              (list texlive-wrapfig
                    texlive-capt-of
                    texlive-hyperref
                    texlive-fonts-ec
                    texlive-latex-geometry
                    texlive-latex-listings
                    texlive-xcolor
                    texlive-ulem
                    texlive-latex-preview
                    texlive-amsfonts))
             (envs
              '((mfhome . "$XDG_DATA_HOME/texmf")
                (mfvar . "$XDG_CACHE_HOME/texlive/texmf-var")
                (mfconfig . "$XDG_CONFIG_HOME/texlive/texmf-config")))))
   (elisp-configuration-service
    `((with-eval-after-load 'ox-latex
        (custom-set-variables
         '(org-latex-listings t)
         '(org-latex-listings-options
           '(("basicstyle" "\\ttfamily")
             ("stringstyle" "\\color{blue}\\ttfamily")
             ("numbers" "left")
             ("numberstyle" "\\tiny")
             ("breaklines" "true")
             ("showstringspaces" "false")
             ("showtabs" "false")
             ("keywordstyle" "\\color{violet}")
             ("commentstyle" "\\color{gray}")
             ("label" "{Figure}"))))))
    #:elisp-packages (list emacs-org-fragtog))))
