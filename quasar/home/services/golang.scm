(define-module (quasar home services golang)
  #:use-module (conses home services emacs)
  #:use-module (conses packages emacs-xyz)
  #:use-module (gnu home-services base)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu home services shells)
  #:export (go-service))

(define (go-service)
  (list
   (home-generic-service 'home-go-packages #:packages (list go))
   (elisp-configuration-service
    '((add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
      (with-eval-after-load 'org
        (require 'ob-go)
        (add-to-list 'org-structure-template-alist '("go" . "src go"))
        (add-to-list 'org-babel-load-languages '(go . t)))
      (with-eval-after-load 'ob-core
        (setq org-babel-default-header-args:go
              '((:imports . "fmt")))))
    #:elisp-packages (list emacs-go-mode emacs-ob-go))
   (simple-service 'home-go-envs
                   home-environment-variables-service-type
                   '(("GOPATH" . "$XDG_DATA_HOME/go")))))
