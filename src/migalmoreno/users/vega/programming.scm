(define-module (migalmoreno users vega programming)
  #:use-module (migalmoreno utils)
  #:use-module (contrib features javascript)
  #:use-module (guix gexp)
  #:use-module (rde features android)
  #:use-module (rde features clojure)
  #:use-module (rde features docker)
  #:use-module (rde features documentation)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features guile)
  #:use-module (rde features golang)
  #:use-module (rde features lisp)
  #:use-module (rde features ocaml)
  #:use-module (rde features password-utils)
  #:use-module (rde features shells)
  #:use-module (rde features shellutils)
  #:use-module (rde features ssh)
  #:use-module (rde features terminals)
  #:use-module (rde features version-control)
  #:use-module (rde features virtualization)
  #:use-module (rde packages))

(define %extra-ssh-config
  (home-ssh-configuration
   (extra-config
    (list
     (ssh-host
      (host "cygnus")
      (options
       `((host-name . ,(getenv "CYGNUS_HOST"))
         (user . "root"))))
     (ssh-host
      (host "deneb")
      (options
       `((host-name . ,(getenv "CYGNUS_HOST"))
         (user . "deneb"))))
     (ssh-host
      (host "auriga")
      (options
       `((host-name . ,(getenv "AURIGA_HOST"))
         (user . "root"))))
     (ssh-host
      (host "capella")
      (options
       `((host-name . ,(getenv "AURIGA_HOST"))
         (user . "capella"))))))))

(define-public features
  (list
   (feature-ssh
    #:ssh-configuration %extra-ssh-config)
   (feature-qemu)
   (feature-docker)
   (feature-android)
   (feature-emacs-flymake)
   (feature-emacs-eglot)
   (feature-emacs-xref)
   (feature-emacs-smartparens
    #:paredit-bindings? #t
    #:smartparens-hooks
    '(prog-mode-hook
      lisp-data-mode-hook
      minibuffer-inactive-mode-hook
      comint-mode-hook
      cider-repl-mode-hook)
    #:smartparens-strict-hooks
    '(prog-mode-hook
      lisp-data-mode-hook
      comint-mode-hook
      cider-repl-mode-hook))
   (feature-emacs-elisp)
   (feature-guile)
   (feature-clojure)
   (feature-javascript
    #:node (@ (gnu packages node) node-lts))
   (feature-lisp
    #:extra-lisp-packages
    (strings->packages "sbcl-prove" "sbcl-cl-cffi-gtk" "sbcl-lisp-unit2")
    #:extra-source-registry-files
    (list
     (plain-file
      "10-projects.conf"
      (format #f "(:tree \"~a/src\")" (getenv "HOME")))))
   (feature-ocaml
    #:extra-init-ml
    (list
     (format #f "#directory \"~a/.guix-home/profile/lib/ocaml/site-lib/\""
             (getenv "HOME"))
     "#directory \"_build\""
     "#use \"topfind\""
     "#thread"
     "#require \"core.top\""
     "#require \"ppx_fields_conv\""
     "#require \"str\""
     "open Core"))
   (feature-go)
   (feature-emacs-git)
   (feature-git
    #:extra-config
    `((sendemail
       ((cc . ,%default-email)
        (thread . #t)))
      (github
       ((user . ,%default-username)))
      (gitlab
       ((user . ,%default-username)))))
   (feature-password-store
    #:remote-password-store-url
    (format #f "git@git.~a:password-store" %default-domain))
   (feature-direnv)
   (feature-emacs-comint)
   (feature-emacs-shell)
   (feature-emacs-eshell)
   (feature-compile)
   (feature-bash)
   (feature-vterm)
   (feature-manpages)))
