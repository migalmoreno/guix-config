(define-module (migalmoreno presets development)
  #:use-module (rde features documentation)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features shells)
  #:use-module (rde features shellutils)
  #:use-module (rde features terminals))

(define-public shell-features
  (list
   (feature-direnv)
   (feature-emacs-comint)
   (feature-emacs-shell)
   (feature-emacs-eshell)
   (feature-compile)
   (feature-bash)
   (feature-vterm)
   (feature-manpages)))
