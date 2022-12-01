(define-module (conses features ocaml)
  #:use-module (conses utils)
  #:use-module (conses home services ocaml)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:export (feature-ocaml))

(define* (feature-ocaml
          #:key
          (ocaml ocaml)
          (emacs-tuareg emacs-tuareg)
          (extra-packages '())
          (opam? #f))
  "Configure tooling and environment for OCaml."
  (ensure-pred any-package? ocaml)
  (ensure-pred file-like? emacs-tuareg)
  (ensure-pred list-of-file-likes? extra-packages)
  (ensure-pred boolean? opam?)

  (define f-name 'ocaml)

  (define (get-home-services config)
    "Return home services related to OCaml."
    (append
     (cond
      ((and opam? (get-value 'zsh config))
       (list
        (simple-service
         'home-ocaml-bash-service
         home-zsh-service-type
         (home-zsh-extension
          (zshrc
           (list "eval \"$(opam env)\""))))))
      ((and opam? (get-value 'bash config))
       (list
        (simple-service
         'home-ocaml-bash-service
         home-bash-service-type
         (home-bash-extension
          (bashrc
           (list "eval \"$(opam env)\""))))))
      (else '()))
     (if opam?
         (list
          (simple-service
           'home-ocaml-environment-variables-service
           home-environment-variables-service-type
           '(("OPAMROOT" . "$XDG_CACHE_HOME/opam"))))
         '())
     (list
      (simple-service
       'home-ocaml-profile-service
       home-profile-service-type
       (if opam?
           (list opam)
           (append
            (list
             ocaml-core
             ocaml-findlib
             dune
             ocaml-ocp-indent
             ocaml-merlin
             ocamlbuild)
            extra-packages)))
      (service home-ocaml-service-type
               (home-ocaml-configuration
                (config
                 `((format #f "#directory \"~a/.guix-home/profile/lib/ocaml/site-lib/\""
                           ,(get-value 'home-directory config))
                   "#directory \"_build\""
                   "#use \"topfind\""
                   "#thread"
                   "#require \"core.top\""
                   "#require \"ppx_fields_conv\""
                   "#require \"str\""
                   "open Core"))))
      (rde-elisp-configuration-service
       f-name
       config
       `((require 'configure-rde-keymaps)
         (defgroup configure-ocaml nil
           "General OCaml programming utilities."
           :group 'configure)

         ,@(if opam?
               '((defun configure-ocaml-load-merlin ()
                   "Set up `merlin-mode' for OCaml."
                   (let ((opam-share (car (process-lines "opam" "var" "share"))))
                     (when (and opam-share (file-directory-p opam-share))
                       (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share)))))

                 (defun configure-ocaml-set-environment ()
                   "When using Opam, set its corresponding environment variables."
                   (dolist (var (car (read-from-string
                                      (shell-command-to-string "opam config env --sexp"))))
                     (setenv (car var) (cadr var)))))
               '())

         (define-minor-mode configure-ocaml-mode
           "Set up convenient tweaks for an OCaml programming environment."
           :global t :group 'configure-ocaml
           (if configure-ocaml-mode
               (progn
                (setq-local comment-style 'multiline
                            comment-continue "   ")
                (when (fboundp 'prettify-symbols-mode)
                  (prettify-symbols-mode 1))
                ,@(if opam?
                      '((configure-ocaml-load-merlin)
                        (configure-ocaml-set-environment))
                      '())
                (autoload 'merlin-mode "merlin" nil t nil)
                (merlin-mode))))

         (add-to-list 'auto-mode-alist '("\\.ml[ily]?\\'" . tuareg-mode))
         (add-hook 'tuareg-mode-hook 'configure-ocaml-mode)
         (define-key rde-app-map "o" 'run-ocaml)
         (setq tuareg-interactive-program
               ,@(if opam?
                     '((format "%s -nopromptcont"
                               (expand-file-name
                                "ocaml"
                                (ignore-errors
                                 (car (process-lines "opam" "var" "bin"))))))
                     `(,(file-append ocaml "/bin/ocaml" " -nopromptcont"))))
         (with-eval-after-load 'tuareg-mode
           (setq tuareg-prettify-symbols-full nil))
         (with-eval-after-load 'merlin
           (setq merlin-command
                 ,@(if opam?
                       'opam
                       `(,(file-append ocaml-merlin "/bin/ocamlmerlin"))))
           (setq merlin-report-warnings nil)
           (setq merlin-error-in-fringe nil)
           (setq merlin-error-check-then-move nil))
         (with-eval-after-load 'org
           (add-to-list 'org-structure-template-alist '("ml" . "src ocaml"))
           (require 'ob-ocaml))
         (with-eval-after-load 'ob-core
           (setq org-babel-default-header-args:ocaml
                 '((:results . "scalar"))))
         (with-eval-after-load 'ob-ocaml
           (setq org-babel-ocaml-command
                 ,@(if opam?
                       '((expand-file-name
                          "ocaml"
                          (ignore-errors
                           (car (process-lines "opam" "var" "bin")))))
                       `(,(file-append ocaml "/bin/ocaml"))))))
       #:elisp-packages (list emacs-tuareg
                              (get-value 'emacs-configure-rde-keymaps config))
       #:summary "OCaml programming utilities"
       #:commentary "General OCaml programming utilities and tooling setup."))))

  (feature
   (name f-name)
   (values `((,f-name . ,ocaml)
             (emacs-tuareg . ,emacs-tuareg)
             (opam? . ,opam?)))
   (home-services-getter get-home-services)))
