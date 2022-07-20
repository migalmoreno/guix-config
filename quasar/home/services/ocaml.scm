(define-module (quasar home services ocaml)
  #:use-module (quasar home)
  #:use-module (conses home services emacs)
  #:use-module (conses home services ocaml)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages base)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages assembly)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home-services base)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:export (ocaml-service))

(define* (ocaml-service #:key (opam-p #f))
  (cons*
   (service home-ocaml-service-type
            (home-ocaml-configuration
             (tools
              (append
                (if opam-p
                    (list opam)
                    (list ocaml-core
                          ocaml-findlib
                          dune
                          ocaml-ocp-indent
                          ocaml-merlin
                          ocamlbuild))
                (list
                 autoconf
                 gcc-toolchain
                 nasm
                 binutils)))
             (config
              `((format #f "#directory \"~a/.guix-home/profile/lib/ocaml/site-lib/\""
                        ,(getenv "HOME"))
                "#directory \"_build\""
                "#use \"topfind\""
                "#thread"
                "#require \"core.top\""
                "#require \"ppx_fields_conv\""
                "#require \"str\""
                "open Core"))))
   (elisp-configuration-service
    `((add-to-list 'auto-mode-alist '("\\.ml[ily]?\\'" . tuareg-mode))
      (add-hook 'tuareg-mode-hook 'eb-prog-ocaml-mode)
      (define-key mode-specific-map "ro" 'run-ocaml)
      (setq tuareg-interactive-program
            (if (executable-find "opam")
                (format "%s -nopromptcont"
                 (expand-file-name
                  "ocaml"
                  (ignore-errors
                   (car (process-lines "opam" "var" "bin")))))
                (concat ,(file-append ocaml "/bin/ocaml") " -nopromptcont")))
      (with-eval-after-load 'tuareg-mode
        (custom-set-variables
         '(tuareg-prettify-symbols-full nil)))
      ,#~""
      (with-eval-after-load 'merlin
        (setq merlin-command (if (executable-find "opam")
                                 'opam
                                 ,(file-append ocaml-merlin "/bin/ocamlmerlin")))
        (custom-set-variables
         '(merlin-report-warnings nil)
         '(merlin-error-in-fringe nil)
         '(merlin-error-check-then-move nil)))
      ,#~""
      (with-eval-after-load 'org
        (add-to-list 'org-structure-template-alist '("ml" . "src ocaml"))
        (add-to-list 'org-babel-load-languages '(ocaml . t)))
      (with-eval-after-load 'ob-core
        (setq org-babel-default-header-args:ocaml
              '((:results . "scalar"))))
      ,#~""
      (with-eval-after-load 'ob-ocaml
        (custom-set-variables
         '(org-babel-ocaml-command
           (if (executable-find "opam")
               (expand-file-name
                "ocaml"
                (ignore-errors
                 (car (process-lines "opam" "var" "bin"))))
               ,(file-append ocaml "/bin/ocaml"))))))
    #:elisp-packages (list emacs-tuareg))
   (if opam-p
       (list
        (simple-service 'home-ocaml-envs
                        home-bash-service-type
                        (home-bash-extensions
                         (bashrc
                          (list "eval \"$(opam env)\"")))))
       '())))
