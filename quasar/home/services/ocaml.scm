(define-module (quasar home services ocaml)
  #:use-module (quasar home)
  #:use-module (conses home services emacs)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages base)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu home-services base)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:export (ocaml-service))

(define (ocaml-service)
  (list
   (home-generic-service
    'ocaml-service
    #:files
    `((".ocamlinit"
       ,(mixed-text-file
         "ocamlinit"
         #~(string-append
            #$@(interpose (list "#use \"topfind\""
                                "#thread"
                                "#require \"core.top\""
                                "#require \"ppx_fields_conv\""
                                "open Core")
                          "\n"
                          'suffix)))))
    #:packages
    (list
     autoconf
     gcc-toolchain
     binutils
     ocaml
     ocaml-core
     dune
     ocaml-ocp-indent
     ocaml-findlib
     ocaml-merlin
     ocamlbuild
     opam))
   (elisp-configuration-service
    `((add-to-list 'auto-mode-alist '("\\.ml[ily]?\\'" . tuareg-mode))
      (add-hook 'tuareg-mode-hook 'eb-prog-ocaml-mode)
      (define-key mode-specific-map "ro" 'run-ocaml)
      (custom-set-variables
       '(tuareg-interactive-program
         (format "%s -nopromptcont"
                 (expand-file-name
                  "ocaml"
                  (ignore-errors
                   (car (process-lines "opam" "var" "bin")))))))
      (with-eval-after-load 'tuareg-mode
        (custom-set-variables
         '(tuareg-prettify-symbols-full nil)))
      ,#~""
      (with-eval-after-load 'merlin
        (custom-set-variables
         '(merlin-command 'opam)
         '(merlin-report-warnings nil)
         '(merlin-error-in-fringe nil)
         '(merlin-error-check-then-move nil)))
      ,#~""
      (add-to-list 'org-structure-template-alist '("ml" . "src ocaml"))
      (add-to-list 'org-babel-load-languages '(ocaml . t))
      (with-eval-after-load 'ob-core
        (setq org-babel-default-header-args:ocaml
              '((:results . "scalar"))))
      ,#~""
      (with-eval-after-load 'ob-ocaml
        (custom-set-variables
         '(org-babel-ocaml-command
           (expand-file-name
            "ocaml"
            (ignore-errors
             (car (process-lines "opam" "var" "bin"))))))))
    #:elisp-packages (list emacs-tuareg))))
