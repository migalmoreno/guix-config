(define-module (quasar home services scheme)
  #:use-module (quasar home)
  #:use-module (efimerspan home services emacs)
  #:use-module (efimerspan home services scheme)
  #:use-module (gnu home-services base)
  #:use-module (gnu home services)
  #:use-module (gnu packages scheme)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:export (guile-service
            guix-service))

(define (guile-service)
  (list
   (service home-guile-service-type
            (home-guile-configuration
             (config
              `(;; (use-modules (guix))
                ,#~""
                (cond ((false-if-exception (resolve-interface '(ice-9 readline)))
                       =>
                       (lambda (module)
                         ;; Enable completion and input history at the REPL.
                         ((module-ref module 'activate-readline))))
                      (else
                       (display "Consider installing the 'guile-readline' package for
convenient interactive line editing and input history.\n\n")))
                ,#~""
                (unless (getenv "INSIDE_EMACS")
                  (cond ((false-if-exception (resolve-interface '(ice-9 colorized)))
                         =>
                         (lambda (module)
                           ;; Enable completion and input history at the REPL.
                           ((module-ref module 'activate-colorized))))
                        (else
                         (display "Consider installing the 'guile-colorized' package
for a colorful Guile experience.\n\n"))))))
             (envs
              `((load-path . ,(string-append
                               "$XDG_CONFIG_HOME/guix/current/share/guile/site/3.0:"
                               "$HOME/.guix-profile/share/guile/site/3.0:"
                               "$HOME/.guix-home/profile/share/guile/3.0:"
                               "$GUILE_LOAD_PATH"))
                (load-compiled-path . ,(string-append
                                        "$XDG_CONFIG_HOME/guix/current/lib/guile/3.0/site-ccache:"
                                        "$GUILE_LOAD_COMPILED_PATH"))))))
   (elisp-configuration-service
    `((add-hook 'geiser-mode-hook 'eb-lisp-geiser-autoconnect)
      (with-eval-after-load 'geiser-impl
        (custom-set-variables
         '(geiser-default-implementation 'guile)))
      ,#~""
      (with-eval-after-load 'geiser-repl
        (define-key geiser-repl-mode-map (kbd "C-M-q") 'indent-sexp)
        (custom-set-variables
         '(geiser-repl-startup-time 20000)
         '(geiser-repl-history-filename (locate-user-emacs-file "geiser_history"))
         '(geiser-repl-query-on-kill-p nil)
         '(geiser-repl-use-other-window nil)
         '(geiser-repl-per-project-p t)))
      ,#~""
      (define-key mode-specific-map "rg" 'run-guile)
      (with-eval-after-load 'geiser-guile
        (custom-set-variables
         '(geiser-guile-load-path (split-string (getenv "GUILE_LOAD_PATH") ":"))
         '(geiser-guile-binary (executable-find "guile"))
         '(geiser-guile-load-init-file-p t)))
      ,#~""
      (add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
      (add-to-list 'org-babel-load-languages '(scheme . t))
      (with-eval-after-load 'ob-core
        (setq org-babel-default-header-args:scheme
              '((:results . "scalar")))))
    #:elisp-packages (list emacs-geiser emacs-geiser-guile))))

(define (guix-service)
  (list
   (service home-guix-service-type
            (home-guix-configuration
             (shell-authorized-directories
              '("~/src/fdroid.el"
                "~/src/guixrc"))
             (envs
              `((profile . ,(string-append (getenv "HOME") "/.guix-profile"))))))
   (elisp-configuration-service
    `(;; (require 'guix-autoloads)
      ;; (guix-emacs-autoload-packages)
      ;; (let ((map mode-specific-map))
      ;;   (define-key map "Gi" 'guix-installed-user-packages)
      ;;   (define-key map "GI" 'guix-installed-user-packages)
      ;;   (define-key map "Gn" 'guix-packages-by-name)
      ;;   (define-key map "GF" 'guix-pull)
      ;;   (define-key map "Gp" 'guix-profile)
      ;;   (define-key map "Ga" 'guix-emacs-autoload-packages)
      ;;   (define-key map "GP" 'guix-find-package-definition)
      ;;   (define-key map "GS" 'guix-find-service-definition))
      ,#~""
      (define-key mode-specific-map "di" 'daemons)
      (with-eval-after-load 'daemons
                            (custom-set-variables
                             '(daemons-init-system-submodules '(daemons-shepherd))
                             '(daemons-always-sudo nil))))
    #:elisp-packages (list
                      ;; emacs-guix
                      emacs-daemons))))
