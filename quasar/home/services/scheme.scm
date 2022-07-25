(define-module (quasar home services scheme)
  #:use-module (quasar home)
  #:use-module (quasar home packages emacs-xyz)
  #:use-module (conses home services emacs)
  #:use-module (conses home services scheme)
  #:use-module (rrr packages emacs-xyz)
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
                ))
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
    `((require 'al-scheme nil t)
      (setq scheme-imenu-generic-expression al/scheme-imenu-generic-expression)
      (advice-add 'scheme-indent-function :override 'al/scheme-indent-function)
      (add-hook 'scheme-mode-hook 'al/scheme-fix-docstring-font-lock)
      (with-eval-after-load 'consult-imenu
        (add-to-list
         'consult-imenu-config
         '(scheme-mode
           :toplevel "Variable" :types
           ((?f "Functions" font-lock-function-name-face)
            (?q "Macros" font-lock-type-face)
            (?m "Methods" font-lock-function-name-face)
            (?M "Modules" font-lock-constant-face)
            (?C "Conditions" font-lock-keyword-face)
            (?c "Classes" font-lock-type-face)
            (?r "Records" font-lock-type-face)
            (?v "Variable" font-lock-variable-name-face)))))
      ,#~""
      (add-hook 'geiser-mode-hook 'eb-scheme-geiser-autoconnect)
      (add-to-list 'display-buffer-alist '("\\*Geiser.*\\*.*"
                                           (display-buffer-no-window)
                                           (allow-no-window . t)))
      ,#~""
      (with-eval-after-load 'geiser-mode
        (custom-set-variables
         '(geiser-mode-company-p nil)))
      ,#~""
      (with-eval-after-load 'geiser-impl
        (custom-set-variables
         '(geiser-default-implementation 'guile)))
      ,#~""
      (with-eval-after-load 'geiser-repl
        (define-key geiser-repl-mode-map (kbd "C-M-q") 'indent-sexp)
        (custom-set-variables
         '(geiser-repl-startup-time 5000)
         '(geiser-repl-history-filename (locate-user-emacs-file "geiser_history"))
         '(geiser-repl-query-on-kill-p nil)
         '(geiser-repl-use-other-window t)
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
    #:elisp-packages (list emacs-geiser
                           emacs-geiser-guile
                           emacs-rrr-al-scheme))))

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
      (define-key mode-specific-map "dI" 'eb-guix-daemons-root)
      (with-eval-after-load 'daemons
        (custom-set-variables
         '(daemons-init-system-submodules '(daemons-shepherd))
         '(daemons-always-sudo nil)))
      (with-eval-after-load 'eb-guix
        (custom-set-variables
         '(eb-guix-home-configuration-dir ,%channel-root))))
    #:elisp-packages (list
                      ;; emacs-guix
                      emacs-daemons))))
