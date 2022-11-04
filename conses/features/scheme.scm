(define-module (conses features scheme)
  #:use-module (conses utils)
  #:use-module (conses packages emacs-xyz)
  #:use-module (conses home services lisp)
  #:use-module (conses home services scheme)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (rde home services emacs-xyz)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages scheme)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:export (feature-guile
            feature-guix))

(define* (feature-guile
          #:key
          (guile guile-3.0))
  "Configure and set up tooling for GNU Guile."

  (define f-name 'guile)

  (define (get-home-services config)
    "Return home services related to Guile."
    (list
     (service
      home-guile-service-type
      (home-guile-configuration
       (guile guile)
       (envs
        `((load-path . ,(string-append
                         "$XDG_CONFIG_HOME/guix/current/share/guile/site/3.0:"
                         "$HOME/.guix-profile/share/guile/site/3.0:"
                         "$HOME/.guix-home/profile/share/guile/3.0:"
                         "$GUILE_LOAD_PATH"))
          (load-compiled-path . ,(string-append
                                  "$XDG_CONFIG_HOME/guix/current/lib/guile/3.0/site-ccache:"
                                  "$GUILE_LOAD_COMPILED_PATH"))))))
     (simple-service
      'home-guile-templates-service
      home-emacs-tempel-service-type
      `(,#~"scheme-mode"
        (lambda "(lambda (" p ")" n> r> ")")
        (var "(define " p "\n" p ")")
        (fun "(define (" p ")" n> r> ")")
        (dmod "(define-module (" p ")" n> r> ")")
        (umod "#:use-module (" p ")")
        (ex "#:export (" p ")")))
     (simple-service
      'home-guile-profile-service
      home-profile-service-type
      (list sicp))
     (rde-elisp-configuration-service
      f-name
      config
      `((require 'geiser)
        (require 'geiser-guile)
        (require 'geiser-repl)
        (require 'configure-rde-keymaps)

        (defgroup configure-guile nil
          "Tooling and environment tweaks to work with GNU Guile."
          :group 'configure)

        (defun configure-scheme-geiser-autoconnect ()
          "Start a Geiser REPL unless an active connection is already present."
          (unless (geiser-repl--connection*)
            (save-window-excursion
              (run-guile))))

        (require 'al-scheme nil t)
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
        (add-hook 'geiser-mode-hook 'configure-scheme-geiser-autoconnect)
        (add-to-list
         'display-buffer-alist
         `(,(rx "*Geiser" (* any) "*")
           (display-buffer-reuse-window
            display-buffer-same-window)
           (reusable-frames . t)))
        (with-eval-after-load 'geiser-mode
          (require 'geiser-capf)
          (setq geiser-mode-company-p nil))
        (with-eval-after-load 'geiser-impl
          (setq geiser-default-implementation 'guile))
        (with-eval-after-load 'geiser-repl
          (define-key geiser-repl-mode-map (kbd "C-M-q") 'indent-sexp)
          (setq geiser-repl-startup-time 5000)
          (setq geiser-repl-history-filename (locate-user-emacs-file "geiser_history"))
          (setq geiser-repl-query-on-kill-p nil)
          (setq geiser-repl-use-other-window nil)
          (setq geiser-repl-per-project-p t))
        (define-key rde-app-map "G" 'geiser-guile)
        (with-eval-after-load 'geiser-guile
          (setq geiser-guile-load-path (and (getenv "GUILE_LOAD_PATH")
                                            (split-string (getenv "GUILE_LOAD_PATH") ":")))
          (setq geiser-guile-binary (executable-find "guile"))
          (setq geiser-guile-load-init-file-p t))
        (with-eval-after-load 'org
          (add-to-list 'org-structure-template-alist '("sc" . "src scheme")))
        (with-eval-after-load 'ob-core
          (require 'ob-scheme)
          (setq org-babel-default-header-args:scheme
                '((:results . "scalar")))))
      #:elisp-packages (list emacs-geiser
                             emacs-geiser-guile
                             emacs-al-scheme
                             (get-value 'emacs-configure-rde-keymaps config))
      #:summary "GNU Guile programming utilities"
      #:commentary "Programming utilities and tooling to work with GNU Guile.")))

  (feature
   (name f-name)
   (values `((,f-name . ,guile)))
   (home-services-getter get-home-services)))

(define* (feature-guix
          #:key
          (authorized-directories '())
          (extra-envs '()))
  "Configure the GNU Guix system and package manager."
  (ensure-pred list? authorized-directories)
  (ensure-pred list? extra-envs)

  (define (get-home-services config)
    "Return home services related to GNU Guix."
    (define emacs-f-name 'guix)

    (list
     (service home-guix-service-type
              (home-guix-configuration
               (shell-authorized-directories
                authorized-directories)
               (envs
                `((profile . ,(string-append (getenv "HOME") "/.guix-profile"))
                  ,@extra-envs))))
     (simple-service
      'home-guix-templates
      home-emacs-tempel-service-type
      `(,#~"scheme-mode"
        (pkg "(define-public " (p "" name) n>
             "(package" n>
             "(name \"" (s name) "\")" n>
             "(version \"" p "\")" n>
             "(source " n> p ")" n>
             "(build-system " p ")" n>
             "(home-page \"" p "\")" n>
             "(synopsis \"" p "\")" n>
             "(description \"" p "\")" n>
             "(license " p ")))")
        (gitref "(git-reference" n>
                "(url \"" p "\")" n>
                "(commit " (p "commit") "))")
        (orig "(origin " n>
              "(method " (p "git" method) "-fetch)" n>
              "(uri " p ")" n>
              "(file-name (" (s method) "-file-name name version))" n>
              "(sha256" n>
              "(base32 \"" p "\")))")))
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((defgroup configure-guix nil
          "Emacs integration with the GNU Guix package manager."
          :group 'configure)

        (defcustom configure-guix-home-configuration-dir nil
          "Directory that holds the GNU Guix home configuration."
          :type 'directory
          :group 'configure-guix)

        (defun configure-guix-daemons-root ()
          "Invoke the `daemons' command as superuser to get the list of system daemons."
          (interactive)
          (let ((default-directory (format "/sudo::%s" (make-temp-file nil t))))
            (daemons)))

        (defun configure-guix-compile-configuration ()
          "Compile the project located in `configure-guix-home-configuration-dir'."
          (interactive)
          (let ((default-directory configure-guix-home-configuration-dir)
                (compilation-read-command nil)
                (compile-command "make home")
                (display-buffer-alist `((,(rx "*compilation*")
                                         (display-buffer-no-window)))))
            (call-interactively 'compile)))

        (setq configure-guix-home-configuration-dir ,%project-root)
        ;; (let ((map mode-specific-map))
        ;;   (define-key map "gi" 'guix-installed-user-packages)
        ;;   (define-key map "gI" 'guix-installed-user-packages)
        ;;   (define-key map "gn" 'guix-packages-by-name)
        ;;   (define-key map "gF" 'guix-pull)
        ;;   (define-key map "gp" 'guix-profile)
        ;;   (define-key map "ga" 'guix-emacs-autoload-packages)
        ;;   (define-key map "gP" 'guix-find-package-definition)
        ;;   (define-key map "gS" 'guix-find-service-definition))
        (define-key rde-app-map "d" 'daemons)
        (define-key rde-app-map "D" 'configure-guix-daemons-root)
        (with-eval-after-load 'daemons
          (setq daemons-init-system-submodules '(daemons-shepherd))
          (setq daemons-always-sudo nil)))
      #:elisp-packages (list
                        ;; emacs-guix
                        emacs-daemons)
      #:summary "GNU Guix helpers for Emacs"
      #:commentary "Provide utilities to work with the GNU Guix package manager from Emacs.")))

  (feature
   (name 'guix)
   (values `((shell-authorized-directories . ,authorized-directories)
             (extra-envs . ,extra-envs)))
   (home-services-getter get-home-services)))
