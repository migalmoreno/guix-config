(define-module (conses features shellutils)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu packages base)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages shellutils)
  #:use-module (guix gexp)
  #:export (feature-direnv
            feature-compile))

(define* (feature-direnv
          #:key
          (direnv direnv)
          (emacs-envrc emacs-envrc))
  "Configure Direnv, an extension for your shell to better
manage per-project environments."
  (ensure-pred file-like? direnv)
  (ensure-pred file-like?  emacs-envrc)

  (define f-name 'direnv)

  (define (get-home-services config)
    "Return home services related to direnv."
    (list
     (simple-service
      'home-direnv-profile-service
      home-profile-service-type
      (list direnv))
     (simple-service
      'direnv-config
      home-xdg-configuration-files-service-type
      `(("direnv/direnvrc" ,(plain-file
                             "direnvrc"
                             "\
use_guix_shell() {
  LOCK_FILE=channels-lock.scm
  if [ -f $LOCK_FILE ]; then
    eval \"$(guix time-machine -C $LOCK_FILE -- shell \"$@\" --search-paths)\"
  else
    eval \"$(guix shell \"$@\" --search-paths)\"
  fi
}"))))
     (rde-elisp-configuration-service
      f-name
      config
      `((with-eval-after-load 'envrc-autoloads
          (envrc-global-mode))
        (with-eval-after-load 'envrc
          (define-key envrc-mode-map (kbd "C-c E") 'envrc-command-map)))
      #:elisp-packages (list emacs-envrc))))

  (feature
   (name f-name)
   (values `((,f-name . ,direnv)))
   (home-services-getter get-home-services)))

(define* (feature-compile
          #:key
          (make gnu-make))
  "Configure compilation tooling."
  (ensure-pred file-like? make)

  (define f-name 'compile)

  (define (get-home-services config)
    "Return home services related to compilation."
    (list
     (simple-service
      'home-compilation-profile-service
      home-profile-service-type
      (list make))
     (rde-elisp-configuration-service
      f-name
      config
      `((defun configure-compile-ansi-color-apply ()
          "Translate control sequences into text properties in the current buffer."
          (interactive)
          (ansi-color-apply-on-region (point-min) (point-max)))

        (with-eval-after-load 'compile
          (setq compilation-ask-about-save nil))
        (add-hook 'compilation-filter-hook 'configure-compile-ansi-color-apply)
        ,@(if (get-value 'emacs-project config)
              '((define-key project-prefix-map "c" 'project-compile)
                (add-to-list 'project-switch-commands '(project-compile "Compile Project")))
              '())))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
