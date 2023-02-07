(define-module (conses features shellutils)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu packages base)
  #:use-module (guix gexp)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:export (feature-compile))

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
      `((defun rde-compile-ansi-color-apply ()
          "Translate control sequences into text properties in the current buffer."
          (interactive)
          (ansi-color-apply-on-region (point-min) (point-max)))

        (with-eval-after-load 'compile
          (setq compilation-ask-about-save nil))
        (add-hook 'compilation-filter-hook 'rde-compile-ansi-color-apply)
        ,@(if (get-value 'emacs-project config)
              '((define-key project-prefix-map "c" 'project-compile)
                (add-to-list 'project-switch-commands '(project-compile "Compile Project")))
              '())))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
