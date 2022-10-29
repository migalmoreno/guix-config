(define-module (conses features emacs)
  #:use-module (conses utils)
  #:use-module (conses features emacs-xyz)
  #:use-module (conses features web-browsers)
  #:use-module (conses packages misc)
  #:use-module (conses packages emacs-xyz)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features predicates)
  #:use-module (rde serializers elisp)
  #:use-module (rde home services emacs)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs)
  #:use-module (guix gexp)
  #:use-module (ice-9 ftw)
  #:export (feature-emacs))

(define* (feature-emacs
          #:key
          (emacs emacs)
          (emacs-server-mode? #t)
          (extra-init-el '())
          (extra-early-init-el '())
          (additional-elisp-packages '()))
  "Configure the GNU Emacs extensible and self-documented editor."
  (ensure-pred any-package? emacs)
  (ensure-pred boolean? emacs-server-mode?)
  (ensure-pred elisp-config? extra-init-el)
  (ensure-pred elisp-config? extra-early-init-el)
  (ensure-pred list-of-file-likes? additional-elisp-packages)

  (define emacsclient (file-append emacs "/bin/emacsclient"))
  (define configure-rde-keymaps (@@ (rde features emacs) emacs-configure-rde-keymaps))

  (define (get-home-services config)
    "Return home services related to Emacs."
    (require-value 'full-name config)
    (require-value 'email config)

    (list
     (service home-emacs-feature-loader-service-type
              (home-emacs-feature-loader-configuration
               (add-to-init-el? #t)))
     (rde-elisp-configuration-service
      'rde-emacs
      config
      `((require 'xdg)
        (require 'configure-rde-keymaps)
        (defgroup configure nil
          "Base configuration group."
          :group 'external
          :prefix 'configure-)

        (add-hook 'after-init-hook 'server-start)
        (with-eval-after-load 'comp
          (setq native-comp-async-report-warnings-errors nil))
        (require 'warnings)
        (setq warning-suppress-types '((diary) (auto-save) (org-babel)))
        (setq warning-suppress-log-types '((comp org-babel)))
        (setq warning-minimum-level :error)
        (setq-default tab-width 2)
        (setq-default indent-tabs-mode nil)
        (with-eval-after-load 'indent
          (setq tab-always-indent 'complete))
        (global-so-long-mode)
        (define-key mode-specific-map "a" '("applications" . rde-app-map))
        (define-key mode-specific-map "t" '("toggles" . rde-toggle-map))
        (define-key global-map (kbd "C-=") 'text-scale-increase)
        (define-key global-map (kbd "C--") 'text-scale-decrease)
        (with-eval-after-load 'face-remap
          (setq text-scale-mode-step 1.075))
        (setq user-full-name ,(get-value 'full-name config))
        (setq user-mail-address ,(get-value 'email config)))
      #:early-init
      `((require 'xdg)
        (add-hook 'emacs-startup-hook (lambda ()
                                        (setq gc-cons-threshold 800000)))
        (setq gc-cons-threshold most-positive-fixnum)
        (setq package-native-compile t)
        (setq package-user-dir (expand-file-name "emacs/elpa" (xdg-data-home)))
        (setq package-enable-at-startup nil)
        (setq auto-save-list-file-prefix (expand-file-name "emacs/auto-save-list/.saves-"
                                                           (xdg-data-home))))
      #:elisp-packages (list configure-rde-keymaps))
     (service
      home-emacs-service-type
      (home-emacs-configuration
       (package emacs)
       (emacs-servers (if emacs-server-mode? '(server) '()))
       (elisp-packages additional-elisp-packages)
       (rebuild-elisp-packages? #f)))
     (simple-service
      'home-emacs-service
      home-emacs-service-type
      (home-emacs-extension
       (init-el extra-init-el)
       (early-init-el extra-early-init-el)))
     (simple-service
      'home-emacs-environment-variables-service
      home-environment-variables-service-type
      `(("VISUAL" . ,emacsclient)
        ("EDITOR" . ,emacsclient)))))

  (feature
   (name 'emacs)
   (values `((emacs . ,emacs)
             (emacs-server-mode? . ,emacs-server-mode?)
             (emacs-configure-rde-keymaps . ,configure-rde-keymaps)))
   (home-services-getter get-home-services)))
