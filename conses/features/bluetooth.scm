(define-module (conses features bluetooth)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services desktop)
  #:use-module (gnu packages emacs-xyz)
  #:export (feature-bluetooth))

(define* (feature-bluetooth
          #:key
          (emacs-bluetooth emacs-bluetooth)
          (auto-enable? #t)
          (bluetooth-key "b"))
  "Configure and set up Bluetooth."
  (ensure-pred boolean? auto-enable?)
  (ensure-pred string? bluetooth-key)

  (define f-name 'bluetooth)

  (define (get-home-services config)
    "Return home services related to Bluetooth."
    (list
     (rde-elisp-configuration-service
      f-name
      config
      `((require 'configure-rde-keymaps)
        (define-key rde-app-map (kbd ,bluetooth-key) 'bluetooth-list-devices)
        (with-eval-after-load 'bluetooth
          (add-hook 'kill-emacs-hook 'bluetooth-toggle-powered)
          (define-key bluetooth-mode-map "C" 'bluetooth-connect-profile)))
      #:elisp-packages (list emacs-bluetooth
                             (get-value 'emacs-configure-rde-keymaps config)))))

  (define (get-system-services config)
    "Return system services related to Bluetooth."
    (list
     (bluetooth-service #:auto-enable? auto-enable?)))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))
