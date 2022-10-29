(define-module (conses features documentation)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages scheme)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:export (feature-manpages))

(define* (feature-manpages
          #:key
          (pager #f))
  "Configure tooling to read manual pages."
 (ensure-pred maybe-file-like? pager)

  (define f-name 'manpages)

  (define (get-home-services config)
    "Return home services related to manpages."
    (list
     (simple-service
      'home-manpages-environment-variables-service
      home-environment-variables-service-type
      `(("PAGER" . ,pager)))
     (rde-elisp-configuration-service
      f-name
      config
      `((require 'configure-rde-keymaps)
        (add-hook 'woman-mode-hook 'toggle-truncate-lines)
        (let ((map rde-app-map))
          (define-key map "W" 'woman)
          (define-key map "M" 'man)
          ,@(if (get-value 'emacs-consult config)
                `((define-key map "dM" 'consult-man))
                '())))
      #:elisp-packages (list
                        (get-value 'emacs-configure-rde-keymaps config)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
