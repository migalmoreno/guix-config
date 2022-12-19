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
          (pager #f)
          (man-key "M")
          (woman-key "W"))
  "Configure tooling to read manual pages."
  (ensure-pred maybe-file-like? pager)
  (ensure-pred string? man-key)
  (ensure-pred string? woman-key)

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
      `((add-hook 'woman-mode-hook 'toggle-truncate-lines)
        (with-eval-after-load 'rde-keymaps
          (let ((map rde-app-map))
            (define-key map (kbd ,woman-key) 'woman)
            (define-key map (kbd ,man-key) ',(if (get-value 'emacs-consult config)
                                                 'consult-man
                                                 'man))))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
