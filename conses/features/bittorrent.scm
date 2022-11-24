(define-module (conses features bittorrent)
  #:use-module (conses utils)
  #:use-module (conses home services bittorrent)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages bittorrent)
  #:use-module (guix gexp)
  #:export (feature-transmission))

(define* (feature-transmission
          #:key
          (transmission transmission)
          (emacs-transmission emacs-transmission))
  "Configure the Transmission daemon in the user space."
  (ensure-pred any-package? transmission)
  (ensure-pred file-like? emacs-transmission)

  (define f-name 'transmission)

  (define (get-home-services config)
    "Return home services related to Transmission."
    (list
     (emacs-xdg-service
      f-name
      "Emacs (Client) [BitTorrent:]"
      #~(system*
         #$(file-append (get-value 'emacs config) "/bin/emacsclient") "--eval"
         (string-append
          "\
(progn
 (transmission-add \"" (cadr (command-line)) "\")
 (revert-buffer))"))
      #:default-for '(x-scheme-handler/magnet application/x-bittorrent))
     (service home-transmission-service-type
              (home-transmission-configuration
               (transmission transmission)))
     (rde-elisp-configuration-service
      f-name
      config
      `((require 'configure-rde-keymaps)
        (define-key rde-app-map "T" 'transmission))
      #:elisp-packages (list emacs-transmission
                             (get-value 'emacs-configure-rde-keymaps config)))))

  (feature
   (name 'transmission)
   (values `((,f-name . ,transmission)
             (emacs-transmission . ,emacs-transmission)))
   (home-services-getter get-home-services)))
