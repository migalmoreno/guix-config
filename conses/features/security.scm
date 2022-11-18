(define-module (conses features security)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home-services state)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu home-services password-utils)
  #:use-module (guix gexp)
  #:export (feature-password-store))

(define* (feature-password-store
          #:key
          (password-store password-store)
          (remote-password-store-url #f)
          (password-store-directory
           (if (getenv "XDG_STATE_HOME")
               (string-append (getenv "XDG_STATE_HOME") "/password-store")
               (string-append (getenv "HOME") "/.local/var/lib/password-store"))))
  "Configure Pass, the standard Unix password manager."
  (ensure-pred file-like? password-store)
  (ensure-pred maybe-string? remote-password-store-url)
  (ensure-pred path? password-store-directory)

  (define f-name 'pass)

  (define (get-home-services config)
    "Return home services related to Pass."
    (list
     (service home-password-store-service-type
              (home-password-store-configuration
               (package password-store)
               (directory password-store-directory)))
     (simple-service
      'password-store-directory-state
      home-state-service-type
      (list
       (state-git
        password-store-directory
        remote-password-store-url)))
     (simple-service
      'home-password-packages
      home-profile-service-type
      (list pass-otp))
     (rde-elisp-configuration-service
      f-name
      config
      `((require 'configure-rde-keymaps)
        (with-eval-after-load 'auth-source
          (require 'password-store)
          (setenv "GPG_AGENT_INFO" nil)
          (setq auth-sources '(password-store))
          (setq auth-source-pass-filename ,password-store-directory))
        (define-key rde-app-map "p" 'password-store-copy)
        (with-eval-after-load 'password-store
          (setq password-store-time-before-clipboard-restore 60))
        (with-eval-after-load 'pass
          (setq pass-show-keybindings nil))
        (with-eval-after-load 'epg-config
          (setq epg-pinentry-mode 'loopback))
        (with-eval-after-load 'pinentry-autoloads
          (add-hook 'after-init-hook 'pinentry-start))
        (with-eval-after-load 'password-cache
          (setq password-cache t)
          (setq password-cache-expiry (* 60 10)))
        ,@(if (get-value 'emacs-embark config)
              '((require 'embark)
                (with-eval-after-load 'embark
                  (embark-define-keymap embark-password-store-actions
                    "Kemaps for actions of password-store."
                    ("c" password-store-copy)
                    ("f" password-store-copy-field)
                    ("o" password-store-otp-token-copy)
                    ("i" password-store-insert)
                    ("g" password-store-generate)
                    ("r" password-store-rename)
                    ("e" password-store-edit)
                    ("d" password-store-remove)
                    ("u" password-store-url))
                  (add-to-list 'embark-keymap-alist '(password-store . embark-password-store-actions))
                  (with-eval-after-load 'marginalia
                    (add-to-list 'marginalia-prompt-categories '("Password entry" . password-store)))))
              '()))
      #:elisp-packages (append
                        (list emacs-pass
                              emacs-password-store
                              emacs-password-store-otp
                              emacs-pinentry
                              (get-value 'emacs-configure-rde-keymaps config))
                        (if (get-value 'emacs-embark config)
                            (list (get-value 'emacs-embark config))
                            '())))))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             (password-store . ,password-store)))
   (home-services-getter get-home-services)))
