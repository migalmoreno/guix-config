(define-module (quasar home services security)
  #:use-module (quasar home)
  #:use-module (conses home services emacs)
  #:use-module (gnu home-services gnupg)
  #:use-module (gnu home-services password-utils)
  #:use-module (gnu home-services base)
  #:use-module (gnu home-services ssh)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:export (ssh-service
            gnupg-service
            password-service))

(define (ssh-service)
  (list
   (service home-ssh-service-type
            (home-ssh-configuration
             (default-host "*")
             (default-options
               '(;; (ControlPersist . "4h")
                 ;; (TCPKeepAlive . "no")
                 ;; (ServerAliveInterval . 30)
                 ;; (ServerAliveCountMax . 5)
                 ))
             (extra-config
              (list
               (ssh-host
                (host "cygnus")
                (options
                 `((host-name . ,(getenv "CYGNUS_IP"))
                   (user . "root"))))))))))

(define (gnupg-service)
  (list
   (home-generic-service 'home-gnupg-packages
                         #:packages (list pinentry-emacs emacs-pinentry))
   (simple-service 'add-gnupg-envs
                   home-environment-variables-service-type
                   '(("GPG_TTY" . "$(tty)")))
   (service home-gnupg-service-type
            (home-gnupg-configuration
             (gpg-agent-config
              (home-gpg-agent-configuration
               (ssh-agent? #t)
               (ssh-keys
                '(("A23B61B2897F524D3D3410E1180423144F1DDB4E")))
               (pinentry-flavor 'emacs)
               (extra-config
                '((allow-preset-passphrase . #t)
                  (max-cache-ttl . 34560000)
                  (max-cache-ttl-ssh . 34560000)
                  (digest-algo . sha512)
                  (no-greeting . #t)))))))))

(define (password-service)
  (list
   (service home-password-store-service-type
            (home-password-store-configuration
             (directory "${XDG_DATA_HOME:-$HOME/.local/share}/password-store")))
   (home-generic-service 'home-password-packages #:packages (list pass-otp))
   (elisp-configuration-service
    `((with-eval-after-load 'pass
        (custom-set-variables
         '(pass-show-keybindings nil)))
      ,#~""
      (define-key mode-specific-map "P" 'password-store-copy)
      (with-eval-after-load 'password-store
        (custom-set-variables
         '(password-store-time-before-clipboard-restore 60)))
      ,#~""
      (with-eval-after-load 'auth-source
        (setenv "GPG_AGENT_INFO" nil)
        (pinentry-start)
        (custom-set-variables
         '(auth-sources '(password-store))
         '(auth-source-pass-filename (expand-file-name "password-store" (xdg-data-home)))))
      (with-eval-after-load 'epg-config
        (custom-set-variables
         '(epg-pinentry-mode 'loopback)
         '(epa-pinentry-mode 'loopback)))
      ,#~""
      (with-eval-after-load 'password-cache
        (custom-set-variables
         '(password-cache t)
         '(password-cache-expiry (* 60 10))))
      ,#~""
      (with-eval-after-load 'embark
        (embark-define-keymap embark-password-store-actions
                              "Kemaps for actions of password-store."
                              ("c" password-store-copy)
                              ("f" password-store-copy-field)
                              ("o" password-store-otp-token-copy)
                              ("i" password-store-insert)
                              ("I" password-store-generate)
                              ("r" password-store-rename)
                              ("e" password-store-edit)
                              ("k" password-store-remove)
                              ("U" password-store-url))
        (add-to-list 'embark-keymap-alist '(password-store . embark-password-store-actions))
        (with-eval-after-load 'marginalia
          (add-to-list 'marginalia-prompt-categories '("Password entry" . password-store)))))
    #:elisp-packages (list emacs-pass emacs-password-store emacs-password-store-otp))))
