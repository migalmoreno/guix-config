(define-module (conses home hydri)
  #:use-module (conses features xorg)
  #:use-module (conses features emacs)
  #:use-module (conses features security)
  #:use-module (conses features emacs-xyz)
  #:use-module (conses features shellutils)
  #:use-module (conses features web-browsers)
  #:use-module (conses features version-control)
  #:use-module (rde packages)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs))

(define-public %home-features
  (list
   (feature-user-info
    #:user-name "hydri"
    #:full-name (getenv "MAIL_PERSONAL_FULLNAME")
    #:email (getenv "MAIL_PERSONAL_EMAIL")
    #:user-groups '("wheel" "netdev" "audio" "video")
    #:rde-advanced-user? #t
    #:emacs-advanced-user? #t)
   (feature-base-packages
    #:home-packages
    (strings->packages
     "make" "nss-certs"))
   (feature-emacs #:emacs emacs-next-pgtk)
   ;; (feature-gtk
   ;;  #:gtk-theme (lambda (config)
   ;;                `((.phosh-topbar-clock
   ;;                   ((margin-left . 125px))))))
   (feature-emacs-all-the-icons)
   (feature-emacs-completion)
   (feature-gnupg
    #:gpg-primary-key (getenv "GPG_PUBLIC_KEY")
    #:ssh-keys '(("A23B61B2897F524D3D3410E1180423144F1DDB4E"))
    #:pinentry-flavor 'emacs
    #:default-ttl 34560000
    #:gpg-agent-extra-config
    '((no-greeting . #t)
      (allow-preset-passphrase . #t)))
   (feature-custom-services
    #:home-services
    (list
     (simple-service
      'home-custom-environment-variables
      home-environment-variables-service-type
      '(("GPG_TTY" . "$(tty)")
        ("LESSHISTFILE" . "-")
        ("HISTFILE" . "$XDG_CACHE_HOME/.bash_history")))))
   ;; (feature-nyxt
   ;;  #:default-browser? #t)
   (feature-emacs-files)
   (feature-desktop-services)
   (feature-cursor)
   (feature-emacs-modus-themes
    #:dark? #t)
   (feature-emacs-vertico)
   (feature-emacs-window)
   (feature-emacs-appearance
    #:auto-theme? #f
    #:margin 0)
   (feature-forge-settings
    #:forge-accounts
    (list
     (forge-account
      (id 'sh)
      (forge 'sourcehut)
      (username (getenv "USERNAME"))
      (email (getenv "SOURCEHUT_EMAIL"))
      (token (getenv "SOURCEHUT_TOKEN")))
     (forge-account
      (id 'gh)
      (forge 'github)
      (username (getenv "GITHUB_USER"))
      (email (getenv "GITHUB_EMAIL"))
      (token (getenv "GITHUB_TOKEN")))))
   (feature-git
    #:primary-forge-account-id 'sh
    #:sign-commits? #t
    #:global-ignores '("**/.direnv"
                       "node_modules"
                       "*.elc"
                       ".log"))
   (feature-compile)
   (feature-password-store)))
