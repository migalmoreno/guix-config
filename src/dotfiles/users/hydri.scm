(define-module (dotfiles users hydri)
  #:use-module (dotfiles common)
  #:use-module (dotfiles utils)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features bittorrent)
  #:use-module (rde features bluetooth)
  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features fontutils)
  #:use-module (rde features gnupg)
  #:use-module (rde features gtk)
  #:use-module (rde features nyxt-xyz)
  #:use-module (rde features shellutils)
  #:use-module (rde features version-control)
  #:use-module (rde features xdg)
  #:use-module (rde features web)
  #:use-module (rde features web-browsers)
  #:use-module (rde packages)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (rde home services shells)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services xdg)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils))


;;; User-specific utilities

(define-public %hydri-signing-key
  (project-file "src/dotfiles/keys/hydri.pub"))

(define-public %hydri-ssh-key
  (plain-file
   "hydri.pub"
   "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ08QYeHnqdrWzd8JnASbXJKeDqS5Kmfsd3RUeWP+YyS\n"))

(define-public sbcl-montezuma-sans-tests
  (let ((sbcl-montezuma (@ (gnu packages lisp-xyz) sbcl-montezuma)))
    (package
      (inherit sbcl-montezuma)
      (arguments
       (append
        (package-arguments sbcl-montezuma)
        (list #:tests? #f))))))

(define nyxt-next
  (let ((nyxt-next (@ (rde packages web-browsers) nyxt-next)))
    (package
      (inherit nyxt-next)
      (name "nyxt-next")
      (inputs
       (modify-inputs (package-inputs nyxt-next)
         (delete "sbcl-montezuma")
         (append sbcl-montezuma-sans-tests))))))


;;; Service extensions

(define extra-shepherd-services-service
  (simple-service
   'run-syncthing-on-userspace
   home-shepherd-service-type
   (list
    (shepherd-service
     (provision '(syncthing))
     (documentation "Run syncthing.")
     (start #~(make-forkexec-constructor
               (list #$(file-append (@ (gnu packages syncthing) syncthing)
                                    "/bin/syncthing")
                     "-no-browser" "-no-restart")))
     (respawn? #f)
     (stop #~(make-kill-destructor))))))

(define extra-bashrc-service
  (simple-service
   'add-bashrc
   home-bash-service-type
   (home-bash-extension
    (bashrc
     (list
      "gpg-connect-agent updatestartuptty /bye > /dev/null"
      "export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)")))))

(define extra-home-envs-service
  (simple-service
   'override-global-gtk-theme-on-wayland
   home-environment-variables-service-type
   '(("GTK_THEME" . "postmarketos-oled")
     ("SSL_CERT_DIR" . "/etc/ssl/certs")
     ("SSL_CERT_FILE" . "/etc/ssl/certs/ca-certificates.crt"))))

(define extra-home-packages
  (strings->packages
   "nss-certs" "glibc-locales" "seahorse"
   "gnome-clocks" "pinentry-tty"
   "portfolio" "pavucontrol" "geary"
   "lollypop" "gst-plugins-good" "gst-plugins-bad"
   "gst-plugins-ugly" "gst-plugins-base" "gst-libav"
   "gnome-console"))

(define extra-gtk-css
  `((.phosh-topbar-clock
     ((margin-left . 125px)))))


;;; User-specific features

(define hydri-nyxt-features
  (make-feature-list
   (feature-nyxt
    #:nyxt nyxt-next
    #:default-browser? #t
    #:restore-session? #f
    #:temporary-history? #t
    #:extra-config-lisp %base-nyxt-extra-config-lisp)
   (feature-nyxt-appearance
    #:status-buffer-height 40
    #:status-buffer-position ':bottom
    #:dark? #t)
   (feature-nyxt-status
    #:format-status-buttons
    '((:raw
       (format-status-back-button status)
       (format-status-reload-button status)
       (format-status-forwards-button status)
       (format-status-close-button status)
       (format-status-new-buffer-button status)
       (format-status-switch-buffer-button status)
       (format-status-execute-button status)))
    #:format-status
    '((:div :id "container"
       (:div :id "controls"
        (:raw (format-status-buttons status)))
       (:div :id "url"
        (:raw
         (format-status-load-status status)
         (format-status-url status))))))
   %nyxt-base-features))

(define-public %user-features
  (make-feature-list
   (feature-user-info
    #:user-name "hydri"
    #:full-name %default-fullname
    #:email %default-email
    #:rde-advanced-user? #t
    #:emacs-advanced-user? #t)
   (feature-base-packages
    #:home-packages extra-home-packages)
   (feature-custom-services
    #:home-services
    (list
     extra-bashrc-service
     extra-shepherd-services-service
     extra-home-envs-service))
   (feature-emacs
    #:emacs (@ (gnu packages emacs) emacs-next-pgtk)
    #:emacs-server-mode? #f
    #:extra-init-el
    '((add-hook 'after-init-hook 'server-start)))
   (feature-gnupg
    #:gpg-primary-key "5F23F458"
    #:ssh-keys `((,%default-ssh-keygrip))
    #:pinentry-flavor 'tty
    #:default-ttl 34560000)
   (feature-emacs-appearance
    #:header-line-as-mode-line? #f
    #:margin 0)
   (feature-emacs-modus-themes
    #:dark? #t
    #:deuteranopia? #f
    #:headings-scaling? #t
    #:extra-modus-themes-overrides
    '((bg-mode-line-active bg-dim)))
   (feature-fonts
    #:font-serif
    (font
     (name "IBM Plex Serif")
     (size 11)
     (package (@ (gnu packages fonts) font-ibm-plex)))
    #:font-sans
    (font
     (name "IBM Plex Sans")
     (size 11)
     (package (@ (gnu packages fonts) font-ibm-plex))
     (weight 'light))
    #:font-unicode
    (font
     (name "Noto Color Emoji")
     (size 11)
     (package (@ (gnu packages fonts) font-google-noto))))
   (feature-gtk3
    #:dark-theme? #t
    #:gtk-theme (make-theme
                 "postmarketos-oled"
                 (@ (gnu packages gnome-xyz) postmarketos-theme))
    #:icon-theme (make-theme
                  "Adwaita"
                  (@ (gnu packages gnome) adwaita-icon-theme))
    #:extra-gtk-css extra-gtk-css)
   (feature-alternative-frontends
    #:youtube-frontend (string-append "https://" %tubo-host)
    #:google-frontend #f)
   hydri-nyxt-features
   %multimedia-base-features
   %emacs-base-features
   %desktop-base-features
   %security-base-features
   %shell-base-features
   %forge-base-features
   %communication-base-features
   %programming-base-features
   %markup-base-features))
