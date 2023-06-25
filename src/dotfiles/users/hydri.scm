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
  #:use-module (rde features lisp)
  #:use-module (rde features nyxt-xyz)
  #:use-module (rde features shellutils)
  #:use-module (rde features terminals)
  #:use-module (rde features version-control)
  #:use-module (rde features xdg)
  #:use-module (rde features web)
  #:use-module (rde features web-browsers)
  #:use-module (rde home services shells)
  #:use-module (rde packages)
  #:use-module (gnu services)
  #:use-module (gnu home services)
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
   "portfolio" "pavucontrol"
   "lollypop" "gst-plugins-good" "gst-plugins-bad"
   "gst-plugins-ugly" "gst-plugins-base" "gst-libav"))

(define hydri-offload-service
  (simple-service
   'add-guix-machines
   activation-service-type
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (let ((machines-file "/etc/guix/machines.scm"))
           (if (file-exists? machines-file)
               (if (and (symbolic-link? machines-file)
                        (store-file-name? (readlink machines-file)))
                   (delete-file machines-file)
                   (rename-file machines-file "/etc/guix/machines.scm.bak"))
               (mkdir-p "/etc/guix"))
           (symlink #$(project-file "src/dotfiles/machines/lyra.scm")
                    machines-file))))))

(define hydri-extra-services
  (list
   hydri-offload-service
   extra-bashrc-service
   extra-shepherd-services-service
   extra-home-envs-service
   (service home-shell-profile-service-type)))

(define (extra-gtk-css _)
  `((.phosh-topbar-clock
     ((margin-left . 125px)))))

(define nyxt-extra-config-lisp
  `(,@%base-nyxt-extra-config-lisp
    (define-configuration nyxt/mode/reduce-tracking:reduce-tracking-mode
      ((nyxt/mode/reduce-tracking:preferred-user-agent
        "Mozilla/5.0 (Linux; Android 10; Google Pixel 4
 Build/QD1A.190821.014.C2; wv) AppleWebKit/537.36 (KHTML, like Gecko)
 Version/4.0 Chrome/78.0.3904.108 Mobile Safari/537.36")))

    (defmethod format-status-buttons :around ((status status-buffer))
      (spinneret:with-html-string
        (:raw
         (status-button
          status "Backward" 'nyxt/mode/history:history-backwards "⊲")
         (status-button status "Reload" 'nyxt:reload-current-buffer "↺")
         (status-button
          status "Forward" 'nyxt/mode/history:history-backwards "⊳")
         (status-button status "Close" 'nyxt:delete-current-buffer "🞫")
         (status-button status "New" 'nyxt:set-url-new-buffer "🞣")
         (status-button status "Switch" 'nyxt:switch-buffer "◱")
         (status-button status "Execute" 'nyxt:execute-command "≡"))))

    (defmethod format-status :around ((status status-buffer))
      (let ((buffer (current-buffer (window status))))
        (spinneret:with-html-string
          (:div :id "container"
                (:div :id "controls"
                      (:raw (format-status-buttons status)))
                (:div :id "url"
                      (:raw
                       (format-status-load-status status)
                       (format-status-url status)))))))))


;;; User-specific features

(define hydri-nyxt-features
  (list*
   (feature-nyxt
    #:default-new-buffer-url "nyxt:nx-mosaic:mosaic"
    #:default-browser? #t
    #:restore-session? #f
    #:temporary-history? #t
    #:extra-config-lisp nyxt-extra-config-lisp)
   (feature-nyxt-appearance
    #:status-buffer-height 40
    #:status-buffer-position ':bottom
    #:dark? #t)
   %nyxt-base-features))

(define-public %hydri-features
  (list*
   (feature-user-info
    #:user-name "hydri"
    #:full-name %default-fullname
    #:email %default-email
    #:rde-advanced-user? #t
    #:emacs-advanced-user? #t)
   (feature-base-packages
    #:base-home-packages '()
    #:home-packages extra-home-packages)
   (feature-custom-services
    #:home-services hydri-extra-services)
   (feature-emacs
    #:emacs (@ (gnu packages emacs) emacs-next-pgtk)
    #:emacs-server-mode? #f
    #:extra-init-el %base-extra-init-el
    #:extra-early-init-el %base-extra-early-init-el)
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
    #:use-serif-for-variable-pitch? #f
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
    #:gtk-dark-theme? #t
    #:gtk-theme (make-theme
                 "postmarketos-oled"
                 (@ (gnu packages gnome-xyz) postmarketos-theme))
    #:icon-theme (make-theme
                  "Adwaita"
                  (@ (gnu packages gnome) adwaita-icon-theme))
    #:extra-gtk-css extra-gtk-css)
   (feature-alternative-frontends
    #:reddit-frontend "https://teddit.pussthecat.org"
    #:youtube-frontend (string-append "https://" %tubo-host)
    #:google-frontend (string-append "https://whoogle." %default-domain))
   (feature-emacs-ebdb
    #:ebdb-sources (list "~/documents/contacts")
    #:ebdb-popup-size 0.2)
   (feature-lisp
    #:extra-lisp-packages
    (strings->packages "sbcl-prove" "sbcl-cl-cffi-gtk" "sbcl-lisp-unit2")
    #:extra-source-registry-files
    (list
     (plain-file
      "10-projects.conf"
      (format #f "(:tree \"~a/src\")" (getenv "HOME")))))
   hydri-nyxt-features
   %communication-base-features
   %multimedia-base-features
   %emacs-base-features
   %desktop-base-features
   %security-base-features
   %shell-base-features
   %forge-base-features
   %org-base-features))
