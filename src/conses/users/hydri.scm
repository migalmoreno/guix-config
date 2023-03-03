(define-module (conses users hydri)
  #:use-module (conses hosts base)
  #:use-module (conses feature-list)
  #:use-module (conses features fontutils)
  #:use-module (conses features emacs-xyz)
  #:use-module (conses features shellutils)
  #:use-module (conses features version-control)
  #:use-module (conses utils)
  #:use-module (rde packages)
  #:use-module (rde packages web-browsers)
  #:use-module (rde features)
  #:use-module (rde features base)
  #:use-module (rde features bittorrent)
  #:use-module (rde features bluetooth)
  #:use-module (rde features emacs)
  #:use-module (rde features gnupg)
  #:use-module (rde features gtk)
  #:use-module (rde features nyxt-xyz)
  #:use-module (rde features xdg)
  #:use-module (rde features web)
  #:use-module (rde features web-browsers)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services xdg)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages package-management)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils))


;;; User-specific utilities

(define-public %hydri-signing-key
  (project-file "src/conses/keys/hydri.pub"))

(define-public %hydri-ssh-key
  (plain-file
   "hydri.pub"
   "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ08QYeHnqdrWzd8JnASbXJKeDqS5Kmfsd3RUeWP+YyS\n"))

(define nyxt-next-sans-gst
  (package
   (inherit nyxt-next)
   (name "nyxt-next-sans-gst")
   (propagated-inputs
    (modify-inputs (package-propagated-inputs nyxt-next)
      (delete "gst-libav" "gst-plugins-bad" "gst-plugins-base"
              "gst-plugins-good" "gst-plugins-ugly")))))


;;; Service extensions

(define extra-xdg-config-service
  (simple-service
   'add-nyxt-xdg-entry-with-gst-plugins
   home-xdg-mime-applications-service-type
   (home-xdg-mime-applications-configuration
    (desktop-entries
     (list
      (xdg-desktop-entry
       (file "nyxt")
       (name "Nyxt")
       (type 'application)
       (config
        `((exec . #~(string-join
                     (list
                      #$(file-append current-guix "/bin/guix")
                      "shell"
                      (@ (gnu packages gstreamer) gst-plugins-good)
                      (@ (gnu packages gstreamer) gst-plugins-bad)
                      (@ (gnu packages gstreamer) gst-plugins-ugly)
                      (@ (gnu packages gstreamer) gst-plugins-base)
                      (@ (gnu packages gstreamer) gst-libav)
                      "--"
                      #$(file-append nyxt-next-sans-gst "/bin/nyxt"))
                     "%U"))
          (terminal . #f)
          (icon . "nyxt")
          (comment . "Be productive")))))))))

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

(define extra-home-envs-service
  (simple-service
   'override-global-gtk-theme-on-wayland
   home-environment-variables-service-type
   '(("GTK_THEME" . "postmarketos-oled"))))

(define extra-home-packages
  (strings->packages
   "nss-certs" "glibc-locales" "seahorse"
   "gnome-clocks" "gnome-console" "pinentry-tty"
   "portfolio" "pavucontrol" "geary" "gnome-maps"
   "nheko" "chromium" "evince"))


;;; User-specific features

(define hydri-nyxt-features
  (make-feature-list
   (feature-nyxt
    #:nyxt (@ (rde packages web-browsers) nyxt-next-sans-gst)
    #:default-browser? #t
    #:restore-session? #f
    #:temporary-history? #t)
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
     extra-xdg-config-service
     extra-shepherd-services-service
     extra-home-envs-service))
   (feature-emacs
    #:emacs emacs-next-pgtk
    #:emacs-server-mode? #f
    #:extra-init-el
    '((add-hook 'after-init-hook 'server-start)))
   (feature-gnupg
    #:gpg-primary-key "5F23F458"
    #:ssh-keys `((,%default-ssh-keygrip))
    #:pinentry-flavor 'tty
    #:default-ttl 34560000)
   (feature-fonts)
   (feature-emacs-appearance
    #:header-line-as-mode-line? #f
    #:margin 0)
   (feature-emacs-tab-bar
    #:modules-center %rde-mpv-tab-bar-modules)
   (feature-gtk3
    #:dark-theme? #t
    #:gtk-theme (make-theme
                 "postmarketos-oled"
                 (@ (conses packages gnome-xyz) postmarketos-theme))
    #:icon-theme (make-theme
                  "Adwaita"
                  (@ (gnu packages gnome) adwaita-icon-theme))
    #:custom-gtk-theme (lambda _
                         `((.phosh-topbar-clock
                            ((margin-left . 125px))))))
   %emacs-completion-base-features
   (feature-alternative-frontends
    #:youtube-frontend (string-append "https://" %tubo-host)
    #:google-frontend #f)
   hydri-nyxt-features
   %multimedia-base-features
   %emacs-base-features
   %desktop-base-features
   %web-base-features
   %security-base-features
   %shell-base-features
   %forge-base-features
   %communication-base-features
   %programming-base-features
   %markup-base-features))
