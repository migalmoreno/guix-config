(define-module (conses home vega)
  #:use-module (conses features)
  #:use-module (conses features android)
  #:use-module (conses features documentation)
  #:use-module (conses features emacs)
  #:use-module (conses features emacs-xyz)
  #:use-module (conses features gtk)
  #:use-module (conses features keyboard)
  #:use-module (conses features nyxt-xyz)
  #:use-module (conses features scheme)
  #:use-module (conses features video)
  #:use-module (conses features web)
  #:use-module (conses features web-browsers)
  #:use-module (conses home services linux)
  #:use-module (conses utils)
  #:use-module (rde features)
  #:use-module (rde features ssh)
  #:use-module (rde features base)
  #:use-module (rde features linux)
  #:use-module (rde features virtualization)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu system keyboard)
  #:use-module (guix gexp))


;;; Service extensions

(define extra-shell-envs-service
  (simple-service
   'add-missing-shell-envs
   home-environment-variables-service-type
   '(("GPG_TTY" . "$(tty)")
     ("LESSHISTFILE" . "-"))))

(define extra-gtk-settings
  `((gtk-cursor-blink . #f)
    (gtk-cursor-theme-size . 16)
    (gtk-decoration-layout . "")
    (gtk-dialogs-use-header . #f)
    (gtk-enable-animations . #t)
    (gtk-enable-event-sounds . #f)
    (gtk-enable-input-feedback-sounds . #f)
    (gtk-error-bell . #f)
    (gtk-overlay-scrolling . #t)
    (gtk-recent-files-enabled . #f)
    (gtk-shell-shows-app-menu . #f)
    (gtk-shell-shows-desktop . #f)
    (gtk-shell-shows-menubar . #f)
    (gtk-xft-antialias . #t)
    (gtk-xft-dpi . 92)
    (gtk-xft-hinting . #t)
    (gtk-xft-hintstyle . hintfull)
    (gtk-xft-rgba . none)))

(define extra-ssh-config
  (home-ssh-configuration
   (extra-config
    (list
     (ssh-host
      (host "cygnus")
      (options
       `((host-name . ,(getenv "CYGNUS_HOST"))
         (user . "root"))))
     (ssh-host
      (host "deneb")
      (options
       `((host-name . ,(getenv "CYGNUS_HOST"))
         (user . "deneb"))))
     (ssh-host
      (host "hydri")
      (options
       `((host-name . ,(getenv "HYDRI_HOST"))
         (user . "hydri"))))))))


;;; Home features

(define-public %home-features
  (make-feature-list
   (feature-user-info
    #:user-name "vega"
    #:full-name (getenv "MAIL_PERSONAL_FULLNAME")
    #:email (getenv "MAIL_PERSONAL_EMAIL")
    #:user-groups '("wheel" "netdev" "audio" "video" "libvirt" "spice")
    #:rde-advanced-user? #t
    #:emacs-advanced-user? #t)
   (feature-alternative-frontends
    #:google-frontend "http://localhost:5000"
    #:youtube-frontend (string-append "https://" (getenv "TAU_URL"))
    #:reddit-frontend "https://teddit.namazso.eu")
   (feature-android)
   (feature-emacs-fdroid)
   (feature-manpages)
   (feature-emacs
    #:emacs (@ (gnu packages emacs) emacs-next)
    #:default-application-launcher? #f
    #:emacs-server-mode? #f
    #:extra-init-el
    '((add-hook 'after-init-hook 'server-start)))
   %ui-base-features
   (feature-gtk3
    #:dark-theme? #t
    #:gtk-theme #f
    #:extra-gtk-settings extra-gtk-settings)
   %emacs-completion-base-features
   (feature-nyxt
    #:scroll-distance 150
    #:temporary-history? #t
    #:smooth-scrolling? #t
    #:autostart-slynk? #t
    #:default-browser? #t
    #:default-new-buffer-url "nyxt:nx-mosaic:mosaic"
    #:restore-session? #f)
   %nyxt-base-features
   (feature-nyxt-prompt #:mouse-support? #f)
   (feature-nyxt-status
    #:height 30
    #:glyphs? #t
    #:format-status-buttons
    '((:raw
       (format-status-back-button status)
       (format-status-reload-button status)
       (format-status-forwards-button status)
       (format-status-close-button status)))
    #:format-status
    '((:div :id "container"
       (:div :id "controls"
        (:raw (format-status-buttons status)))
       (:div :id "url"
        (:raw
         (format-status-load-status status)
         (format-status-url status)))
       (:div :id "modes"
             :title (nyxt::modes-string buffer)
        (:raw
         (format-status-modes status))))))
   %multimedia-base-features
   %emacs-desktop-base-features
   %emacs-base-features
   (feature-desktop-services
    #:default-desktop-home-services
    (append (@@ (rde features base) %rde-desktop-home-services)
            (list
             extra-shell-envs-service
             (service home-udiskie-service-type)
             (service home-redshift-service-type
                      (home-redshift-configuration
                       (dawn-time "07:00")
                       (dusk-time "20:00"))))))
   %desktop-base-features
   (feature-pipewire)
   %web-base-features
   %mail-base-features
   %security-base-features
   %shell-base-features
   %forge-base-features
   %communication-base-features
   %programming-base-features
   %markup-base-features
   (feature-emacs-dashboard
    #:emacs-dashboard (@ (conses packages emacs-xyz) emacs-dashboard-next)
    #:logo-title "Welcome to GNU/Emacs"
    #:item-generators '((recents . dashboard-insert-recents)
                        (bookmarks . dashboard-insert-bookmarks)
                        (agenda . dashboard-insert-agenda)
                        (registers . dashboard-insert-registers))
    #:items '((agenda . 7)
              (bookmarks . 7)
              (recents . 7))
    #:navigator-buttons '((("☆" "Calendar" "Show calendar"
                            (lambda (&rest _)
                              (calendar)) diary "[" "]")))
    #:banner (file-append (@ (conses packages misc) gnu-meditate-logo) "/meditate.png")
    #:org-agenda-prefix-format "%?-12:c"
    #:banner-max-height 320
    #:banner-max-width 240
    #:path-max-length 50
    #:bookmarks-show-base? #f
    #:path-style 'truncate-beginning
    #:set-heading-icons? #t
    #:set-file-icons? #f
    #:set-footer? #f
    #:set-init-info? #f)
   (feature-emacs-polymode)
   (feature-qmk
    #:keyboard "dztech/dz65rgb/v1"
    #:keymap "custom")
   (feature-keyboard
    #:keyboard-layout (@ (conses system) %default-keyboard-layout)
    #:default-input-method "spanish-keyboard")
   (feature-guix
    #:authorized-directories
    '("~/src/projects/fdroid.el"
      "~/src/projects/nyxt.el"
      "~/src/projects/dotfiles"
      "~/src/projects/tau"))
   (feature-ssh
    #:ssh-configuration extra-ssh-config)
   (feature-qemu)))
