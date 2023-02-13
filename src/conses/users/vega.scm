(define-module (conses users vega)
  #:use-module (conses feature-list)
  #:use-module (conses features emacs-xyz)
  #:use-module (conses features keyboard)
  #:use-module (conses features video)
  #:use-module (conses hosts base)
  #:use-module (conses utils)
  #:use-module (rde features)
  #:use-module (rde features android)
  #:use-module (rde features base)
  #:use-module (rde features documentation)
  #:use-module (rde features emacs)
  #:use-module (rde features linux)
  #:use-module (rde features gnupg)
  #:use-module (rde features gtk)
  #:use-module (rde features networking)
  #:use-module (rde features nyxt-xyz)
  #:use-module (rde features scheme)
  #:use-module (rde features ssh)
  #:use-module (rde features virtualization)
  #:use-module (rde features web)
  #:use-module (rde features web-browsers)
  #:use-module (rde packages)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services linux)
  #:use-module (gnu system keyboard)
  #:use-module (guix gexp))


;;; Service extensions

(define extra-shell-envs-service
  (simple-service
   'add-missing-shell-envs
   home-environment-variables-service-type
   '(("GPG_TTY" . "$(tty)")
     ("LESSHISTFILE" . "-"))))

(define extra-home-packages-service
  (simple-service
   'add-extra-home-packages
   home-profile-service-type
   (strings->packages
    "haunt" "ddcutil" "light" "xclip"
    "nasm" "gcc-toolchain" "autoconf"
    "v4l-utils" "binutils" "wireguard-tools"
    "texinfo" "pass-otp")))

(define guix-shell-authorized-directories
  (map (lambda (dir)
         (string-append (getenv "HOME") "/" dir))
       (list
        "src/projects/fdroid.el"
        "src/projects/nyxt.el"
        "src/projects/dotfiles"
        "src/projects/tubo"
        "src/projects/dojo"
        "src/projects/blog")))

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

(define extra-init-el
  '((add-hook 'after-init-hook 'server-start)
    (with-eval-after-load 'password-cache
      (setq password-cache t)
      (setq password-cache-expiry (* 60 10)))
    (with-eval-after-load 'pass
      (setq pass-show-keybindings nil))
    (with-eval-after-load 'epg-config
      (setq epg-pinentry-mode 'loopback))
    (with-eval-after-load 'pinentry-autoloads
      (add-hook 'after-init-hook 'pinentry-start))
    (with-eval-after-load 'password-store
      (setq password-store-time-before-clipboard-restore 60))
    (setq-default frame-title-format '("%b - Emacs"))
    (with-eval-after-load 'frame
      (add-to-list 'initial-frame-alist '(fullscreen . maximized)))
    (setq mode-line-misc-info
          (remove '(global-mode-string ("" global-mode-string))
                  mode-line-misc-info))
    (setq echo-keystrokes 0)
    (setq ring-bell-function 'ignore)
    (setq visible-bell nil)
    (fset 'yes-or-no-p 'y-or-n-p)
    (transient-mark-mode)
    (delete-selection-mode)
    (tooltip-mode -1)
    (with-eval-after-load 'prog-mode
      (setq prettify-symbols-unprettify-at-point 'right-edge)
      (setq-default prettify-symbols-alist
                    '((":LOGBOOK:" . "")
                      (":PROPERTIES:" . "")
                      ("# -*-" . "")
                      ("-*-" . ""))))
    (with-eval-after-load 'rde-completion
      (add-to-list 'rde-completion-initial-narrow-alist
                   '(cider-repl-mode . ?c)))
    (with-eval-after-load 'face-remap
      (setq text-scale-mode-step 1.075))
    (with-eval-after-load 'comp
      (setq native-comp-async-report-warnings-errors nil))
    (setq-default tab-width 2)
    (with-eval-after-load 'indent
      (setq tab-always-indent 'complete))
    (global-so-long-mode)
    (require 'warnings)
    (setq warning-suppress-types '((diary) (auto-save) (org-babel)))
    (setq warning-suppress-log-types '((comp org-babel)))
    (setq warning-minimum-level :error)
    (with-eval-after-load 'autorevert
      (setq auto-revert-remote-files nil))
    (setq auto-save-no-message t)
    (setq create-lockfiles nil)
    (setq delete-old-versions t)
    (setq kept-new-versions 3)
    (setq kept-old-versions 2)
    (setq version-control t)
    (setq remote-file-name-inhibit-cache nil)
    (add-hook 'before-save-hook 'delete-trailing-whitespace)
    (with-eval-after-load 'mwheel
      (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)
                                          ((control) . 1)))
      (setq mouse-wheel-progressive-speed nil)
      (setq mouse-wheel-follow-mouse t)
      (setq scroll-conservatively 100)
      (setq mouse-autoselect-window nil)
      (setq what-cursor-show-names t)
      (setq focus-follows-mouse t))))

(define extra-early-init-el
  '((require 'xdg)
    (setq package-native-compile t)
    (setq package-user-dir
          (expand-file-name "emacs/elpa" (xdg-data-home)))
    (setq auto-save-list-file-prefix
          (expand-file-name "emacs/auto-save-list/.saves-"
                            (xdg-data-home)))))


;;; User-specific features

(define vega-nyxt-features
  (make-feature-list
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
         (format-status-modes status))))))))

(define vega-desktop-features
  (make-feature-list
   %desktop-base-features
   (feature-desktop-services
    #:default-desktop-home-services
    (append (@@ (rde features base) %rde-desktop-home-services)
            (list
             extra-shell-envs-service
             extra-home-packages-service
             (service home-udiskie-service-type)
             (service home-redshift-service-type
                      (home-redshift-configuration
                       (dawn-time "07:00")
                       (dusk-time "20:00"))))))
   (feature-networking)
   (feature-pipewire)))

(define-public %user-features
  (make-feature-list
   (feature-user-info
    #:user-name "vega"
    #:full-name %default-fullname
    #:email %default-email
    #:user-groups '("wheel" "netdev" "audio" "video" "libvirt" "spice")
    #:rde-advanced-user? #t
    #:emacs-advanced-user? #t)
   (feature-gnupg
    #:gpg-primary-key "5F23F458"
    #:ssh-keys '(("D6B4894a600BB392AB2AEDE499CBBCF3E0620B7F6"))
    #:pinentry-flavor 'emacs
    #:default-ttl 34560000)
   (feature-alternative-frontends
    #:google-frontend "http://localhost:5000"
    #:youtube-frontend (string-append "https://" %tubo-host)
    #:reddit-frontend "https://teddit.namazso.eu")
   (feature-android)
   (feature-emacs-fdroid)
   (feature-manpages)
   (feature-emacs
    #:emacs (@ (gnu packages emacs) emacs-next)
    #:default-application-launcher? #t
    #:emacs-server-mode? #f
    #:extra-early-init-el extra-early-init-el
    #:extra-init-el extra-init-el
    #:additional-elisp-packages
    (strings->packages
     "emacs-tempel-collection" "emacs-ox-haunt" "emacs-pinentry"))
   (feature-nyxt
    #:scroll-distance 150
    #:temporary-history? #t
    #:smooth-scrolling? #t
    #:autostart-slynk? #t
    #:default-browser? #t
    #:default-new-buffer-url "nyxt:nx-mosaic:mosaic"
    #:restore-session? #f)
   %ui-base-features
   (feature-gtk3
    #:dark-theme? #t
    #:gtk-theme #f
    #:extra-gtk-settings extra-gtk-settings)
   %emacs-completion-base-features
   %emacs-base-features
   %emacs-desktop-base-features
   vega-nyxt-features
   vega-desktop-features
   %multimedia-base-features
   %web-base-features
   %mail-base-features
   %security-base-features
   %shell-base-features
   %forge-base-features
   %communication-base-features
   %programming-base-features
   %markup-base-features
   (feature-emacs-dashboard
    #:item-generators
    '((recents . dashboard-insert-recents)
      (bookmarks . dashboard-insert-bookmarks)
      (agenda . dashboard-insert-agenda)
      (registers . dashboard-insert-registers))
    #:items
    '((agenda . 7)
      (bookmarks . 7)
      (recents . 7))
    #:dashboard-agenda-prefix-format "%?-12:c"
    #:path-max-length 50)
   (feature-emacs-polymode)
   (feature-qmk
    #:keyboard "dztech/dz65rgb/v1"
    #:keymap "custom")
   (feature-keyboard
    #:keyboard-layout %default-keyboard-layout
    #:default-input-method "spanish-keyboard")
   (feature-guix
    #:shell-authorized-directories guix-shell-authorized-directories)
   (feature-ssh
    #:ssh-configuration extra-ssh-config)
   (feature-qemu)))
