(define-module (conses home hydri)
  #:use-module (conses utils)
  #:use-module (conses features irc)
  #:use-module (conses features gtk)
  #:use-module (conses features lisp)
  #:use-module (conses features xorg)
  #:use-module (conses features emacs)
  #:use-module (conses features video)
  #:use-module (conses features scheme)
  #:use-module (conses features matrix)
  #:use-module (conses features security)
  #:use-module (conses features nyxt-xyz)
  #:use-module (conses features messaging)
  #:use-module (conses features fontutils)
  #:use-module (conses features bluetooth)
  #:use-module (conses features emacs-xyz)
  #:use-module (conses features bittorrent)
  #:use-module (conses features shellutils)
  #:use-module (conses features web-browsers)
  #:use-module (conses features version-control)
  #:use-module (rde packages)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages emacs)
  #:use-module (guix gexp))

(define-public %hydri-signing-key
  (project-file "conses/keys/hydri.pub"))

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
     "make" "nss-certs" "seahorse"))
   (feature-custom-services
    #:home-services
    (list
     (simple-service
      'run-syncthing-on-userspace
      home-shepherd-service-type
      (list
       (shepherd-service
        (provision '(syncthing))
        (documentation "Run syncthing.")
        (start #~(make-forkexec-constructor
                  (list #$(file-append (@ (gnu packages syncthing) syncthing) "/bin/syncthing")
                        "-no-browser"
                        "-no-restart")))
        (respawn? #f)
        (stop #~(make-kill-destructor)))))))
   (feature-proxy
    #:youtube-proxy "https://invidio.xamh.de"
    #:google-proxy (string-append "https://whoogle." (getenv "DOMAIN")))
   (feature-fonts)
   (feature-emacs
    #:emacs emacs-next-pgtk
    #:emacs-server-mode? #t
    #:extra-init-el
    '((add-hook 'after-init-hook 'server-start)))
   (feature-gtk
    #:dark-theme? #t
    #:gtk-theme (lambda _
                  `((.phosh-topbar-clock
                     ((margin-left . 125px))))))
   (feature-emacs-all-the-icons)
   (feature-emacs-completion
    #:consult-initial-narrowing? #t)
   (feature-gnupg
    #:gpg-primary-key (getenv "GPG_PUBLIC_KEY")
    #:ssh-keys '(("D6B4894600BB392AB2AEDE499CBBCF3E0620B7F6"))
    #:pinentry-flavor 'emacs
    #:default-ttl 34560000
    #:gpg-agent-extra-config
    '((no-greeting . #t)
      (allow-preset-passphrase . #t)))
   (feature-nyxt
    #:nyxt (@ (conses packages web-browsers) nyxt-next-sans-gst)
    #:default-browser? #t
    #:default-new-buffer-url "nyxt:nx-mosaic:mosaic")
   (feature-nyxt-emacs)
   (feature-youtube-dl
    #:emacs-ytdl (@ (conses packages emacs-xyz) emacs-ytdl-next)
    #:music-dl-args '("-q" "-x"  "-f" "bestaudio" "--audio-format" "mp3" "--add-metadata"
                      "--compat-options" "all")
    #:video-dl-args '("-q" "-f" "bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best"
                      "--add-metadata" "--compat-options" "all"))
   (feature-mpv
    #:emacs-mpv (@ (conses packages emacs-xyz) emacs-mpv-next)
    #:extra-mpv-conf
    `((border . no)
      (volume . 100)
      (screenshot-directory . ,(string-append (getenv "XDG_DATA_HOME") "/mpv/screenshots"))
      (autofit . 800x800)
      (osd-border-size . 2)
      (osd-bar . yes)
      (osd-level . 0)
      (slang . en)
      (ytdl-raw-options . "ignore-config=,sub-lang=en,write-auto-sub=")
      (script-opts-add=osc-visibility . never)
      (script-opts-add=osc-windowcontrols . no))
    #:extra-bindings
    `(("F" . "cycle fullscreen")
      ("M" . "cycle mute")
      ("+" . "add volume 2")
      ("-" . "add volume -2")
      (":" . "script-binding console/enable")
      ("s" . "screenshot video")
      ("Q" . "quit-watch-later")
      ("O" . "no-osd cycle-values osd-level 3 0")
      ("o" . "osd-bar show-progress")
      ("v" . "cycle sub-visibility")
      ("b" . "cycle sub")
      ("n" . "script-message osc-visibility always")
      ("N" . "script-message osc-visibility never")
      ("L" . "cycle-values loop-file \"inf\" \"no\"")))
   (feature-emacs-tab-bar
    #:modules-center
    '((make-configure-tab-bar-module
       :id 'mpv-string
       :label 'mpv-mode-line-string)
      (make-configure-tab-bar-module
       :id 'mpv-prev
       :label 'mpv-prev-button
       :help "Previous playlist entry"
       :action 'mpv-playlist-prev)
      (make-configure-tab-bar-module
       :id 'mpv-toggle
       :label 'mpv-toggle-button
       :help "Toggle playback"
       :action 'mpv-pause)
      (make-configure-tab-bar-module
       :id 'mpv-next
       :label 'mpv-next-button
       :help "Next playlist entry"
       :action 'mpv-playlist-next)
      (make-configure-tab-bar-module
       :id 'mpv-playing-time
       :label 'mpv-playing-time-string)))
   (feature-emacs-emms)
   (feature-bluetooth)
   (feature-nyxt-nx-tailor
    #:auto? #f
    #:dark-theme? #t
    #:themes
    '((tailor:make-theme
       'modus-operandi
       :background-color "white"
       :on-background-color "black"
       :primary-color "#e5e5e5"
       :on-primary-color "black"
       :secondary-color "#005a5f"
       :on-secondary-color "black"
       :accent-color "#0000c0"
       :on-accent-color "white"
       :font-family "Iosevka")
      (tailor:make-theme
       'modus-vivendi
       :dark-p t
       :background-color "black"
       :on-background-color "white"
       :primary-color "#212121"
       :on-primary-color "#a8a8a8"
       :secondary-color "#100f10"
       :on-secondary-color "#6ae4b9"
       :accent-color "#00bcff"
       :on-accent-color "black"
       :font-family "Iosevka")))
   (feature-nyxt-prompt)
   (feature-nyxt-mosaic)
   (feature-nyxt-status
    #:height 30
    #:glyphs? #t
    #:format-status-buttons
    '((:raw
       (format-status-back-button status)
       (format-status-reload-button status)
       (format-status-forwards-button status)
       (format-status-close-button status)
       (format-status-switch-buffer-button sttatus)
       (format-status-execute-button status)))
    #:format-status
    '((:div :id "container"
       (:div :id "controls"
        (:raw (format-status-buttons status)))
       (:div :id "url"
        (:raw
         (format-status-load-status status)
         (format-status-url status))))))
   (feature-nyxt-nx-router
    #:media-enabled? #t
    #:extra-routes
    '((router:make-route
       (match-regex ".*/watch\\?.*v=.*")
       :redirect "www.youtube.com"
       :external (lambda (req)
                   (play-video-mpv (url req) :formats nil :audio t :repeat t)))
      (router:make-route
       (match-regex "https://gfycat.com/.*" "https://streamable.com/.*"
                    "https://.*/videos/watch/.*" ".*cloudfront.*master.m3u8")
       :external (lambda (req)
                   (play-video-mpv (url req) :formats nil)))
      (router:make-route
       (match-scheme "magnet")
       :external (lambda (req)
                   (eval-in-emacs
                    `(transmission-add ,(quri:render-uri (url req))))))))
   (feature-nyxt-nx-search-engines
    #:extra-engines
    '((engines:wordnet
       :shortcut "wn"
       :show-examples t
       :show-word-frequencies t
       :show-sense-numbers t)
      (engines:startpage
       :shortcut "sp")
      (engines:sourcehut
       :shortcut "sh")
      (engines:libgen
       :shortcut "lg"
       :covers t
       :results 100
       :object :files
       :fallback-url (quri:uri "http://libgen.gs")
       :base-search-url "https://libgen.gs/index.php?req=~a")
      (engines:peertube
       :shortcut "pt")
      (engines:lemmy
       :shortcut "le")
      (engines:meetup
       :shortcut "me")
      (engines:hacker-news
       :shortcut "hn"
       :fallback-url (quri:uri "https://news.ycombinator.com")
       :search-type :all)
      (engines:lobsters
       :shortcut "lo")
      (make-instance
       'search-engine
       :shortcut "to"
       :search-url "https://torrents-csv.ml/#/search/torrent/~a/1"
       :fallback-url "https://torrents-csv.ml")
      (engines:google
       :shortcut "go"
       :safe-search nil
       :lang-ui :english
       :results-number 50
       :new-window t)))
   (feature-emacs-files)
   (feature-emacs-cursor)
   (feature-direnv)
   (feature-emacs-dired)
   (feature-emacs-browse-url)
   (feature-emacs-modus-themes
    #:dark? #t)
   (feature-emacs-vertico)
   (feature-emacs-window)
   (feature-emacs-appearance
    #:header-line-as-mode-line? #f
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
      (token (getenv "SOURCEHUT_TOKEN")))))
   (feature-git
    #:primary-forge-account-id 'sh
    #:sign-commits? #t)
   (feature-irc-settings
    #:irc-accounts
    (list
     (irc-account
      (id 'srht)
      (network "chat.sr.ht")
      (bouncer? #t)
      (nick (getenv "IRC_BOUNCER_NICK")))
     (irc-account
      (id 'libera)
      (network "irc.libera.chat")
      (nick (getenv "IRC_LIBERA_NICK")))
     (irc-account
      (id 'oftc)
      (network "irc.oftc.net")
      (nick (getenv "IRC_OFTC_NICK")))))
   (feature-emacs-erc
    #:autojoin-channels-alist
    '((Libera.Chat
       "#nyxt" "#emacs" "#org-mode" "#guix" "#ocaml"
       "#clojure" "#commonlisp" "#scheme" "#tropin")
      (OFTC "#postmarketos" "#mobian")))
   (feature-matrix-settings
    #:homeserver (string-append "https://pantalaimon." (getenv "DOMAIN"))
    #:matrix-accounts
    (list
     (matrix-account
      (id (getenv "MATRIX_USER"))
      (homeserver (string-append "matrix." (getenv "DOMAIN")))
      (local? #t))))
   (feature-emacs-ement)
   (feature-slack-settings
    #:slack-accounts
    (list
     (slack-account
      (workspace (getenv "SLACK_WORKSPACE"))
      (token (getenv "SLACK_TOKEN"))
      (cookie (getenv "SLACK_COOKIE")))))
   (feature-emacs-slack)
   (feature-emacs-telega)
   (feature-emacs-corfu #:corfu-doc? #t)
   (feature-emacs-comint)
   (feature-emacs-shell)
   (feature-emacs-eshell)
   (feature-compile)
   (feature-password-store
    #:remote-password-store-url "git@git.sr.ht:~conses/password-store")
   (feature-guile)
   (feature-emacs-smartparens)
   (feature-lisp
    #:extra-source-registry-entries
    `(("common-lisp/source-registry.conf.d/10-home.conf"
       ,(plain-file "10-home.conf" (format #f "(:tree \"~a/src\")" (getenv "HOME"))))))))
