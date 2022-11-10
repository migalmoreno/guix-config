(define-module (conses home hydri)
  #:use-module (conses utils)
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

(define-public %hydri-signing-key
  (project-file "conses/keys/hydri.pub"))

(define-public %home-features
  (list
   (feature-user-info
    #:user-name "user"
    #:full-name (getenv "MAIL_PERSONAL_FULLNAME")
    #:email (getenv "MAIL_PERSONAL_EMAIL")
    #:user-groups '("wheel" "netdev" "audio" "video")
    #:rde-advanced-user? #t
    #:emacs-advanced-user? #t)
   (feature-base-packages
    #:home-packages
    (strings->packages
     "make" "nss-certs"))
   (feature-fonts)
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
        ("LESSHISTFILE" . "-")))))
   (feature-nyxt
    #:default-browser? #t)
   (feature-nyxt-emacs)
   (feature-youtube-dl
    #:emacs-ytdl (@ (conses packages emacs-xyz) emacs-ytdl-next)
    #:music-dl-args '("-q" "-x" "--add-metadata" "--audio-format" "mp3")
    #:video-dl-args '("-q" "-f" "bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best"
                      "--add-metadata" "--compat-options" "all"))
   (feature-mpv
    #:emacs-mpv ((@ (conses packages emacs-xyz) emacs-mpv-next))
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
   (feature-emacs-emms)
   (feature-nyxt-status
    #:status-buffer-layout
    #:height 30
    '(:div :id "container"
      (:div :id "controls"
       (:raw
        (format-close-button status)))))
   (feature-nyxt-userscript
    #:userstyles
    '((make-instance
       'nyxt/user-script-mode:user-style
       :include '("https://github.com/*"
                  "https://gist.github.com/*")
       :code (cl-css:css
              `((,(str:join "," '("#dashboard .body"
                                  ".js-inline-dashboard-render"
                                  ".js-feed-item-component"
                                  ".js-yearly-contributions"
                                  ".js-profile-editable-area div .mb-3"
                                  ".starring-container"
                                  "#js-contribution-activity"
                                  "#year-list-container"
                                  "a[href$=watchers]"
                                  "a[href$=stargazers]"
                                  "a[href$=followers]"
                                  "a[href$=following]"
                                  "a[href$=achievements]"
                                  "[action*=follow]"))
                 :display "none !important")
                ("img[class*=avatar]"
                 :visibility "hidden"))))))
   (feature-nyxt-nx-tailor
    #:auto-theme? #f
    #:dark-theme? #t
    #:themes
    '((tailor:make-theme
       "modus-operandi"
       :background-color "white"
       :on-background-color "black"
       :primary-color "#093060"
       :secondary-color "#dfdfdf"
       :on-secondary-color "black"
       :accent-color "#8f0075"
       :on-accent-color "#005a5f"
       :font-family "Iosevka"
       :cut (make-instance 'tailor:cut))
      (tailor:make-theme
       "modus-vivendi"
       :dark-p t
       :background-color "black"
       :on-background-color "white"
       :primary-color "#c6eaff"
       :secondary-color "#323232"
       :on-secondary-color "#a8a8a8"
       :accent-color "#afafef"
       :on-accent-color "#a8a8a8"
       :font-family "Iosevka"
       :cut (make-instance 'tailor:cut))))
   (feature-nyxt-nx-router
    #:media-enabled? #t
    #:routes
    '((router:make-route
       (match-regex "https://(www.)?insta.*")
       :original "www.instagram.com"
       :redirect (make-instance
                  'router:redirect
                  :to "www.picuki.com"
                  :rules '(("/profile/" . (not "/" "/p/" "/tv/" "/reels/"))
                           ("/media/" . "/p/"))))
      (router:make-route
       (match-domain "tiktok.com")
       :original "www.tiktok.com"
       :redirect (make-instance
                  'router:redirect
                  :to "tok.artemislena.eu"
                  :rules '(("/@placeholder/video/" . (not "/" "/@"))))
       :blocklist '(:path (:contains (not "video"))))
      (router:make-route
       (match-domain "reddit.com")
       :original "www.reddit.com"
       :redirect "teddit.namazso.eu"
       :instances 'make-teddit-instances
       :blocklist (make-instance
                   'router:blocklist
                   :rules '(:contains (not "/comments/" "/wiki/"))))
      (router:make-route
       (match-regex ".*/watch\\?.*v=.*")
       :redirect "www.youtube.com"
       :external (lambda (data)
                   (eval-in-emacs
                    `(configure-mpv-play-url
                      ,(quri:render-uri (url data))
                      :audio-only t :repeat t))))
      (router:make-route
       (match-regex "https://gfycat.com/.*"
                    "https://streamable.com/.*"
                    "https://.*/videos/watch/.*"
                    ".*cloudfront.*master.m3u8")
       :external (lambda (data)
                   (eval-in-emacs
                    `(configure-mpv-play-url
                      ,(quri:render-uri (url data))))))
      (router:make-route
       (match-scheme "magnet")
       :external (lambda (data)
                   (eval-in-emacs
                    `(transmission-add ,(quri:render-uri (url data))))))
      (router:make-route
       (match-domain "youtube.com" "youtu.be")
       :original "www.youtube.com"
       :redirect "invidio.xamh.de"
       :instances 'make-invidious-instances
       :blocklist '(:path (:starts "/c/")))
      (router:make-route
       (match-domain "medium.com")
       :original "www.medium.com"
       :redirect "scribe.rip"
       :instances 'make-scribe-instances)
      (router:make-route
       (match-domain "imgur.com")
       :original "imgur.com"
       :redirect "imgin.voidnet.tech")
      (router:make-route
       (match-domain "quora.com")
       :original "www.quora.com"
       :redirect "quora.vern.cc")
      (router:make-route
       (match-domain "lemmy.ml")
       :blocklist '(:path (:starts ("/u/" "/c"))))
      (router:make-route
       (match-domain "twitter.com")
       :original "www.twitter.com"
       :redirect (make-instance
                  'router:redirect
                  :to "farside.link"
                  :rules '(("/nitter/" . "/"))))))
   (feature-nyxt-nx-search-engines
    #:engines
    '((engines:wordnet
       :shortcut "wn"
       :show-examples t
       :show-word-frequencies t
       :show-sense-numbers t)
      (engines:github
       :shortcut "gh"
       :object :advanced)
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
      (engines:google
       :shortcut "go"
       :completion-function nil
       :safe-search nil
       :results-number 50
       :new-window t)
      (engines:peertube
       :shortcut "pt")
      (engines:invidious
       :shortcut "yt"
       :completion-function nil)
      (engines:lemmy
       :shortcut "le")
      (engines:discourse
       :shortcut "ae")
      (engines:discourse
       :shortcut "cv"
       :fallback-url (quri:uri "https://clojureverse.org")
       :base-search-url "https://clojureverse.org/search?q=~a")
      (engines:discourse
       :shortcut "oc"
       :fallback-url (quri:uri "https://discuss.ocaml.org")
       :base-search-url "https://discuss.ocaml.org/search?q=~a")
      (engines:discourse
       :shortcut "or"
       :fallback-url (quri:uri "https://org-roam.discourse.group")
       :base-search-url "https://org-roam.discourse.group/search?q=~a")
      (engines:discourse
       :shortcut "pc"
       :fallback-url (quri:uri "https://community.penpot.app/latest")
       :base-search-url "https://community.penpot.app/search?q=~a")
      (engines:meetup
       :shortcut "me")
      (engines:gitea
       :shortcut "gi")
      (engines:gitea-users
       :shortcut "giu")
      (engines:teddit
       :shortcut "re"
       :fallback-url (quri:uri "https://teddit.namazso.eu")
       :base-search-url "https://teddit.namazso.eu/search?q=~a")
      (engines:hacker-news
       :shortcut "hn"
       :fallback-url (quri:uri "https://news.ycombinator.com")
       :search-type :all)
      (engines:lobsters
       :shortcut "lo")
      (make-instance
       'search-engine
       :shortcut "clj"
       :search-url "https://clojars.org/search?q=~a"
       :fallback-url "https://clojars.org")
      (make-instance
       'search-engine
       :shortcut "to"
       :search-url "https://torrents-csv.ml/#/search/torrent/~a/1"
       :fallback-url "https://torrents-csv.ml")
      (engines:google
       :shortcut "go"
       :completion-function nil
       :lang-ui :english
       :new-window t)))
   (feature-emacs-files)
   ;; (feature-desktop-services)
   (feature-emacs-cursor)
   (feature-direnv)
   (feature-emacs-dired)
   (feature-emacs-browse-url)
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
