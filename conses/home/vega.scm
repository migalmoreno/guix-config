(define-module (conses home vega)
  #:use-module (conses system)
  #:use-module (conses features android)
  #:use-module (conses features bluetooth)
  #:use-module (conses features bittorrent)
  #:use-module (conses features clojure)
  #:use-module (conses features documentation)
  #:use-module (conses features emacs)
  #:use-module (conses features emacs-xyz)
  #:use-module (conses features fontutils)
  #:use-module (conses features golang)
  #:use-module (conses features gtk)
  #:use-module (conses features irc)
  #:use-module (conses features keyboard)
  #:use-module (conses features tex)
  #:use-module (conses features lisp)
  #:use-module (conses features mail)
  #:use-module (conses features matrix)
  #:use-module (conses features messaging)
  #:use-module (conses features nyxt-xyz)
  #:use-module (conses features ocaml)
  #:use-module (conses features scheme)
  #:use-module (conses features security)
  #:use-module (conses features shellutils)
  #:use-module (conses features version-control)
  #:use-module (conses features video)
  #:use-module (conses features web-browsers)
  #:use-module (conses features wm)
  #:use-module (conses features web)
  #:use-module (conses features xorg)
  #:use-module (rde packages)
  #:use-module (rde features wm)
  #:use-module (rde features ssh)
  #:use-module (rde features xdg)
  #:use-module (rde features base)
  #:use-module (rde features gnupg)
  #:use-module ((rde features mail) #:select (feature-mail-settings
                                              feature-isync))
  #:use-module (rde features linux)
  #:use-module (rde features shells)
  #:use-module (rde features docker)
  #:use-module (rde features virtualization)
  #:use-module ((rde features emacs-xyz) #:select (feature-emacs-keycast
                                                   feature-emacs-eglot))
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu system keyboard)
  #:use-module (gnu packages emacs))

(define-public %home-features
  (list
   (feature-user-info
    #:user-name "vega"
    #:full-name (getenv "MAIL_PERSONAL_FULLNAME")
    #:email (getenv "MAIL_PERSONAL_EMAIL")
    #:user-groups '("wheel" "netdev" "audio" "video" "libvirt" "spice")
    #:rde-advanced-user? #t
    #:emacs-advanced-user? #t)
   (feature-gtk
    #:dark-theme? #t)
   (feature-android)
   (feature-emacs-fdroid)
   (feature-transmission)
   (feature-bluetooth)
   (feature-manpages)
   (feature-emacs
    #:default-application-launcher? #t
    #:emacs-server-mode? #f
    #:extra-init-el
    '((add-hook 'after-init-hook 'server-start)))
   (feature-gnupg
    #:gpg-primary-key (getenv "GPG_PUBLIC_KEY")
    #:ssh-keys '(("A23B61B2897F524D3D3410E1180423144F1DDB4E"))
    #:pinentry-flavor 'emacs
    #:default-ttl 34560000
    #:gpg-agent-extra-config
    '((no-greeting . #t)
      (allow-preset-passphrase . #t)))
   (feature-emacs-project)
   (feature-emacs-appearance
    #:auto-theme? #f)
   (feature-emacs-whitespace
    #:global-modes '(not org-mode org-agenda-mode
                         org-agenda-follow-mode org-capture-mode
                         dired-mode eshell-mode magit-status-mode
                         diary-mode magit-diff-mode text-mode
                         pass-view-mode erc-mode))
   (feature-emacs-all-the-icons)
   (feature-emacs-completion
    #:consult-initial-narrowing? #t)
   (feature-direnv)
   (feature-compile)
   (feature-password-store
    #:remote-password-store-url "git@git.sr.ht:~conses/password-store")
   (feature-clojure)
   (feature-nyxt
    #:default-browser? #t
    #:default-cookie-policy ':no-third-party
    #:auto-mode-rules
    '((((match-host "wikipedia.org") :included (nyxt/style-mode:dark-mode))))
    #:extra-config-lisp
    '((define-configuration prompt-buffer
        ((mouse-support-p nil))))
    #:extra-bindings
    '("C-c s" 'query-selection-in-search-engine))
   (feature-nyxt-emacs)
   (feature-nyxt-blocker)
   (feature-emacs-pdf-tools)
   (feature-fonts)
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
   (feature-emacs-modus-themes
    #:dark? #t)
   (feature-emacs-corfu
    #:corfu-doc? #t)
   (feature-emacs-vertico)
   (feature-emacs-tempel)
   (feature-emacs-files)
   (feature-emacs-ibuffer)
   (feature-emacs-emms)
   (feature-emacs-image)
   (feature-emacs-graphviz)
   (feature-emacs-ednc
    #:notifications-icon "")
   (feature-emacs-calendar
    #:week-numbers? #t)
   (feature-emacs-bookmark)
   (feature-emacs-dashboard)
   (feature-emacs-spelling
    #:spelling-program (@ (gnu packages aspell) aspell)
    #:spelling-dictionaries (strings->packages "aspell-dict-en")
    #:flyspell-hooks
    '(org-mode-hook bibtex-mode-hook)
    #:ispell-standard-dictionary "en_US")
   (feature-emacs-markdown)
   (feature-emacs-browse-url
    #:url-mappings
    `(("www.youtube.com" . ("^invidio.*" . "invidious.namazso.eu"))
      ("www.reddit.com" . ("^teddit.*" . "teddit.namazso.eu"))
      ("quora.com" . ("^quora.*" . "quora.vern.cc"))
      ("twitter.com" . ("^nitter.*" . "nitter.namazso.eu"))
      ("imgur.com" . ("^imgin.*" . "imgin.voidnet."))
      ("medium.com" . ("^scribe.*" . "scribe.rip"))))
   (feature-emacs-exwm
    #:window-configurations
    '(((string= exwm-class-name "Nyxt")
       char-mode t
       workspace 1
       simulation-keys nil
       (exwm-layout-hide-mode-line))
      ((string= exwm-instance-name "emacs") char-mode t)))
   (feature-emacs-exwm-run-on-tty
    #:emacs-exwm-tty-number 1
    #:launch-arguments '("-mm" "--debug-init"))
   (feature-emacs-window)
   (feature-emacs-pulseaudio-control
    #:emacs-pulseaudio-control ((@ (conses packages emacs-xyz) emacs-pulseaudio-control-next)))
   (feature-emacs-org
    #:org-capture-templates
    '(("t" "Tasks/Projects")
      ("tt" "TODO Entry" entry (file+headline "~/documents/tasks.org" "TODOs")
       "* TODO %? %^G\n%U\n"
       :empty-lines 1)
      ("tl" "Linked Entry" entry (file+headline "~/documents/tasks.org" "TODOs")
       "* TODO %? %a %^G\n%U\n"
       :immediate-finish 1)
      ("tb" "Read It Later" entry (file+headline "~/documents/tasks.org" "Read It Later")
       "* %a %^G"
       :immediate-finish 1)
      ("tv" "Watch It Later" entry (file+headline "~/documents/tasks.org" "Watch It Later")
       "* %a %^G"
       :immediate-finish 1)
      ("th" "Habit Entry" entry (file+headline "~/documents/tasks.org" "Habits")
       "* TODO %? %^G\nSCHEDULED:%^t\n:PROPERTIES:\n:STYLE: habit\n:END:"
       :empty-lines 1)
      ("tc" "Critical TODO Entry" entry (file+headline "~/documents/tasks.org" "TODOs")
       "* INTR %? %^G\n%U\n"
       :empty-lines 1)
      ("ts" "TODO Entry with Subtasks" entry (file+headline "~/documents/tasks.org" "TODOs")
       "* TODO %? [/]\n- [ ]"
       :empty-lines 1)
      ("td" "Scheduled Task" entry (file+headline "~/documents/tasks.org" "TODOs")
       "* TODO %?\nSCHEDULED: %^T"
       :empty-lines 1))
    #:org-priority-faces
    '((?A . (:foreground "#FF665C" :weight bold))
      (?B . (:foreground "#51AFEF"))
      (?C . (:foreground "#4CA171")))
    #:org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "INTR(i)" "|" "DONE(d!)")
      (sequence "GOING(g)" "|" "MISSED(m@)" "ATTENDED(a@)"))
    #:org-todo-keyword-faces
    '(("TODO" . "#ff665c")
      ("NEXT" . "#FCCE7B")
      ("PROG" . "#51afef")
      ("INTR" . "#a991f1")
      ("DONE" . "#7bc275"))
    #:org-tag-alist
    '((:startgroup)
      (:endgroup)
      ("work" . ?w)
      ("emacs" . ?e)
      ("project" . ?p)
      ("linux" . ?l)
      ("education" . ?d)
      ("finance" . ?f)
      ("guix" . ?g)
      ("chore" . ?c)))
   (feature-emacs-org-roam
    #:org-roam-directory "~/notes"
    #:org-roam-dailies-directory "journal/"
    #:org-roam-capture-templates
    `(("d" "default" plain "%?"
       :if-new (file+head
                "%<%Y%m%d%H%M%S>-${slug}.org"
                "#+title: ${title}\n#+filetags: :${Topic}:\n")
       :unnarrowed t)
      ("e" "Programming Concept" plain
       ,(string-append
         "#+begin_src %^{Language|elisp|lisp|scheme"
         "|clojure|ocaml|javascript}\n%?\n#+end_src\n")
       :if-new (file+head
                "%<%Y%m%d%H%M%S>-${slug}.org"
                ,(string-append
                  ":PROPERTIES:\n:DATA_TYPE: "
                  "%^{Data Type|Function|Method|Variable|Macro|Procedure}"
                  "\n:END:\n#+title: ${title}\n#+filetags: :${Topic}:"))
       :unnarrowed t)
      ("r" "Referenced Concept" plain "%?"
       :if-new (file+head
                "%<%Y%m%d%H%M%S>-${slug}.org"
                ,(string-append
                  ":PROPERTIES:\n:ROAM_REFS: %^{Reference}\n:END:\n"
                  "#+title: ${title}\n#+filetags: :${Topic}:"))
       :unnarrowed t)
      ("w" "Web Resource" plain "%?"
       :if-new (file+head
                "%<%Y%m%d%H%M%S>-${slug}.org"
                ,(string-append
                  ":PROPERTIES:\n:ROAM_REFS: %l\n:END:\n"
                  "#+title: ${title}\n#+filetags: :${Topic}:"))
       :unnarrowed t)
      ("r" "Recipe" plain "* Ingredients\n- %?\n* Directions"
       :if-new (file+head
                "%<%Y%m%d%H%M%S>-${title}.org"
                ,(string-append
                  ":PROPERTIES:\n:ROAM_REFS: %l\n"
                  ":MEAL_TYPE: %^{Meal Type|Lunch\|Breakfast|Appetizer|Dessert}"
                  "\n:END:\n#+title: ${title}\n#+filetags: :cooking:"))
       :unnarrowed t)
      ("b" "Book" plain
       "* Chapters\n%?"
       :if-new (file+head
                "%<%Y%M%d%H%M%S>-${slug}.org"
                ,(string-append
                  ":PROPERTIES:\n:AUTHOR: ${Author}\n:DATE: ${Date}\n"
                  ":PUBLISHER: ${Publisher}\n:EDITION: ${Edition}\n:END:\n"
                  "#+title: ${title}\n#+filetags: :${Topic}:"))
       :unnarrowed t))
    #:org-roam-dailies-capture-templates
    '(("d" "default" entry
       "* %<%I:%M %p>: %?"
       :if-new (file+head "%<%Y-%m-%d>.org"
                          "#+title: %<%Y-%m-d>\n"))))
   (feature-emacs-org-agenda
    #:org-agenda-files
    '("~/documents/tasks.org"))
   (feature-tex
    #:listings-options
    '(("basicstyle" "\\ttfamily")
      ("stringstyle" "\\color{blue}\\ttfamily")
      ("numbers" "left")
      ("numberstyle" "\\tiny")
      ("breaklines" "true")
      ("showstringspaces" "false")
      ("showtabs" "false")
      ("keywordstyle" "\\color{violet}")
      ("commentstyle" "\\color{gray}")
      ("label" "{Figure}"))
    #:extra-packages
    (strings->packages
     "texlive-wrapfig" "texlive-capt-of"
     "texlive-hyperref" "texlive-fonts-ec"
     "texlive-latex-geometry" "texlive-listings"
     "texlive-xcolor" "texlive-ulem" "texlive-latex-preview"
     "texlive-amsfonts" "texlive-grfext" "texlive-latex-natbib"))
   (feature-emacs-citar)
   (feature-emacs-info)
   (feature-emacs-which-key)
   (feature-emacs-helpful)
   (feature-emacs-keycast)
   (feature-emacs-eww)
   (feature-emacs-webpaste)
   (feature-emacs-time
    #:timezones
    '(("Europe/London" "London")
      ("Europe/Madrid" "Madrid")
      ("Europe/Moscow" "Moscow")
      ("America/New_York" "New York")))
   (feature-emacs-dired)
   (feature-emacs-calc)
   (feature-emacs-tramp)
   (feature-emacs-battery)
   (feature-emacs-display-wttr
    #:emacs-display-wttr (@ (conses packages emacs-xyz) emacs-display-wttr-local))
   (feature-emacs-tab-bar
    #:modules-left
    '((make-configure-tab-bar-module
       :id 'menu-bar
       :label (format " %s "
                      (all-the-icons-fileicon
                       "emacs" :v-adjust -0.1 :height 1))
       :help "Menu"
       :action 'tab-bar-menu-bar)
      (make-configure-tab-bar-module
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
       :label 'mpv-playing-time-string)
      (make-configure-tab-bar-module
       :id 'notifications
       :label '(:eval (configure-ednc--notify))))
    #:modules-center
    '((make-configure-tab-bar-module
       :id 'time
       :label 'display-time-string))
    #:modules-right
    '((make-configure-tab-bar-module
       :id 'org-timer
       :label 'org-timer-mode-line-string)
      (make-configure-tab-bar-module
       :id 'appointments
       :label 'appt-mode-string)
      (make-configure-tab-bar-module
       :id 'weather
       :label 'display-wttr-string)
      (make-configure-tab-bar-module
       :id 'volume-sink
       :label 'pulseaudio-control-display-volume-string)
      (make-configure-tab-bar-module
       :id 'battery
       :label 'battery-mode-line-string)))
   (feature-emacs-comint)
   (feature-emacs-shell)
   (feature-emacs-eshell)
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
      (network "oftc.net")
      (nick (getenv "IRC_OFTC_NICK")))))
   (feature-emacs-erc
    #:autojoin-channels-alist
    '((Libera.Chat
       "#nyxt" "#emacs" "#org-mode" "#guix" "#ocaml"
       "#clojure" "#commonlisp" "#scheme" "#tropin")
      (OFTC "#postmarketos" "#mobian")))
   (feature-slack-settings
    #:slack-accounts
    (list
     (slack-account
      (workspace (getenv "SLACK_WORKSPACE"))
      (token (getenv "SLACK_TOKEN"))
      (cookie (getenv "SLACK_COOKIE")))))
   (feature-emacs-slack)
   (feature-emacs-telega)
   (feature-emacs-eglot)
   (feature-emacs-smartparens
    #:paredit-bindings? #t
    #:extra-sp-lisp-modes '(sly-mode
                            lisp-data-mode
                            elisp-mode
                            minibuffer-inactive-mode
                            comint-mode))
   (feature-emacs-xref)
   (feature-emacs-re-builder)
   (feature-emacs-elisp)
   (feature-emacs-rainbow-delimiters)
   (feature-emacs-yaml)
   (feature-emacs-lang-web)
   (feature-emacs-polymode)
   (feature-go)
   (feature-qmk
    #:keyboard "dztech/dz65rgb/v1"
    #:keymap "custom")
   (feature-keyboard
    #:keyboard-layout %default-keyboard-layout
    #:default-input-method "spanish-keyboard")
   (feature-lisp
    #:extra-packages
    (strings->packages "sbcl-prove"))
   (feature-mail-settings
    #:mail-accounts
    (list
     (mail-acc 'personal (getenv "MAIL_PERSONAL_EMAIL") 'gandi))
    #:mail-directory-fn mail-directory-fn
    #:mailing-lists
    (list
     (mail-lst 'guix-devel "guix-devel@gnu.org"
               '("https://yhetil.org/guix-devel/0"))
     (mail-lst 'guix-devel "guix-bugs@gnu.org"
               '("https://yhetil.org/guix-bugs/0"))
     (mail-lst 'guix-devel "guix-patches@gnu.org"
               '("https://yhetil.org/guix-patches/1"))))
   (feature-isync)
   (feature-goimapnotify
    #:goimapnotify (@ (conses packages mail) go-gitlab.com-shackra-goimapnotify-next))
   (feature-emacs-ebdb)
   (feature-emacs-gnus
    #:topic-alist
    '(("personal"
       "nnmaildir+personal:inbox"
       "nnmaildir+personal:drafts"
       "nnmaildir+personal:sent"
       "nnmaildir+personal:spam"
       "nnmaildir+personal:trash")
      ("clojure"
       "nntp+gwene:gwene.clojure.planet"
       "nntp+gwene:gwene.com.google.groups.clojure")
      ("lisp"
       "nntp+gwene:gwene.org.lisp.planet"
       "nntp+gwene:gwene.engineer.atlas.nyxt"
       "nntp+gwene:gwene.org.wingolog")
      ("technology"
       "nntp+gwene:gwene.org.fsf.news"
       "nntp+gwene:gwene.rs.lobste"
       "nntp+gwene:gwene.org.hnrss.newest.points"
       "nntp+gwene:gwene.com.unixsheikh"
       "nntp+gwene:gwene.com.drewdevault.blog"
       "nntp+gwene:gwene.net.lwn.headlines.newrss"
       "nntp+gwene:gwene.com.usesthis"
       "nntp+gwene:gwene.org.sourcehut.blog"
       "nntp+gwene:gwene.cc.tante"
       "nntp+gwene:gwene.org.matrix.blog")
      ("emacs"
       "nntp+gwene:gmane.emacs.devel"
       "nntp+gwene:gmane.emacs.erc.general"
       "nntp+gwene:gwene.com.oremacs"
       "nntp+gwene:gwene.org.emacslife.planet"
       "nntp+gwene:gwene.group.discourse.org-roam.latest")
      ("guix"
       "nntp+gwene:gmane.comp.gnu.guix.bugs"
       "nntp+gwene:gmane.comp.gnu.guix.patches"
       "nntp+gwene:gwene.org.gnu.guix.feeds.blog")
      ("Gnus"))
    #:topic-topology
    '(("Gnus" visible)
      (("personal" visible nil))
      (("clojure" visible nil))
      (("lisp" visible nil))
      (("technology" visible nil))
      (("emacs" visible nil))
      (("guix" visible nil)))
    #:group-parameters
    '(("^nnmaildir"
       (display . 100)
       (gcc-self . "nnmaildir+personal:sent"))
      ("^nntp"
       (display . 1000)))
    #:posting-styles
    `(((header "to" ".*@lists.sr.ht")
       (name ,(getenv "USERNAME"))
       (cc ,(getenv "MAIL_PERSONAL_EMAIL"))
       (In-Reply-To make-message-in-reply-to)
       (signature ,(string-append "Best regards,\n"
                                  (getenv "USERNAME")))
       (To configure-gnus-get-article-participants))
      ("^nntp.+:"
       (name ,(getenv "USERNAME"))
       (cc ,(getenv "MAIL_PERSONAL_EMAIL"))
       (signature ,(string-append "Best regards,\n"
                                  (getenv "USERNAME")))
       (To configure-gnus-get-article-participants))))
   (feature-emacs-message)
   (feature-emacs-org-mime)
   (feature-emacs-smtpmail)
   (feature-desktop-services)
   (feature-matrix-settings
    #:homeserver (string-append "https://matrix." (getenv "DOMAIN"))
    #:matrix-accounts
    (list
     (matrix-account
      (id (getenv "MATRIX_USER"))
      (homeserver (string-append "matrix." (getenv "DOMAIN")))
      (local? #t))))
   (feature-pantalaimon)
   (feature-emacs-ement)
   (feature-nyxt-status
    #:height 30
    #:glyphs? #t
    #:status-buffer-layout
    '(:div :id "container"
      (:div :id "controls"
       (:raw
        (format-close-button status)))
      (:div :id "url"
       (:raw
        (format-status-load-status status)
        (format-status-url status)))
      (:div :id "modes"
            :title (nyxt::modes-string buffer)
       (:raw (format-status-modes status)))))
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
       (match-regex "https://whoogle.*"
                    "https://.*google.com/search.*")
       :original (quri:uri "https://www.google.com")
       :redirect (quri:uri "http://localhost:5000"))
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
      (engines:whoogle
       :shortcut "who"
       :base-search-url "http://localhost:5000/search?q=~a"
       :completion-function nil
       :theme :system
       :alternatives nil
       :lang-results :english
       :lang-ui :english
       :view-image t
       :no-javascript t
       :new-tab t)))
   (feature-ocaml)
   (feature-guile)
   (feature-guix
    #:authorized-directories
    '("~/src/projects/fdroid.el"
      "~/src/projects/nyxt.el"
      "~/src/projects/dotfiles"))
   (feature-ssh
    #:ssh-configuration
    (home-ssh-configuration
     (default-host "*")
     (default-options
      '((ControlPersist . "4h")
        (TCPKeepAlive . "no")
        (ServerAliveInterval . 30)
        (ServerAliveCountMax . 5)))
     (extra-config
      (list
       (ssh-host
        (host "cygnus")
        (options
         `((host-name . ,(getenv "CYGNUS_IP"))
           (user . "root"))))))))
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
   (feature-sourcehut)
   (feature-git
    #:primary-forge-account-id 'sh
    #:sign-commits? #t
    #:global-ignores '("**/.direnv"
                       "node_modules"
                       "*.elc"
                       ".log"))
   (feature-youtube-dl
    #:emacs-ytdl (@ (conses packages emacs-xyz) emacs-ytdl-next)
    #:music-dl-args '("-q" "-x" "--add-metadata" "--audio-format" "mp3")
    #:video-dl-args '("-q" "-f" "bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best"
                      "--add-metadata" "--compat-options" "all"))
   (feature-xdg
    #:xdg-user-directories-configuration
    (home-xdg-user-directories-configuration
     (desktop "$HOME")
     (documents "$HOME/documents")
     (download "$HOME/downloads")
     (music "$HOME/music")
     (pictures "$HOME/pictures")
     (publicshare "$HOME")
     (videos "$HOME/videos")
     (templates "$HOME")))
   (feature-pipewire)
   (feature-emacs-cursor)
   (feature-xorg)
   (feature-qemu)
   (feature-bash)))