(define-module (migalmoreno common)
  #:use-module (migalmoreno utils)
  #:use-module (contrib features javascript)
  #:use-module (farg source)
  #:use-module (farg theme)
  #:use-module (gnu services)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (rde features)
  #:use-module (rde features bittorrent)
  #:use-module (rde features bluetooth)
  #:use-module (rde features clojure)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features fontutils)
  #:use-module (rde features guile)
  #:use-module (rde features golang)
  #:use-module (rde features irc)
  #:use-module (rde features keyboard)
  #:use-module (rde features linux)
  #:use-module (rde features lisp)
  #:use-module (rde features mail)
  #:use-module (rde features markup)
  #:use-module (rde features matrix)
  #:use-module (rde features messaging)
  #:use-module (rde features networking)
  #:use-module (rde features nyxt-xyz)
  #:use-module (rde features ocaml)
  #:use-module (rde features password-utils)
  #:use-module (rde features shells)
  #:use-module (rde features shellutils)
  #:use-module (rde features terminals)
  #:use-module (rde features version-control)
  #:use-module (rde features video)
  #:use-module (rde features wm)
  #:use-module (rde features xdg)
  #:use-module (rde features xdisorg)
  #:use-module (rde home services video)
  #:use-module (rde home services web-browsers)
  #:use-module (rde packages)
  #:use-module (srfi srfi-1))


;;; Themes

(define-public %wallpaper
  (origin
    (method url-fetch)
    (uri
     "https://w.wallhaven.cc/full/28/wallhaven-28vjgm.jpg")
    (sha256
     (base32
      "14b5h86jjimdzfw9krbc90abcd9kgvfhavqqq7xzxjxjbakrkzdl"))))

(define-public %dark-theme
  (farg-source
   (theme
    (farg-theme
     (fg "#FFFFFF")
     (bg "#000000")
     (bg-alt "#212121")
     (accent-0 "#00BCFF")
     (accent-1 "#212121")
     (accent-2 "#F0F0F0")
     (alpha 0.8)
     (light? #f)
     (other '((red . "#ff5f59")))
     (wallpaper %wallpaper)))))

(define-public %light-theme
  (farg-source
   (theme
    (farg-theme
     (fg "#000000")
     (bg "#FFFFFF")
     (bg-alt "#212121")
     (accent-0 "#9fc6ff")
     (accent-1 "#212121")
     (accent-2 "#F0F0F0")
     (alpha 0.8)
     (light? #t)
     (other '((red . "#a60000")))
     (wallpaper %wallpaper)))))


;;; Base features

(define (mpv-run-with-emacs cmd)
  (format #f (string-append
              "run \"/bin/sh\" \"-c\" \"emacsclient -e "
              (if (string? cmd)
                  "~a\""
                  "'~s'\""))
          cmd))

(define extra-mpv-config
  `((global ((border . no)
             (volume . 100)
             ,(cons 'screenshot-directory
                    (string-append
                     (or (getenv "XDG_DATA_HOME") "~/.local/share")
                     "/mpv/screenshots"))
             (autofit . 800x800)
             (osd-border-size . 2)
             (osd-bar . yes)
             (osd-level . 0)
             (slang . en)
             (ytdl-raw-options . "ignore-config=,sub-lang=en,write-auto-sub=")
             (script-opts-add=osc-visibility . never)
             (script-opts-add=osc-windowcontrols . no)
             (save-position-on-quit . #t)))))

(define extra-mpv-bindings
  `("ctrl+a seek 0 absolute-percent"
    "ctrl+e seek 100 absolute-percent"
    "ctrl+f seek 5 relative"
    "ctrl+b seek -5 relative"
    "Shift+n add chapter 1"
    "Shift+p add chapter -1"
    "F cycle fullscreen"
    (format #f "D ~a" ,(mpv-run-with-emacs '(rde-mpv-download)))
    (format #f "Alt+c ~a" ,(mpv-run-with-emacs '(rde-mpv-capture)))
    "M cycle mute"
    "+ add volume 2"
    "- add volume -2"
    ": script-binding console/enable"
    "s screenshot video"
    "Q quit-watch-later"
    "O no-osd cycle-values osd-level 3 0"
    "o osd-bar show-progress"
    "v cycle sub-visibility"
    "b cycle sub"
    "n script-message osc-visibility always"
    "N script-message osc-visibility never"
    "L cycle-values loop-file \"inf\" \"no\""))

(define-public extra-mpv-settings-service
  (simple-service
   'add-mpv-extra-settings
   home-mpv-service-type
   (home-mpv-extension
    (mpv-conf extra-mpv-config)
    (input-conf extra-mpv-bindings))))

(define base-ytdl-args '("-q" "--add-metadata" "--compat-options" "all"))

(define-public %multimedia-base-features
  (list
   (feature-transmission
    #:download-dir (format #f "~a/videos" (getenv "HOME")))
   (feature-yt-dlp
    #:music-dl-args
    `("-x" "-f" "bestaudio" "--audio-format" "mp3" ,@base-ytdl-args)
    #:video-dl-args
    `("-f" "bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best"
      ,@base-ytdl-args))
   (feature-mpv)
   (feature-emacs-emms)))

(define extra-tempel-templates
  `(text-mode
    ,#~""
    (cut "--8<---------------cut here---------------start------------->8---"
         n r n
         "--8<---------------cut here---------------end--------------->8---"
         n)
    (asciibox "+-" (make-string (length str) ?-) "-+" n
              "| " (s str) " |" n
              "+-" (make-string (length str) ?-) "-+" n)))

(define-public %base-extra-init-el
  `((eval-when-compile
     (require 'cl-lib))
    (setq frame-title-format
          '(multiple-frames
            "%b"
            ("" "%b - GNU Emacs at " system-name
             " [" (:eval (frame-parameter (selected-frame) 'window-id)) "]")))
    (require 'warnings)
    (setq warning-suppress-types '((diary) (auto-save) (org-babel)))
    (setq warning-suppress-log-types '((comp org-babel)))
    (setq warning-minimum-level :error)
    (setq mode-line-misc-info
          (remove '(global-mode-string ("" global-mode-string))
                  mode-line-misc-info))
    (setq ring-bell-function 'ignore)
    (setq visible-bell nil)
    (fset 'yes-or-no-p 'y-or-n-p)
    (transient-mark-mode)
    (delete-selection-mode)
    (tooltip-mode -1)
    (with-eval-after-load 'comp
      (setq native-comp-async-report-warnings-errors nil))
    (global-so-long-mode)
    (column-number-mode 0)
    (with-eval-after-load 'autorevert
      (setq auto-revert-remote-files nil))
    (setq auto-save-no-message t)
    (setq create-lockfiles nil)
    (setq delete-old-versions t)
    (setq kept-new-versions 3)
    (setq kept-old-versions 2)
    (setq version-control t)
    (setq remote-file-name-inhibit-cache nil)))

(define-public %base-extra-early-init-el
  '((require 'xdg)
    (setq echo-keystrokes 0)
    (setq package-native-compile t)
    (setq package-user-dir
          (expand-file-name "emacs/elpa" (xdg-data-home)))
    (setq auto-save-list-file-prefix
          (expand-file-name "emacs/auto-save-list/.saves-"
                            (xdg-data-home)))))

(define-public %emacs-base-features
  (list
   (feature-emacs-completion)
   (feature-emacs-vertico)
   (feature-emacs-corfu #:corfu-doc-auto #t)
   (feature-emacs-all-the-icons)
   (feature-emacs-pdf-tools)
   (feature-emacs-tempel
    #:templates extra-tempel-templates)
   (feature-emacs-graphviz)
   (feature-emacs-calendar
    #:calendar-date-style 'european
    #:diary-file "~/documents/diary")
   (feature-emacs-spelling
    #:flyspell-hooks '(org-mode-hook message-mode-hook bibtex-mode-hook)
    #:ispell-standard-dictionary "en_US")
   (feature-emacs-info)
   (feature-emacs-which-key)
   (feature-emacs-help)
   (feature-emacs-time
    #:display-time? #t
    #:display-time-24hr? #t
    #:display-time-date? #t
    #:world-clock-time-format "%R %Z"
    #:world-clock-timezones
    '(("Europe/London" "London")
      ("Europe/Madrid" "Madrid")
      ("Europe/Moscow" "Moscow")
      ("America/New_York" "New York")
      ("Australia/Sydney" "Sydney")
      ("Asia/Tokyo" "Tokyo")))
   (feature-emacs-dashboard
    #:show-on-startup? #f
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
   (feature-emacs-dired
    #:kill-when-opening-new-buffer? #t
    #:group-directories-first? #t)
   (feature-emacs-calc)
   (feature-emacs-tramp)
   (feature-emacs-re-builder)
   (feature-emacs-ace-window)
   (feature-emacs-project)
   (feature-emacs-keycast)
   (feature-emacs-input-methods
    #:default-input-method "spanish-keyboard")
   (feature-emacs-browse-url)
   (feature-emacs-webpaste)))

(define-public %org-base-features
  (list
   (feature-emacs-org
    #:org-directory "~/documents"
    #:org-priority-faces
    '((?A . (:foreground "#FF665C" :weight bold))
      (?B . (:foreground "#51AFEF"))
      (?C . (:foreground "#4CA171")))
    #:org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d!)"))
    #:org-todo-keyword-faces
    '(("TODO" . "#ff665c")
      ("NEXT" . "#FCCE7B")
      ("HOLD" . "#a991f1")
      ("DONE" . "#7bc275"))
    #:org-tag-alist
    '((:startgroup)
      (:endgroup)
      ("work" . ?w)
      ("project" . ?p)
      ("education" . ?d)
      ("finance" . ?f)
      ("chore" . ?c)))
   (feature-emacs-org-recur)
   (feature-emacs-org-roam
    #:org-roam-directory "~/notes"
    #:org-roam-todo? #t
    #:org-roam-dailies-directory "./"
    #:org-roam-capture-templates
    `(("w" "work" plain "%?"
       :if-new (file+head "work/pages/%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+title: ${title}\n#+filetags: :${Topic}:\n")
       :unnarrowed t)
      ("p" "personal" plain "%?"
       :if-new (file+head "personal/%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+title: ${title}\n#+filetags: :${Topic}:\n")
       :unnarrowed t))
    #:org-roam-dailies-capture-templates
    '(("w" "work" entry
       "* %?"
       :if-new (file+head "work/journals/%<%Y-%m-%d>.org"
                          "#+title: %<%Y-%m-%d>\n"))
      ("p" "personal" entry
       "* %?"
       :if-new (file+head "personal/journals/%<%Y-%m-%d>.org"
                          "#+title: %<%Y-%m-%d>\n"))))
   (feature-emacs-org-agenda)))

(define-public %markup-base-features
  (list*
   (feature-emacs-monocle)
   %org-base-features
   (feature-markdown
    #:headings-scaling? #t)
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
    #:extra-tex-packages
    (strings->packages
     "texlive-wrapfig" "texlive-capt-of"
     "texlive-hyperref" "texlive-fonts-ec"
     "texlive-latex-geometry" "texlive-xcolor"
     "texlive-ulem" "texlive-latex-preview"
     "texlive-amsfonts" "texlive-grfext" "texlive-latex-natbib"
     "texlive-titling" "texlive-latex-titlesec" "texlive-enumitem"))
   (feature-emacs-citation
    #:global-bibliography (list "~/documents/references.bib"))))

(define-public %communication-base-features
  (list
   (feature-matrix-settings
    #:homeserver (string-append "https://pantalaimon.conses.eu")
    #:matrix-accounts
    (list
     (matrix-account
      (id "@sloan:conses.eu")
      (server "matrix.conses.eu"))))
   (feature-emacs-ement)
   (feature-irc-settings
    #:irc-accounts
    (list
     (irc-account
      (id 'srht)
      (network "chat.sr.ht")
      (bouncer? #t)
      (nick "mmoreno"))
     (irc-account
      (id 'libera)
      (network "irc.libera.chat")
      (nick "migalmoreno"))
     (irc-account
      (id 'oftc)
      (network "irc.oftc.net")
      (nick "migalmoreno"))))
   (feature-emacs-erc
    #:erc-auto-query 'bury
    #:erc-query-display 'buffer
    #:erc-join-buffer 'bury
    #:erc-images? #t
    #:erc-log? #f
    #:erc-autojoin-channels-alist
    '((Libera.Chat
       "#nyxt" "#emacs" "#org-mode" "#guix" "#nonguix" "#ocaml"
       "#clojure" "#commonlisp" "#guile" "#tropin" "#newpipe")
      (OFTC "#postmarketos")))
   (feature-slack-settings
    #:slack-accounts
    (list
     (slack-account
      (workspace "clojurians")
      (nick "vienzer")
      (cookie? #t))))
   (feature-emacs-slack)
   (feature-emacs-telega)))

(define-public %shell-base-features
  (list
   (feature-direnv)
   (feature-emacs-comint)
   (feature-emacs-shell)
   (feature-emacs-eshell)
   (feature-compile)
   (feature-bash)
   (feature-vterm)))

(define gnus-topic-alist
  '(("Personal"
     "nnmaildir+personal:inbox"
     "nnmaildir+personal:drafts"
     "nnmaildir+personal:sent"
     "nnmaildir+personal:spam"
     "nnmaildir+personal:trash")
    ("Clojure"
     "nntp+gwene:gwene.clojure.planet"
     "nntp+gwene:gwene.com.google.groups.clojure")
    ("Common Lisp"
     "nntp+gwene:gwene.org.lisp.planet"
     "nntp+gwene:gwene.engineer.atlas.nyxt")
    ("Scheme"
     "nntp+gwene:gwene.org.scheme.planet")
    ("Technology"
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
    ("Emacs"
     "nntp+gwene:gmane.emacs.devel"
     "nntp+gwene:gmane.emacs.erc.general"
     "nntp+gwene:gwene.com.oremacs"
     "nntp+gwene:gwene.org.emacslife.planet"
     "nntp+gwene:gwene.group.discourse.org-roam.latest")
    ("Guix"
     "nntp+gwene:gmane.comp.gnu.guix.bugs"
     "nntp+gwene:gmane.comp.gnu.guix.patches"
     "nntp+gwene:gwene.org.gnu.guix.feeds.blog")
    ("Inbox")
    ("Lisp")
    ("News")
    ("Gnus")))

(define gnus-topic-topology
  '(("Gnus" visible)
    (("Inbox" visible)
     (("Personal" visible nil)))
    (("News" visible)
     (("Common Lisp" visible nil))
     (("Clojure" visible nil))
     (("Scheme" visible nil))
     (("Emacs" visible nil))
     (("Guix" visible nil))
     (("Technology" visible nil)))))

(define gnus-group-parameters
  '(("^nnmaildir"
     (gcc-self . "nnmaildir+personal:sent"))
    ("^nntp"
     (display . 1000))))

(define gnus-posting-styles
  `((".*"
     (cc ,%default-email))
    ((header "cc" ".*@debbugs.gnu.org")
     (To rde-gnus-get-article-participants)
     (cc nil))
    ((header "to" ".*@lists.sr.ht")
     (To rde-gnus-get-article-participants)
     (cc ,%default-email))
    ("^nntp.+:"
     (To rde-gnus-get-article-participants)
     (cc ,%default-email))))

(define-public %mail-base-features
  (list
   (feature-mail-settings
    #:mail-accounts
    (list
     (mail-account
      (id 'personal)
      (type 'gandi)
      (fqda %default-email)
      (pass-cmd "pass show mail/personal")))
    #:mail-directory-fn
    (const (string-append (getenv "XDG_STATE_HOME") "/mail")))
   (feature-isync)
   (feature-goimapnotify
    #:notify? #t)
   (feature-emacs-gnus
    #:topic-alist gnus-topic-alist
    #:topic-topology gnus-topic-topology
    #:message-archive-method '(nnmaildir "personal")
    #:message-archive-group '((".*" "sent"))
    #:group-parameters gnus-group-parameters
    #:posting-styles gnus-posting-styles)
   (feature-emacs-message)
   (feature-emacs-org-mime
    #:html-element-styles
    '(("pre" . "color: #E6E1Dc; background-color: #232323; padding: 0.5em;")
      ("blockquote" . "border-left: 2px solid gray; padding-left: 4px;")))
   (feature-msmtp)
   (feature-emacs-debbugs)
   (feature-emacs-ebdb
    #:ebdb-sources (list "~/documents/contacts")
    #:ebdb-popup-size 0.2)))

(define-public %programming-base-features
  (list
   (feature-emacs-flymake)
   (feature-emacs-eglot)
   (feature-emacs-xref)
   (feature-emacs-smartparens
    #:paredit-bindings? #t
    #:smartparens-hooks
    '(prog-mode-hook
      lisp-data-mode-hook
      minibuffer-inactive-mode-hook
      comint-mode-hook
      cider-repl-mode-hook)
    #:smartparens-strict-hooks
    '(prog-mode-hook
      lisp-data-mode-hook
      comint-mode-hook
      cider-repl-mode-hook))
   (feature-emacs-elisp)
   (feature-guile)
   (feature-clojure)
   (feature-javascript
    #:node (@ (gnu packages node) node-lts))
   (feature-lisp
    #:extra-lisp-packages
    (strings->packages "sbcl-prove" "sbcl-cl-cffi-gtk" "sbcl-lisp-unit2")
    #:extra-source-registry-files
    (list
     (plain-file
      "10-projects.conf"
      (format #f "(:tree \"~a/src\")" (getenv "HOME")))))
   (feature-ocaml
    #:extra-init-ml
    (list
     (format #f "#directory \"~a/.guix-home/profile/lib/ocaml/site-lib/\""
             (getenv "HOME"))
     "#directory \"_build\""
     "#use \"topfind\""
     "#thread"
     "#require \"core.top\""
     "#require \"ppx_fields_conv\""
     "#require \"str\""
     "open Core"))
   (feature-go)))

(define-public %desktop-base-features
  (list
   (feature-networking)
   (feature-pipewire)
   (feature-emacs-pulseaudio-control)
   (feature-emacs-power-menu)
   (feature-emacs-ednc)
   (feature-swaylock
    #:swaylock (@ (gnu packages wm) swaylock-effects)
    #:extra-config
    '((screenshots)
      (clock)
      (indicator)
      (effect-blur . 7x5)
      (effect-vignette . "0.5:0.5")
      (hide-keyboard-layout)))
   (feature-swayidle)
   (feature-kanshi
    #:extra-config
    `((profile headless ((output eDP-1 enable)))
      (profile docked ((output eDP-1 disable)
                       (output DP-3 enable)))))
   (feature-bluetooth)
   (feature-keyboard
    #:keyboard-layout %default-keyboard-layout)
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
     (templates "$HOME")))))

(define-public %ui-base-features
  (list
   (feature-emacs-appearance)
   (feature-emacs-modus-themes
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
     (package (@ (gnu packages fonts) font-google-noto))))))

(define (extra-nx-router-routers config)
  `(,@(if (get-value 'google-frontend config)
          `((make-instance 'router:redirector
                           :name 'google
                           :route (match-regex ".*google.com/search.*")
                           :reverse (quri:uri "https://www.google.com")
                           :redirect
                           (quri:uri ,(get-value 'google-frontend config))
                           :instances-builder
                           router:whoogle-instances-builder))
          '())
    ,@(if (get-value 'youtube-frontend config)
          `((make-instance 'router:redirector
                           :route (match-domain "youtube.com")
                           :redirect
                           (list
                            (list
                             ,(format #f "~a/stream?url=\\&"
                                      (get-value 'youtube-frontend config))
                             ".*/watch\\?v.*"
                             ".*/shorts/.*")
                            (cons
                             ,(format #f "~a/playlist?list=\\&"
                                      (get-value 'youtube-frontend config))
                             ".*/playlist/.*")
                            (cons
                             ,(format #f "~a/channel?url=\\&"
                                      (get-value 'youtube-frontend config))
                             ".*/channel/.*")
                            (cons
                             ,(format #f "~a/search?q=\\1&serviceId=0"
                                      (get-value 'youtube-frontend config))
                             ".*/search\\?q=(.*)"))))
          '())
    ,@(if (get-value 'soundcloud-frontend config)
          `((make-instance 'router:redirector
                           :route (match-domain "soundcloud.com")
                           :redirect
                           (list
                            (cons
                             ,(format #f "~a/stream?url=\\&"
                                      (get-value 'soundcloud-frontend config))
                             ".*/.*/.*"))))
          '())
    ,@(if (get-value 'quora-frontend config)
          `((make-instance 'router:redirector
                           :route (match-domain "quora.com")
                           :reverse "www.quora.com"
                           :redirect
                           (quri:uri ,(get-value 'quora-frontend config))))
          '())
    ,@(if (get-value 'imgur-frontend config)
          `((make-instance 'router:redirector
                           :route (match-domain "imgur.com")
                           :reverse "imgur.com"
                           :redirect
                           (quri:uri ,(get-value 'imgur-frontend config))))
          '())
    ,@(if (get-value 'medium-frontend config)
          `((make-instance 'router:redirector
                           :route (match-domain "medium.com")
                           :reverse "www.medium.com"
                           :redirect
                           (quri:uri ,(get-value 'medium-frontend config))
                           :instances-builder
                           router:scribe-instances-builder))
          '())
    ,@(if (get-value 'twitter-frontend config)
          `((make-instance 'router:redirector
                           :route (match-domain "twitter.com")
                           :reverse "www.twitter.com"
                           :redirect
                           (quri:uri ,(get-value 'twitter-frontend config))))
          '())
    ,@(if (get-value 'reddit-frontend config)
          `((make-instance 'router:redirector
                           :route (match-domain "reddit.com")
                           :reverse "www.reddit.com"
                           :redirect
                           (quri:uri ,(get-value 'reddit-frontend config))
                           :instances-builder
                           router:teddit-instances-builder))
          '())
    ,@(if (get-value 'fandom-frontend config)
          `((make-instance 'router:redirector
                           :route (match-domain "fandom.com")
                           :redirect
                           (list
                            (cons
                             ,(format #f "~a/\\1/wiki/\\2"
                                      (get-value 'fandom-frontend config))
                             ".*://(\\w+)\\.fandom.com/wiki/(.*)"))
                           :instances-builder
                           router:breezewiki-instances-builder))
          '())

    (make-instance 'router:opener
                   :route (match-scheme "magnet" "mailto")
                   :resource "xdg-open ~a")))

(define (extra-nx-search-engines config)
  `((make-search-engine "toys" "https://toys.whereis.みんな/?search=~a")
    (make-search-engine "yt" "https://www.youtube.com/search?q=~a")
    (make-search-engine "sc" "https://soundcloud.com/search?q=~a")
    (engines:google
     :safe-search nil
     :lang-ui :english
     :results-number 50
     :new-window t)
    ,@(if (get-value 'google-frontend config)
          `((engines:whoogle
             :base-search-url ,(format #f "~a/search?q=~~a"
                                       (get-value 'google-frontend config))
             :new-tab t
             :lang-ui :english))
          '())))

(define-public %base-extra-nyxt-config-lisp
  `((in-package :nyxt-user)
    (nyxt:use-nyxt-package-nicknames)
    (local-time:reread-timezone-repository)
    (setf local-time:*default-timezone*
          (local-time:find-timezone-by-location-name ,%default-timezone))
    (asdf:load-system :nx-mosaic)

    (defun eval-in-emacs (&rest s-exps)
      "Evaluate S-EXPS with `emacsclient'."
      (let ((s-exps-string (str:replace-all
                            "nyxt::" "" (write-to-string
                                         `(progn
                                            (setq print-length nil)
                                            ,@s-exps)
                                         :case :downcase))))
        (format *error-output* "Sending to Emacs:~%~a~%" s-exps-string)
        (uiop:run-program
         (list "emacsclient" "-e" s-exps-string)
         :output '(:string :stripped t))))

    (define-command save-as-emacs-bookmark ()
      "Save current page in Nyxt as a bookmark record in Emacs."
      (eval-in-emacs
       `(let ((bookmark-make-record-function
                (lambda ()
                  (rde-browse-url-bookmark-make-record
                   ,(quri:render-uri (url (current-buffer)))
                   ,(title (current-buffer))))))
          (bookmark-set)))
      (echo "Bookmark stored"))

    (define-command org-roam-ref-capture ()
      "Reference an org-roam node with the current page."
      (eval-in-emacs
       `(rde-org-roam-ref-add ,(render-url (url (current-buffer)))
                              (org-roam-node-read))))

    (defun play-emacs-mpv (url &rest extra-args &key &allow-other-keys)
      "Play stream from URL with EXTRA-ARGS in an Emacs mpv process."
      (let* ((nyxt::*interactive-p* t)
             (url (render-url (quri:uri url)))
             (playlist (null (member (eval-in-emacs
                                      '(progn
                                        (require 'mpv)
                                        (mpv-get-property "playlist")))
                                     '("nil" "[]") :test 'string=)))
             (play-or-enqueue
               (when playlist
                 (nyxt:prompt1
                  :prompt "Select"
                  :sources (make-instance
                            'prompter:yes-no-source
                            :constructor '("Play" "Enqueue")))))
             (formats (delete-duplicates
                       (remove-if-not
                        (lambda (f)
                          (ppcre:scan "\\d+x\\d+" f))
                        (mapcar (lambda (f) (getf f :resolution))
                                (with-input-from-string
                                    (s (eval-in-emacs
                                        `(progn
                                           (require 'ytdl)
                                           (ytdl--list-formats ,url))))
                                  (read s))))
                       :test 'equal))
             (format (when formats
                       (ppcre:register-groups-bind
                           (height)
                           ("\\d+x(\\d+)"
                            (nyxt:prompt1
                             :prompt "Format"
                             :sources (make-instance
                                       'prompter:source
                                       :name "Formats"
                                       :constructor formats)))
                         (format nil "best[height<=~a]" height))))
             (res (and play-or-enqueue
                       (string= play-or-enqueue "Enqueue"))))
        (eval-in-emacs
         `(apply 'rde-mpv-play-url ,url ,format
                 :select nil :playlist ,res ',extra-args))))

    (define-command play-emacs-mpv-current (&optional
                                            (buffer (current-buffer)))
      "Play contents of BUFFER in an Emacs-controlled mpv process."
      (play-emacs-mpv (render-url (url buffer))))

    (define-command-global magit-clone ()
      "Clone and open the current repository with Magit."
      (let ((path (sera:path-join
                   (user-homedir-pathname)
                   "src/"
                   (car (last
                         (str:split
                          "/" (quri:uri-path
                               (url (current-buffer)))))))))
        (if (uiop:directory-exists-p path)
            (echo "Error: Directory ~a already exists."
                  (namestring path))
            (eval-in-emacs
             '(require 'magit)
             `(magit-clone-internal
               ,(quri:render-uri (url (current-buffer)))
               ,(namestring path)
               nil)))))

    (define-configuration web-buffer
      ((default-modes
         (append '(nyxt/mode/reduce-tracking:reduce-tracking-mode)
                 %slot-value%))))

    (define-configuration (web-buffer
                           prompt-buffer
                           nyxt/mode/editor:editor-buffer)
      ((default-modes `(nyxt/mode/emacs:emacs-mode ,@%slot-value%))))))

(define-public (extra-nx-tailor-styles config)
  `((define-configuration status-buffer
      ((style
        (tailor:with-style 'status-buffer
          `("#container"
            :height "100%"
            :width "100%"
            :background ,theme:primary
            :font-family ,theme:font-family
            :align-items "center"
            :padding "0 5px"
            :box-sizing "border-box")
          `("#controls"
            :background "inherit"
            :box-sizing "border-box")
          `("#controls button"
            :color ,theme:on-background
            :padding "3px")
          `("#controls button:hover"
            :background none
            :color ,theme:on-background)
          `(".arrow-right, .arrow-left"
            :clip-path "none"
            :margin-right 0)
          `("#url"
            :background "inherit"
            :color ,theme:on-background
            :font-weight "bold"
            :padding "5px"
            :display "flex"
            :align-items "center"
            :box-sizing "border-box"
            :flex-grow "6"
            :flex-shrink "3")
          `("#url button"
            :white-space "nowrap"
            :text-overflow "ellipsis"
            :overflow "hidden")
          `("#modes"
            :background "inherit"
            :padding-left "5px"
            :flex-grow "1")
          `(:media "(max-width: 360px)"
                   ("#container"
                    :padding "0 1px")
                   ("#url"
                    :flex-basis "5em"
                    :flex-grow "2"
                    :flex-shrink "2"
                    :height "80%"
                    :border "1px solid #505050"
                    :border-radius "3px"
                    :overflow hidden)
                   ("#controls"
                    :width "fit-content"
                    :flex-basis "auto"
                    :padding "10% 5px")
                   ("#controls button"
                    :height "80%"
                    :width "30px"
                    :padding 0
                    :border-color "#505050"
                    :border-width "1px")
                   ("#controls button:not(:first-of-type):not(:last-of-type)"
                    :border-width "1px 1px 1px 0px"
                    :border-style "solid"
                    :border-color "#505050"
                    :border-radius 0)
                   ("#controls button:first-of-type"
                    :border-width "1px"
                    :border-style "solid"
                    :border-radius "3px 0px 0px 3px")
                   ("#controls button:last-of-type"
                    :border-width "1px 1px 1px 0px"
                    :border-style "solid"
                    :border-radius "0px 3px 3px 0px"))))))

          (define-configuration window
            ((message-buffer-style
              (tailor:with-style 'window
                `(body
                  :color ,theme:on-background
                  :background ,theme:background
                  :font-family ,theme:font-family
                  :overflow-x "hidden")))))

          (define-configuration web-buffer
            ((style
              (tailor:with-style 'web-buffer
                `(:let ((sans ,,(font-name (get-value 'font-sans config))))
                   ("*, body, div, section, input"
                    :font-family ,theme:font-family
                    :background-color ,theme:background
                    :color ,theme:on-background)
                   ("h1,h2,h3,h4,h5,h6"
                    :color ,theme:on-secondary
                    :font-family #(sans))
                   ("p, td , dt, button, .button, a, a:link"
                    :font-family #(sans)
                    :color ,theme:on-background)
                   ("button, .button"
                    :padding "10px")
                   (pre
                    :font-family ,theme:font-family
                    :color ,theme:on-background
                    :background ,theme:background)
                   (code
                    :font-family ,theme:font-family
                    :color ,theme:on-background
                    :background ,(if (theme:dark-p theme:theme)
                                     theme:secondary
                                     theme:primary))
                   (:media "(max-width: 768px)"
                           (body
                            :font-size "12px")))))))

          (define-configuration prompt-buffer
            ((style
              (tailor:with-style 'prompt-buffer
                `(* :font-family ,theme:font-family)
                `("#prompt-area"
                  :background ,theme:background
                  :color ,theme:on-secondary)
                `("#prompt"
                  :padding-left "15px")
                `("#prompt-modes"
                  :padding-right "10px")
                `("#input"
                  :background ,theme:background)
                `("#input:focus"
                  :box-shadow none)
                `(".source"
                  :margin 0)
                `(".source-name"
                  :background ,theme:background
                  :color ,theme:on-primary
                  :font-style "italic"
                  :padding "5px 15px")
                `(".source-content"
                  :border-collapse "collapse"
                  :margin-left 0)
                `(".source-content td"
                  :padding "5px 15px"
                  :text-overflow "ellipsis")
                `(".source-content th"
                  :padding "5px 15px"
                  :background ,theme:background
                  :font-weight "bold")
                `(".arrow-right, .arrow-left"
                  :clip-path "none"
                  :margin-right 0)
                `((:or "#prompt-modes" "#close-button")
                  :display "none")
                `("#selection"
                  :background ,(str:concat theme:primary "E6")
                  :color ,theme:on-background)
                `(.marked
                  :background ,theme:accent
                  :color ,theme:on-accent)))))))

(define-public %nyxt-base-features
  (list
   (feature-nyxt-blocker)
   (feature-nyxt-nx-search-engines
    #:default-engine-shortcut "go"
    #:extra-engines extra-nx-search-engines)
   (feature-nyxt-nx-router
    #:routers extra-nx-router-routers)
   (feature-nyxt-nx-tailor #:auto? #f)
   (feature
    (name 'nyxt-nx-tailor-extra-styles)
    (home-services-getter
     (lambda (config)
       (list
        (simple-service
         'rde-nx-tailor-extension
         (get-value 'nyxt-rde-nx-tailor-service-type config)
         (home-nyxt-lisp-extension
          (config (extra-nx-tailor-styles config))))))))
   (feature-nyxt-appearance)))

(define-public %security-base-features
  (list
   (feature-password-store
    #:remote-password-store-url
    (format #f "git@git.~a:password-store" %default-domain))))

(define-public %forge-base-features
  (list
   (feature-emacs-git)
   (feature-git
    #:extra-config
    `((sendemail
       ((cc . ,%default-email)
        (thread . #t)))
      (github
       ((user . ,%default-username)))
      (gitlab
       ((user . ,%default-username)))))))
