(define-module (dotfiles common)
  #:use-module (dotfiles utils)
  #:use-module (contrib features javascript)
  #:use-module (contrib features wm)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (rde features)
  #:use-module (rde features bittorrent)
  #:use-module (rde features bluetooth)
  #:use-module (rde features clojure)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features fontutils)
  #:use-module (rde features golang)
  #:use-module (rde features irc)
  #:use-module (rde features keyboard)
  #:use-module (rde features lisp)
  #:use-module (rde features mail)
  #:use-module (rde features markup)
  #:use-module (rde features matrix)
  #:use-module (rde features messaging)
  #:use-module (rde features nyxt-xyz)
  #:use-module (rde features ocaml)
  #:use-module (rde features password-utils)
  #:use-module (rde features shells)
  #:use-module (rde features shellutils)
  #:use-module (rde features terminals)
  #:use-module (rde features version-control)
  #:use-module (rde features video)
  #:use-module (rde features xdg)
  #:use-module (rde home services video)
  #:use-module (rde home services web-browsers)
  #:use-module (rde packages)
  #:use-module (srfi srfi-1))


;;; Base features

(define (mpv-run-with-emacs cmd)
  (format #f (string-append
              "run \"/bin/sh\" \"-c\" \"emacsclient -e "
              (if (string? cmd)
                  "~a\""
                  "'~s'\""))
          cmd))

(define mpv-extra-config
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

(define mpv-extra-bindings
  `((global (("ctrl+a" . "seek 0 absolute-percent")
             ("ctrl+e" . "seek 100 absolute-percent")
             ("ctrl+f" . "seek 5 relative")
             ("ctrl+b" . "seek -5 relative")
             ("Shift+n" . "add chapter 1")
             ("Shift+p" . "add chapter -1")
             ("F" . "cycle fullscreen")
             ("D" . ,(mpv-run-with-emacs '(rde-mpv-download)))
             ("Alt+c" . ,(mpv-run-with-emacs '(rde-mpv-capture)))
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
             ("L" . "cycle-values loop-file \"inf\" \"no\"")))))

(define-public extra-mpv-settings-service
  (simple-service
   'add-mpv-extra-settings
   home-mpv-service-type
   (home-mpv-extension
    (mpv-conf mpv-extra-config)
    (input-conf mpv-extra-bindings))))

(define base-ytdl-args '("-q" "--add-metadata" "--compat-options" "all"))

(define-public %multimedia-base-features
  (list
   (feature-transmission
    #:download-dir (format #f "~a/videos" (getenv "HOME")))
   (feature-youtube-dl
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
    (add-hook 'after-init-hook 'server-start)
    (setq-default frame-title-format '("%b - Emacs"))
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
      ("emacs" . ?e)
      ("project" . ?p)
      ("linux" . ?l)
      ("education" . ?d)
      ("finance" . ?f)
      ("guix" . ?g)
      ("chore" . ?c)))
   (feature-emacs-org-recur)
   (feature-emacs-org-roam
    #:org-roam-directory "~/documents/notes"
    #:org-roam-dailies-directory "journal/"
    #:org-roam-todo? #t
    #:org-roam-capture-templates
    `(("d" "default" plain "%?"
       :if-new (file+head
                "%<%Y%m%d%H%M%S>-${slug}.org"
                "#+title: ${title}\n#+filetags: :${Topic}:\n")
       :unnarrowed t)
      ("r" "reference" plain "%?"
       :if-new (file+head
                "%<%Y%m%d%H%M%S>-${slug}.org"
                ,(string-append
                  ":PROPERTIES:\n:ROAM_REFS: ${ref}\n:END:\n"
                  "#+title: ${title}\n#+filetags: :${Topic}:"))
       :unnarrowed t)
      ("m" "recipe" plain "* Ingredients\n- %?\n* Directions"
       :if-new (file+head
                "%<%Y%m%d%H%M%S>-${title}.org"
                "#+title: ${title}\n#+filetags: :cooking:\n")
       :unnarrowed t)
      ("b" "book" plain
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
       "* %?"
       :if-new (file+head "%<%Y-%m-%d>.org"
                          "#+title: %<%Y-%m-d>\n"))))
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
   (feature-emacs-citar
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
      (nick "ardon"))
     (irc-account
      (id 'oftc)
      (network "irc.oftc.net")
      (nick "nvsop"))))
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

(define* (mail-acc id user type #:optional pass-cmd)
  "Make a simple mail account."
  (mail-account
   (id id)
   (fqda user)
   (type type)
   (pass-cmd (format #f "pass show mail/~a | head -1" id))))

(define-public %mail-base-features
  (list
   (feature-mail-settings
    #:mail-accounts
    (list
     (mail-acc 'personal %default-email 'gandi))
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
   (feature-emacs-guix)
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
   (feature-emacs-geiser)
   (feature-go)))

(define-public %emacs-desktop-base-features
  (list
   (feature-emacs-ednc
    #:notifications-icon "")
   (feature-emacs-pulseaudio-control
    #:display-volume? #t)
   (feature-emacs-display-wttr)
   (feature-emacs-battery)
   (feature-emacs-tab-bar
    #:modules-left
    `((make-rde-tab-bar-module
       :id 'menu-bar
       :label (format " %s "
                      (all-the-icons-fileicon
                       "emacs" :v-adjust -0.1 :height 1))
       :help "Menu"
       :action 'tab-bar-menu-bar)
      (make-rde-tab-bar-module
       :id 'notifications
       :label '(:eval (exwm-modeline-segment)))
      (make-rde-tab-bar-module
       :id 'notifications
       :label '(:eval (rde-ednc--notify))))
    #:modules-center
    '((make-rde-tab-bar-module
       :id 'time
       :label 'display-time-string))
    #:modules-right
    '((make-rde-tab-bar-module
       :id 'org-timer
       :label 'org-timer-mode-line-string)
      (make-rde-tab-bar-module
       :id 'appointments
       :label 'appt-mode-string)
      (make-rde-tab-bar-module
       :id 'weather
       :label 'display-wttr-string)
      (make-rde-tab-bar-module
       :id 'volume-sink
       :label 'pulseaudio-control-display-volume-string)
      (make-rde-tab-bar-module
       :id 'battery
       :label 'battery-mode-line-string)))
   (feature-emacs-exwm
    #:workspace-number 4
    #:window-configurations
    '(((string= exwm-class-name "Nyxt")
       char-mode t
       workspace 1
       simulation-keys nil
       (exwm-layout-hide-mode-line))
      ((string= exwm-instance-name "emacs")
       char-mode t)
      ((string-match "Android Emulator" exwm-title)
       floating t))
    #:extra-exwm-bindings
    `((cons (kbd "s-<next>") 'pulseaudio-control-decrease-sink-volume)
      (cons (kbd "s-<prior>") 'pulseaudio-control-increase-sink-volume)
      (cons (kbd "s-l") '(lambda ()
                           (interactive)
                           (call-process "slock")))
      (cons (kbd "M-o") 'ace-window)
      (cons (kbd "s-<SPC>") 'app-launcher-run-app))
    #:extra-exwm-init
    `((call-process
       ,(file-append (@ (gnu packages xorg) xsetroot)
                     "/bin/xsetroot")
       nil nil nil "-cursor_name" "left_ptr")
      (if (listp (rde-exwm--get-outputs))
          (fontaine-set-preset 'docked)
        (fontaine-set-preset 'headless))
      (with-eval-after-load 'pinentry-autoloads
        (pinentry-start))))
   (feature-emacs-exwm-run-on-tty
    #:emacs-exwm-tty-number 1
    #:launch-arguments '("-mm" "--debug-init")
    #:extra-xorg-config
    (list
     "Section \"Monitor\"
  Identifier \"DP-3\"
  Option \"DPMS\" \"false\"
EndSection
Section \"ServerFlags\"
  Option \"BlankTime\" \"0\"
EndSection"))))

(define-public %desktop-base-features
  (list
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
    #:dark? #t
    #:deuteranopia? #f
    #:headings-scaling? #t
    #:extra-modus-themes-overrides
    '((bg-mode-line-active bg-dim)))
   (feature-emacs-circadian)
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
     (package (@ (gnu packages fonts) font-google-noto)))
    #:extra-fontaine-presets
    '((docked
       :default-height 107
       :variable-pitch-weight light)
      (headless
       :default-height 105
       :variable-pitch-weight light)))))

(define (nx-router-routers config)
  `((make-instance 'router:opener
                   :name 'youtube-videos
                   :route (match-regex
                           ".*/watch\\?.*v=.*"
                           ".*/playlist\\?list=.*")
                   :resource
                   (lambda (url)
                     (play-emacs-mpv url :formats nil :audio t :repeat t)))
    (make-instance 'router:redirector
                   :name 'youtube-videos
                   :redirect-url (quri:uri "https://www.youtube.com"))
    (make-instance 'router:opener
                   :route (match-regex "^https://(m.)?soundcloud.com/.*/.*")
                   :resource (lambda (url)
                               (play-emacs-mpv
                                url :formats nil :audio t :repeat t)))
    (make-instance 'router:opener
                   :route (match-regex "https://gfycat.com/.*"
                                       "https://streamable.com/.*"
                                       "https://.*/videos/watch/.*"
                                       ".*cloudfront.*master.m3u8")
                   :resource (lambda (url)
                               (play-emacs-mpv url :formats nil)))
    (make-instance 'router:opener
                   :route (match-scheme "mailto")
                   :resource "xdg-open ~s &")
    (make-instance 'router:opener
                   :route (match-scheme "magnet" "torrent")
                   :resource (lambda (url)
                               (eval-in-emacs `(transmission-add ,url))))
    ,@(if (get-value 'google-frontend config)
          `((make-instance 'router:redirector
                           :name 'google
                           :route
                           (match-regex ".*google.com/search.*")
                           :original-url
                           (quri:uri "https://www.google.com")
                           :redirect-url
                           (quri:uri
                            ,(get-value 'google-frontend config))
                           :instances-builder
                           router:whoogle-instances-builder))
          '())
    ,@(if (get-value 'youtube-frontend config)
          `((make-instance 'router:redirector
                           :name 'youtube
                           :route
                           (match-domain "youtube.com" "youtu.be")
                           :original-url "www.youtube.com"
                           :redirect-url
                           (quri:uri
                            ,(get-value 'youtube-frontend config))
                           :instances-builder
                           router:invidious-instances-builder))
          '())
    ,@(if (get-value 'quora-frontend config)
          `((make-instance 'router:redirector
                           :name 'quora
                           :route (match-domain "quora.com")
                           :original-url "www.quora.com"
                           :redirect-url
                           (quri:uri
                            ,(get-value 'quora-frontend config))))
          '())
    ,@(if (get-value 'imgur-frontend config)
          `((make-instance 'router:redirector
                           :name 'imgur
                           :route (match-domain "imgur.com")
                           :original-url "imgur.com"
                           :redirect-url
                           (quri:uri
                            ,(get-value 'imgur-frontend config))))
          '())
    ,@(if (get-value 'medium-frontend config)
          `((make-instance 'router:redirector
                           :name 'medium
                           :route (match-domain "medium.com")
                           :original-url "www.medium.com"
                           :redirect-url
                           (quri:uri
                            ,(get-value 'medium-frontend config))
                           :instances-builder
                           router:scribe-instances-builder))
          '())
    ,@(if (get-value 'twitter-frontend config)
          `((make-instance 'router:redirector
                           :name 'twitter
                           :route (match-domain "twitter.com")
                           :original-url "www.twitter.com"
                           :redirect-url
                           (quri:uri
                            ,(get-value 'twitter-frontend config))))
          '())
    ,@(if (get-value 'reddit-frontend config)
          `((make-instance 'router:redirector
                           :name 'reddit
                           :route (match-domain "reddit.com")
                           :original-url "www.reddit.com"
                           :redirect-url
                           (quri:uri
                            ,(get-value 'reddit-frontend config))
                           :instances-builder
                           router:teddit-instances-builder)
            (make-instance 'router:blocker
                           :name 'reddit
                           :blocklist
                           '(:path (:contains (not "/comments/" "/wiki/")))))
          '())
    ,@(if (get-value 'tiktok-frontend config)
          `((make-instance 'router:redirector
                           :name 'tiktok
                           :route (match-domain "tiktok.com")
                           :original-url "www.tiktok.com"
                           :redirect-url
                           (quri:uri
                            ,(get-value 'tiktok-frontend config))
                           :redirect-rule
                           `(("/@placeholder/video/" .
                              (not "/" "/@" "/t")))
                           :instances-builder
                           router:proxitok-instances-builder)
            (make-instance 'router:blocker
                           :name 'tiktok
                           :blocklist
                           '(:path (:contains (not "/video/" "/t/")))))
          '())
    ,@(if (get-value 'instagram-frontend config)
          `((make-instance 'router:redirector
                           :name 'instagram
                           :route
                           (match-regex "https://(www.)?insta.*")
                           :original-url "www.instagram.com"
                           :redirect-url
                           (quri:uri
                            ,(get-value 'instagram-frontend config))
                           :redirect-rule
                           '(("/profile/"
                              . (not "/" "/p/" "/tv/" "/reels/"))
                             ("/media/" . "/p/")))
            (make-instance 'router:blocker
                           :name 'instagram
                           :blocklist
                           '(:path (:contains (not "/media/")))))
          '())
    ,@(if (get-value 'fandom-frontend config)
          `((make-instance 'router:redirector
                           :name 'fandom
                           :route (match-domain "fandom.com")
                           :redirect-url
                           ,(format #f "~a/\\1/wiki/\\2"
                                    (get-value 'fandom-frontend config))
                           :redirect-rule
                           ".*://(\\w+)\\.fandom.com/wiki/(.*)"
                           :instances-builder
                           router:breezewiki-instances-builder))
          '())))

(define (nx-search-engines-extra-engines config)
  `((make-instance 'search-engine
                   :shortcut "clj"
                   :search-url "https://clojars.org/search?q=~a"
                   :fallback-url "https://clojars.org")
    (make-instance 'search-engine
                   :shortcut "npm"
                   :search-url "https://www.npmjs.com/search?q=~a"
                   :fallback-url "https://www.npmjs.com")
    (make-instance 'search-engine
                   :shortcut "fa"
                   :search-url "https://fontawesome.com/search?q=~a&m=free"
                   :fallback-url "https://fontawesome.com")
    (make-instance 'search-engine
                   :shortcut "et"
                   :search-url "https://www.etsy.com/search?q=~a"
                   :fallback-url "https://www.etsy.com")
    (make-instance 'search-engine
                   :shortcut "to"
                   :search-url "https://torrents-csv.ml/search/~a"
                   :fallback-url "https://torrents-csv.ml")
    (make-instance 'search-engine
                   :shortcut "ra"
                   :search-url "https://rargb.to/search/?search=~a"
                   :fallback-url "https://rargb.to")
    (make-instance 'search-engine
                   :shortcut "kt"
                   :search-url "https://kickasstorrents.to/usearch/~a"
                   :fallback-url "https://kickasstorrents.to")
    (make-instance 'search-engine
                   :shortcut "mdn"
                   :search-url
                   "https://developer.mozilla.org/en-US/search?q=~a"
                   :fallback-url "https://developer.mozilla.org")
    (make-instance 'search-engine
                   :shortcut "sc"
                   :search-url
                   ,(format #f "https://~a/search?q=~~a&serviceId=1"
                            %tubo-host)
                   :fallback-url ,(string-append "https://" %tubo-host))
    (make-instance 'search-engine
                   :shortcut "yt"
                   :search-url
                   ,(format #f "https://~a/search?q=~~a&serviceId=0"
                            %tubo-host)
                   :fallback-url ,(string-append "https://" %tubo-host))
    (make-instance 'search-engine
                   :shortcut "pt"
                   :search-url
                   ,(format #f "https://~a/search?q=~~a&serviceId=3"
                            %tubo-host)
                   :fallback-url ,(string-append "https://" %tubo-host))
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
    (engines:lemmy
     :shortcut "le"
     :fallback-url (quri:uri "https://beehaw.org")
     :base-search-url "https://beehaw.org/search/q/~a")
    (engines:google
     :shortcut "go"
     :fallback-url (quri:uri "https://www.google.es")
     :base-search-url "https://www.google.es/search?q=~a"
     :safe-search nil
     :lang-ui :english
     :results-number 50
     :new-window t)
    ,@(if (get-value 'google-frontend config)
          `((engines:whoogle
             :shortcut "who"
             :fallback-url
             (quri:uri ,(get-value 'google-frontend config))
             :base-search-url
             ,(string-append (get-value 'google-frontend config)
                             "/search?q=~a")
             :theme :system
             :alternatives nil
             :lang-results :english
             :lang-ui :english
             :view-image t
             :no-javascript t
             :new-tab t))
          '())))

(define-public %base-nyxt-extra-config-lisp
  `((in-package :nyxt-user)
    (nyxt:use-nyxt-package-nicknames)
    (asdf:load-system :nx-mosaic)
    (local-time:reread-timezone-repository)
    (setf local-time:*default-timezone*
          (local-time:find-timezone-by-location-name ,%default-timezone))

    (defun status-button (buffer title action label)
      (spinneret:with-html-string
        (:button :type "button" :class "button" :title title
                 :onclick (ps:ps (nyxt/ps:lisp-eval
                                  (:title title :buffer buffer)
                                  (funcall action)))
                 label)))

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

(define-public (feature-nyxt-nx-tailor-extra-styles)
  (define (get-home-services config)
    (define font-sans (and=> (get-value 'font-sans config) font-name))
    (list
     (simple-service
      'rde-nx-tailor-extension
      (get-value 'nyxt-rde-nx-tailor-service-type config)
      (home-nyxt-lisp-extension
       (config
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
                `(:media "(max-width: 768px)"
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
                `(:let ((font-sans ,,font-sans))
                   ("*, body, div, section, input"
                    :font-family ,theme:font-family
                    :background-color ,theme:background
                    :color ,theme:on-background)
                   ("h1,h2,h3,h4,h5,h6"
                    :color ,theme:on-secondary
                    :font-family #(font-sans))
                   ("p, td , dt, button, .button, a, a:link"
                    :font-family #(font-sans)
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
                `("#selection"
                  :background ,(str:concat theme:primary "E6")
                  :color ,theme:on-background)
                `(.marked
                  :background ,theme:accent
                  :color ,theme:on-accent)))))))))))

  (feature
   (name 'nx-tailor-extra-styles)
   (home-services-getter get-home-services)))

(define-public %nyxt-base-features
  (list
   (feature-nyxt-blocker)
   (feature-nyxt-nx-search-engines
    #:default-engine-shortcut "who"
    #:extra-engines nx-search-engines-extra-engines)
   (feature-nyxt-nx-router
    #:routers nx-router-routers)))

(define-public %security-base-features
  (list
   (feature-password-store
    #:remote-password-store-url
    (format #f "git@git.~a:password-store" %default-domain))))

(define-public %forge-base-features
  (list
   (feature-git
    #:primary-forge-account-id 'sh
    #:extra-config
    `((sendemail
       ((cc . ,%default-email)
        (thread . #t))))
    #:extra-global-ignores
    '("**/.direnv" ".log"))))
