(define-module (quasar home services web-browsers)
  #:use-module (quasar home)
  #:use-module (conses home services web-browsers)
  #:use-module (conses home services emacs)
  #:use-module (conses packages web-browsers)
  #:use-module (gnu home-services base)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (gnu services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages browser-extensions)
  #:use-module (gnu packages gstreamer)
  #:use-module (guix gexp)
  #:use-module (ice-9 ftw)
  #:export (nyxt-service
            web-service))

(define appearance-service
  (nyxt-configuration-service
   `((import 'tailor:make-important)
     ,#~""
     (define-configuration tailor:cut
       ((tailor:name "Minimal")
        (tailor:prompt
         '((* :font-family theme:font-family)
           ("#prompt-modes"
            :display "none")
           ("#prompt-area"
            :background-color theme:secondary
            :color theme:on-secondary
            :border "1px solid"
            :border-color (if (theme:dark-p theme:theme)
                              theme:on-secondary
                              theme:on-background))
           ("#input"
            :background-color theme:secondary
            :color theme:on-background)
           (".source-content"
            :border "none"
            :border-collapse collapse)
           (".source-name"
            :background-color theme:background
            :color theme:on-background
            :font-style "italic")
           (".source-content th"
            :padding-left "0"
            :background-color theme:background
            :font-weight "bold")
           (".source-content td"
            :padding "0 2px")
           ("#selection"
            :font-weight "bold"
            :background-color theme:secondary
            :color theme:on-background)))
        (tailor:buffer
         '(("*, body, div, section, input"
            :font-family theme:font-family
            :background-color theme:background
            :color theme:on-background)
           ("h1,h2,h3,h4,h5,h6"
            :font-family "IBM Plex Sans"
            :color (make-important theme:primary))
           ("p,pre,td,dt"
            :font-family "IBM Plex Sans"
            :color theme:on-background)
           (pre
            :font-family "Iosevka"
            :background-color (make-important theme:background))
           ("button, a, a:link"
            :color theme:on-background
            :font-family "IBM Plex Sans")
           (".button, .button:hover , .button:visited, .button:active"
            :background-color theme:secondary
            :border "1px solid"
            :border-color (if (theme:dark-p theme:theme)
                              theme:on-secondary
                              theme:on-background)
            :color theme:on-background)
           (code
            :font-family "Iosevka"
            :color (make-important theme:on-background)
            :background-color (make-important theme:secondary))))
        (tailor:status
         '((body
            :font-family theme:font-family
            :height "100%"
            :width "100%"
            :border "1px solid"
            :border-color (if (theme:dark-p theme:theme)
                              theme:on-secondary
                              theme:on-background)
            :box-sizing "border-box"
            :line-height "20px"
            :display "flex"
            :flex-direction "column"
            :background theme:secondary
            :flex-wrap "wrap")
           ("#container"
            :display "flex"
            :height "100%"
            :width "100%"
            :line-height "20px"
            :justify-content "space-between"
            :align-items "center")
           ("#buttons"
            :display "flex"
            :align-items "center"
            :justify-content "center"
            :line-height "20px"
            :height "100%")
           ("#url"
            :font-weight "bold"
            :max-width "60%"
            :padding-right "0"
            :padding-left "5px"
            :text-overflow "ellipsis"
            :background-color theme:secondary
            :color theme:on-background
            :box-sizing "border-box"
            :z-index "auto")
           ("#tabs, #controls" :display "none")
           ("#modes"
            :padding-right "2px"
            :background-color theme:secondary
            :box-sizing "border-box"
            :color theme:on-background
            :display "flex"
            :justify-contents "flex-end"
            :z-index "auto")
           (.button
            :color theme:on-background)))
        (tailor:message
         '((body
            :color theme:on-background
            :background-color theme:background
            :font-family theme:font-family)))))
     ,#~""
     (local-time:reread-timezone-repository)
     (setf local-time:*default-timezone* (local-time:find-timezone-by-location-name ,(getenv "LYRA_TIMEZONE")))
     (define-configuration tailor:tailor-mode
       ((tailor:auto-p :time)
        (tailor:light-theme-threshold (* 6 60 60))
        (tailor:dark-theme-threshold (* 21.5 60 60))
        (tailor:main '("modus-operandi" . "modus-vivendi"))
        (tailor:themes
         (list
          (make-theme "modus-operandi"
                      :background-color "white"
                      :on-background-color "black"
                      :primary-color "#093060"
                      :secondary-color "#dfdfdf"
                      :on-secondary-color "black"
                      :accent-color "#8f0075"
                      :on-accent-color "#005a5f"
                      :font-family "Iosevka"
                      :cut (make-instance 'tailor:cut))
          (make-theme "modus-vivendi"
                      :dark-p t
                      :background-color "black"
                      :on-background-color "white"
                      :primary-color "#c6eaff"
                      :secondary-color "#323232"
                      :on-secondary-color "#a8a8a8"
                      :accent-color "#afafef"
                      :on-accent-color "#a8a8a8"
                      :font-family "Iosevka"
                      :cut (make-instance 'tailor:cut))
          (make-theme "Dracula"
                      :dark-p t
                      :background-color "#282a36"
                      :on-background-color "#f8f8f2"
                      :primary-color "#bd93f9"
                      :secondary-color "#6272a4"
                      :on-secondary-color "#f8f8f2"
                      :accent-color "#8be9fd"
                      :on-accent-color "#282a36"
                      :font-family "Iosevka"
                      :cut (make-instance 'tailor:cut))))))
     ,#~""
     (define-configuration nxdr:dark-reader-mode
       ((nxdr:brightness 80)
        (nxdr:text-color "white")
        (nyxt:glyph "nxdr")))
     ,#~""
     (define-configuration nyxt/user-script-mode:user-script-mode
       ((nyxt/user-script-mode:user-styles
         (list
          (make-instance 'nyxt/user-script-mode:user-style
                         :include '("https://github.com/*")
                         :code (cl-css:css
                                `((,(str:join "," '("#dashboard .body"
                                                    ".js-inline-dashboard-render"
                                                    ".js-feed-item-component"
                                                    ".js-yearly-contributions"
                                                    ".js-form-toggle-container"
                                                    "#js-contribution-activity"
                                                    ".starring-container"
                                                    "a[href$=watchers]"
                                                    "a[href$=stargazers]"
                                                    "a[href$=followers]"
                                                    "a[href$=following]"))
                                   :display "none !important"))))))))
     ,#~""
     (defmethod custom-format-buttons ((status status-buffer))
       "Buttons used for custom status formatter."
       (declare (ignorable status))
       (spinneret:with-html-string
        (:div :id "buttons"
         (:button :type "button"
          :title "Close Buffer"
          :onclick (ps:ps (nyxt/ps:lisp-eval
                           (:title "close current buffer")
                           (nyxt:delete-current-buffer)))
          "🞫"))))
     ,#~""
     (defmethod custom-formatter ((status status-buffer))
       (let ((buffer (current-buffer (window status))))
         (spinneret:with-html-string
          (:div :id "container"
           (:div :id "url"
            (:raw (format-status-load-status buffer)
             (format-status-url buffer)))
           (:div :id "modes"
            :title (nyxt::modes-string buffer)
            (:raw (format-status-modes status)))))))
     ,#~""
     (define-mode graphical-status-mode ()
       "Applies graphical buttons and interfaces to the status bar.")
     ,#~""
     (defmethod enable ((mode graphical-status-mode) &key)
       ;; (setf (format-status (current-window))
       ;;       (lambda (status)
       ;;         (str:concat (custom-format-buttons status)
       ;;                     (custom-formatter status))))
       )
     ,#~""
     (defmethod disable ((mode graphical-status-mode) &key)
       ;; (setf (format-status (current-window)) 'custom-formatter)
       )
     ,#~""
     (define-configuration status-buffer
       ((glyph-mode-presentation-p t))))))

(define engines-service
  (nyxt-configuration-service
   `((asdf:load-system :nx-search-engines)
     ,#~""
     (define-configuration buffer
       ((search-engines
         (append
          %slot-default%
          (list
           (engines:wordnet
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
            :shortcut "yt")
           (engines:lemmy
            :shortcut "le")
           (engines:discourse
            :shortcut "ae")
           (engines:discourse
            :shortcut "cv"
            :fallback-url (quri:uri "https://clojureverse.org")
            :base-search-url "https://clojureverse.org/search?q=~a")
           (engines:meetup
            :shortcut "me")
           (engines:gitea
            :shortcut "gi")
           (engines:gitea-users
            :shortcut "giu")
           (engines:teddit
            :shortcut "re"
            :base-search-url "https://teddit.namazso.eu/search?q=~a")
           (engines:hacker-news
            :shortcut "hn"
            :search-type :all)
           (engines:lobsters
            :shortcut "lo")
           (engines:whoogle
            :shortcut "who"
            :base-search-url "http://localhost:5000/search?q=~a"
            :completion-function nil
            :theme :system
            :alternatives nil
            :view-image t
            :no-javascript t
            :new-tab t))))))
     ,#~""
     (defun simple-search (query)
       "Executes a simple search with QUERY in the default search engine."
       (buffer-load
        (first (nyxt::input->queries query))
        :buffer (make-buffer-focus))))))

(define routing-service
  (nyxt-configuration-service
   `((defun make-invidious-instances ()
         "Returns a list of Invidious instances."
         (mapcar 'first
                 (json:with-decoder-simple-list-semantics
                  (json:decode-json-from-string
                   (dex:get "https://api.invidious.io/instances.json")))))
     ,#~""
     (defun make-bibliogram-instances ()
       "Returns a list of Bibliogram instances."
       (mapcar (lambda (instance)
                 (alex:assoc-value instance :address))
               (alex:assoc-value
                (json:with-decoder-simple-list-semantics
                 (json:decode-json-from-string
                  (dex:get "https://bibliogram.pussthecat.org/api/instances")))
                :data)))
     ,#~""
     (defun make-teddit-instances ()
       "Returns a list of Teddit instances."
       (mapcar (lambda (instance)
                 (unless (str:emptyp (alex:assoc-value instance :url))
                   (alex:assoc-value instance :url)))
               (json:with-decoder-simple-list-semantics
                (json:decode-json-from-string (dex:get "https://teddit.net/instances.json")))))
     ,#~""
     (define-configuration router:router-mode
       ((router:enforce-p nil)
        (router:media-enabled-p t)
        (router:banner-p t)
        (router:routes
         (list
          (make-route '(match-regex "https://insta.*")
                      :redirect '("bibliogram.pussthecat.org" (:path ("/u" (not "/" "/p/" "/tv"))))
                      :instances 'make-bibliogram-instances)
          (make-route (match-domain "tiktok.com")
                      :redirect "proxitok.herokuapp.com"
                      :blocklist '(:path (:contains (not "video"))))
          (make-route '(match-domain "reddit.com")
                      :redirect "teddit.namazso.eu"
                      :blocklist '(:path (:contains (not "/comments"))))
          (make-route (match-host "news.ycombinator.com")
                      :blocklist '(:path (:contains (not "item"))))
          (make-route (match-regex "https://whoogle.*"
                                   "https://.*google.com/search.*")
                      :redirect "localhost:5000")
          (make-route '(match-regex ".*/watch\\?v=.*")
                      :redirect "youtube.com"
                      :external (lambda (data)
                                  (eval-in-emacs
                                   `(eb-media-mpv-start ,(quri:render-uri (url data))
                                                             :audio-only t :repeat t))))
          (make-route '(match-regex "https://gfycat.com/.*"
                                    "https://streamable.com/.*"
                                    ".*/w/.*"
                                    ".*master.m3u8")
                      :external (lambda (data)
                                  (eval-in-emacs
                                   `(eb-media-mpv-start ,(quri:render-uri (url data))))))
          (make-route (match-scheme "magnet")
                      :external (lambda (data)
                                  (eval-in-emacs
                                   `(transmission-add ,(quri:render-uri (url data))))))
          (make-route '(match-domain "youtube.com" "youtu.be")
                      :redirect "invidious.namazso.eu"
                      :blocklist '(:path (:starts ("/c/"))))
          (make-route (match-domain "medium.com")
                      :redirect "scribe.rip")
          (make-route (match-domain "imgur.com")
                      :redirect "imgin.voidnet.tech")
          (make-route (match-domain "lemmy.ml")
                      :blocklist '(:path (:starts ("/u/" "/c"))))
          (make-route (match-domain "twitter.com")
                      :redirect "nitter.42l.fr")
          (make-route (match-regex ".*moodle.*(pdf|ppt|doc|pptx|docx)")
                      :external (lambda (data)
                                  (eval-in-emacs
                                   `(eb-web-open-with-cookies
                                     `(("MoodleSession" ,(password-store-get "education/moodle/session"))
                                       ("SimpleSAMLSessionID" ,(password-store-get "education/moodle/id"))
                                       ("SimpleSAMLAuthToken" ,(password-store-get "education/moodle/token")))
                                     ,(quri:render-uri (url data))))))
          (make-route (match-file-extension "pdf" "pptx" "docx")
                      :external "xdg-open ~a"))))))))

(define (nyxt-service)
  (list
   (simple-service
    'home-nyxt-xdg
    home-xdg-mime-applications-service-type
    (home-xdg-mime-applications-configuration
     (default
      '((x-scheme-handler/http . nyxt.desktop)
        (x-scheme-handler/https . nyxt.desktop)
        (x-scheme-handler/about . nyxt.desktop)))
     (desktop-entries
      (let ((nyxt-file (format #f "~a/src/nyxt/build-scripts/nyxt.scm" (getenv "HOME"))))
        (if (file-exists? nyxt-file)
            (list
             (xdg-desktop-entry
              (file "nyxt-dev")
              (name "Nyxt Development")
              (type 'application)
              (config
               `((exec . ,(format #f "guix shell -D -f ~a -- ~a" nyxt-file
                                  (string-append ((compose dirname dirname) nyxt-file) "/nyxt")))))))
            '())))))
   routing-service
   appearance-service
   engines-service
   (service home-nyxt-service-type
            (home-nyxt-configuration
             (package nyxt-next)
             (config-lisp
              `((let ((sbcl-init (merge-pathnames ".sbclrc" (user-homedir-pathname))))
                  (when (probe-file sbcl-init)
                    (load sbcl-init)))
                ,#~""
                (asdf:ensure-source-registry)
                (reset-asdf-registries)
                (asdf:load-system :nx-router)
                (import 'router:make-route)
                (asdf:load-system :nx-tailor)
                (import 'tailor:make-theme)
                (asdf:load-system :nx-dark-reader)
                ,#~""
                (in-package #:nyxt-user)
                (use-nyxt-package-nicknames)
                ,#~""
                (defvar *custom-keymap* (make-keymap "custom-map"))
                (define-key *custom-keymap*
                  "C-m" 'nyxt/bookmark-mode:bookmark-current-url
                  "C-s" 'nyxt/search-buffer-mode:search-buffer
                  "C-b" 'nyxt/history-mode:history-backwards
                  "M-n" 'switch-buffer-next
                  "M-p" 'switch-buffer-previous
                  "M-c f" 'nyxt/document-mode:focus-first-input-field
                  "M-c b" 'switch-buffer
                  "M-c k" 'delete-buffer
                  "M-c m" 'nyxt/bookmark-mode:set-url-from-bookmark
                  "M-c i" 'open-inspector
                  "M-c s" 'query-selection-in-search-engine
                  "M-h v" 'describe-variable
                  "M-h s" 'describe-slot
                  "M-h f" 'describe-function
                  "M-h c" 'describe-class
                  "M-h k" 'describe-key)
                (defmethod files:resolve ((profile nyxt-profile) (file nyxt:history-file))
                  "Store history in a temporary directory."
                  (sera:path-join (nfiles:expand (make-instance 'nyxt-temporary-directory))
                                  (uiop:relativize-pathname-directory (call-next-method))))
                (define-mode custom-keymap-mode ()
                  "Dummy mode for the custom key bindings in `*custom-keymap*.'"
                  ((keyscheme-map (keymaps:make-keyscheme-map
                                   keyscheme:emacs *custom-keymap*))
                   (visible-in-status-p nil)))
                ,#~""
                (define-configuration buffer
                  ((smooth-scrolling t)
                   (scroll-distance 150)
                   (default-modes `(nyxt/emacs-mode:emacs-mode
                                    nyxt/blocker-mode:blocker-mode
                                    nyxt/user-script-mode:user-script-mode
                                    custom-keymap-mode
                                    tailor:tailor-mode
                                    router:router-mode
                                    nxdr:dark-reader-mode
                                    ,@%slot-default%))))
                ,#~""
                (define-configuration browser
                  ((default-cookie-policy :no-third-party)
                   (session-restore-prompt :never-restore)))
                ,#~""
                (define-configuration nyxt/hint-mode:hint-mode
                  ((nyxt/hint-mode:hints-alphabet "asdfghjklqwertyuiop")
                   (visible-in-status-p nil)))
                ,#~""
                (define-configuration (nyxt/certificate-exception-mode:certificate-exception-mode
                                       nyxt/auto-mode:auto-mode)
                  ((visible-in-status-p nil)))
                ,#~""
                (sera:export-always 'eval-in-emacs)
                (defun eval-in-emacs (&rest s-exps)
                       "Evaluate S-EXPS with `emacsclient'."
                       (let ((s-exps-string (str:replace-all
                                             "nyxt::" "" (write-to-string
                                                          `(progn ,@s-exps) :case :downcase))))
                         (format *error-output* "Sending to Emacs:~%~a~%" s-exps-string)
                         (uiop:run-program
                          (list "emacsclient" "-e" s-exps-string))))))
             (auto-mode-rules-lisp
              '((((match-host "wikipedia.org") :included (nyxt/style-mode:dark-mode)))))))
   (elisp-configuration-service
    `((with-eval-after-load 'consult
        (add-to-list 'consult-bookmark-narrow
                     `(?n "Nyxt" ,'eb-web--jump-to-bookmark)))
      (let ((map mode-specific-map))
        (define-key map "rn" 'eb-web-connect-to-slynk)
        (define-key map "ws" 'eb-web-search)
        (define-key map "wb" 'eb-web-nyxt-set-up-window)
        (define-key map "ww" 'eb-web-copy-url)
        (define-key map "wv" 'eb-web-set-nyxt-transient-map))
      ,#~""
      (with-eval-after-load 'eb-web
        (custom-set-variables
         '(eb-web-nyxt-development-p nil)
         '(eb-web-nyxt-startup-threshold 8)))))))

(define* (web-service #:key alt-browser-p)
  (let ((chromium-flags (list "--remove-tabsearch-button"
                              "--incognito"
                              "--blink-settings=imagesEnabled=false"
                              "--show-avatar-button=never")))
    (list
     (if alt-browser-p
         (home-generic-service 'home-browser-packages
                               #:packages (list
                                           ungoogled-chromium
                                           ublock-origin/chromium))
         '())
     (simple-service
      'web-environment-variables
      home-environment-variables-service-type
      `(("BROWSER" . #t)))
     (if alt-browser-p
         (simple-service
          'home-chromium-xdg
          home-xdg-mime-applications-service-type
          (home-xdg-mime-applications-configuration
           (desktop-entries
            (list
             (xdg-desktop-entry
              (file "chromium")
              (name "Chromium")
              (type 'application)
              (config
               `((exec . ,#~(string-append
                             #$(file-append ungoogled-chromium "/bin/chromium")
                             " "
                             #$(string-join chromium-flags)
                             " %U"))
                 (terminal . #f)
                 (comment . "Access the Internet"))))))))
         '())
     (elisp-configuration-service
      `((add-hook 'eww-mode-hook 'eb-web-eww-mode)
        (with-eval-after-load 'eww
          (custom-set-variables
           '(eww-search-prefix "https://search.sethforprivacy.com/search?q=")))
        ,#~""
        (with-eval-after-load 'embark
          (define-key embark-bookmark-map "c" 'eb-web--jump-to-bookmark-alt))
        ,#~""
        (with-eval-after-load 'browse-url
          (custom-set-variables
           '(browse-url-generic-program (if-let ((file (expand-file-name
                                                        "nyxt-dev.desktop"
                                                        (concat (xdg-data-home)
                                                                "/applications/")))
                                                 (browser (not (getenv "BROWSER"))))
                                                (and (file-exists-p file)
                                                     (gethash "Exec" (xdg-desktop-read-file file)))
                                                (getenv "BROWSER")))
           '(browse-url-browser-function 'browse-url-xdg-open)
           '(browse-url-chromium-arguments ',chromium-flags)))
        ,#~""
        (let ((map mode-specific-map))
          (define-key map "Wb" 'webpaste-paste-buffer)
          (define-key map "Wr" 'webpaste-paste-region)
          (define-key map "Wp" 'webpaste-paste-buffer-or-region)
          (define-key map "Wt" 'eb-web-webpaste-text))
        (with-eval-after-load 'webpaste
          (custom-set-variables
           '(webpaste-provider-priority '("bpa.st" "bpaste.org" "dpaste.org" "dpaste.com"))
           '(webpaste-paste-confirmation t)))
        ,#~""
        (advice-add 'browse-url-xdg-open :around 'eb-web-add-url-scheme))
      #:elisp-packages (list emacs-webpaste
                             emacs-nginx-mode)))))
