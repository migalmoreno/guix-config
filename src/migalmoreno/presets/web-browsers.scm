(define-module (migalmoreno presets web-browsers)
  #:use-module (migalmoreno utils)
  #:use-module (rde features)
  #:use-module (rde features fontutils)
  #:use-module (rde features nyxt-xyz)
  #:use-module (rde home services web-browsers))

(define (extra-nx-router-routers config)
  (define (alternative-frontend service)
    (assoc-ref (get-value 'alternative-frontends config) service))

  `(,@(if (alternative-frontend 'google)
          `((make-instance 'router:redirector
                           :name 'google
                           :route (match-regex ".*google.com/search.*")
                           :reverse (quri:uri "https://www.google.com")
                           :redirect
                           (quri:uri ,(alternative-frontend 'google))
                           :instances-builder
                           router:whoogle-instances-builder))
          '())
    ,@(if (alternative-frontend 'youtube)
          `((make-instance 'router:redirector
                           :route (match-domain "youtube.com")
                           :redirect
                           (list
                            (list
                             ,(format #f "~a/stream?url=\\&"
                                      (alternative-frontend 'youtube))
                             ".*/watch\\?v.*"
                             ".*/shorts/.*")
                            (cons
                             ,(format #f "~a/playlist?list=\\&"
                                      (alternative-frontend 'youtube))
                             ".*/playlist/.*")
                            (cons
                             ,(format #f "~a/channel?url=\\&"
                                      (alternative-frontend 'youtube))
                             ".*/channel/.*")
                            (cons
                             ,(format #f "~a/search?q=\\1&serviceId=0"
                                      (alternative-frontend 'youtube))
                             ".*/search\\?q=(.*)"))))
          '())
    ,@(if (alternative-frontend 'soundcloud)
          `((make-instance 'router:redirector
                           :route (match-domain "soundcloud.com")
                           :redirect
                           (list
                            (cons
                             ,(format #f "~a/stream?url=\\&"
                                      (alternative-frontend 'soundcloud))
                             ".*/.*/.*"))))
          '())
    ,@(if (alternative-frontend 'quora)
          `((make-instance 'router:redirector
                           :route (match-domain "quora.com")
                           :reverse "www.quora.com"
                           :redirect
                           (quri:uri ,(alternative-frontend 'quora))))
          '())
    ,@(if (alternative-frontend 'imgur)
          `((make-instance 'router:redirector
                           :route (match-domain "imgur.com")
                           :reverse "imgur.com"
                           :redirect
                           (quri:uri ,(alternative-frontend 'imgur))))
          '())
    ,@(if (alternative-frontend 'medium)
          `((make-instance 'router:redirector
                           :route (match-domain "medium.com")
                           :reverse "www.medium.com"
                           :redirect
                           (quri:uri ,(alternative-frontend 'medium))
                           :instances-builder
                           router:scribe-instances-builder))
          '())
    ,@(if (alternative-frontend 'x)
          `((make-instance 'router:redirector
                           :route (match-domain "x.com")
                           :reverse "x.com"
                           :redirect
                           (quri:uri ,(alternative-frontend 'x))))
          '())
    ,@(if (alternative-frontend 'reddit)
          `((make-instance 'router:redirector
                           :route (match-domain "reddit.com")
                           :reverse "www.reddit.com"
                           :redirect
                           (quri:uri ,(alternative-frontend 'reddit))
                           :instances-builder
                           router:teddit-instances-builder))
          '())
    ,@(if (alternative-frontend 'fandom)
          `((make-instance 'router:redirector
                           :route (match-domain "fandom.com")
                           :redirect
                           (list
                            (cons
                             ,(format #f "~a/\\1/wiki/\\2"
                                      (alternative-frontend 'fandom))
                             ".*://(\\w+)\\.fandom.com/wiki/(.*)"))
                           :instances-builder
                           router:breezewiki-instances-builder))
          '())

    (make-instance 'router:opener
                   :route (match-scheme "magnet" "mailto")
                   :resource "xdg-open ~a")))

(define (extra-nx-search-engines config)
  `((make-search-engine "toys" "https://toys.whereis.みんな/?search=~a")
    (make-search-engine "youtube" "https://www.youtube.com/search?q=~a")
    (make-search-engine "soundcloud" "https://soundcloud.com/search?q=~a")
    (engines:google
     :safe-search nil
     :lang-ui :english
     :results-number 50
     :new-window t)
    ,@(if (assoc-ref (get-value 'alternative-frontends config) 'google)
          `((engines:whoogle
             :base-search-url
             ,(format #f "~a/search?q=~~a"
                      (assoc-ref (get-value 'alternative-frontends config)
                                 'google))
             :new-tab t
             :lang-ui :english))
          '())))

(define-public extra-nyxt-config-lisp
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

    (define-configuration (web-buffer
                           prompt-buffer
                           nyxt/mode/editor:editor-buffer)
      ((default-modes `(nyxt/mode/emacs:emacs-mode ,@%slot-value%))))))

(define (extra-nx-router-config _)
  (home-nyxt-lisp-extension
   (config
    `((define-configuration router:opener
        ((router:toplevel-p nil)))

      (define-command copy-url ()
        "Save current URL to clipboard."
        (let ((url (render-url (router:trace-url (url (current-buffer))))))
          (copy-to-clipboard url)
          (echo "~a copied to clipboard." url)))

      (defmethod nyxt:on-signal-load-finished
        :around ((mode nyxt/mode/history:history-mode) url)
        (call-next-method mode (router:trace-url url)))

      (defmethod nyxt/mode/bookmark:bookmark-current-url
        :around (&optional (buffer (current-buffer)))
        (setf (url buffer) (router:trace-url (url buffer)))
        (call-next-method buffer))))))

(define (extra-nx-tailor-config _)
  (home-nyxt-lisp-extension
   (config
    `((define-configuration status-buffer
        ((style
          (tailor:with-style 'status-buffer
            `(body
              :font-family ,theme:monospace-font-family)
            `("#container"
              :background ,theme:background-)
            `("#controls"
              :color ,theme:on-background
              :background inherit)
            `("#controls button"
              :padding "3px")
            `("#url"
              :background inherit
              :color ,theme:on-background
              :font-weight bold
              :font-size "55vh"
              :padding "0 5px"
              :display flex
              :align-items center
              :box-sizing border-box
              :flex-grow 6
              :flex-shrink 3)
            `("#url button"
              :white-space nowrap
              :text-overflow ellipsis
              :overflow hidden)
            `("#modes"
              :background inherit
              :color ,theme:on-background
              :font-size "55vh"
              :flex-grow 1)
            `((:or .arrow-right .arrow-left)
              :clip-path none
              :margin-right 0)
            `((:and (:or .button .tab "#url") :hover)
              :background none
              :color ,theme:on-background)))))

      (define-configuration prompt-buffer
        ((style
          (tailor:with-style 'prompt-buffer
            `(body
              :font-family ,theme:monospace-font-family
              :color ,theme:on-background
              :border none)
            `("#input"
              :padding-left "0 !important")
            `("#input:focus"
              :box-shadow none)
            `("#prompt-area"
              :background ,theme:background
              :border none)
            `("#prompt"
              :padding-left "10px")
            `("#suggestions"
              :margin-right 0)
            `("#selection"
              :background ,(cl-colors2:print-hex theme:on-primary :alpha 0.5)
              :color ,theme:on-background)
            `(.source
              :margin-left 0)
            `(.source-name
              :padding-left "10px"
              :color ,theme:secondary
              :background ,theme:background)
            `(.source-content
              :border-collapse collapse
              :padding-left 0
              :margin-left 0
              (th
               :padding "5px 10px"
               :color ,theme:on-background
               :background ,theme:background
               :font-weight bold)
              (td
               :padding "5px 10px"
               :text-overflow ellipsis))
            `((:or "#prompt" "#prompt-extra")
              :color ,theme:secondary
              :background none)
            `((:or .arrow-right .arrow-left)
              :clip-path none
              :margin-right 0)
            `((:or "#prompt-modes" "#close-button" "#previous-source"
               "#next-source" "#toggle-attributes")
              :display none)))))))))

(define-public (nyxt-features _ palette)
  (define (%nyxt-theme config)
    (nyxt-theme
     (name (if (palette 'light?) 'light 'dark))
     (palette
      `(:background-color ,(palette 'bg)
        :background-color- ,(palette 'bg-alt)
        :on-background-color ,(palette 'fg)
        :primary-color ,(palette (if (palette 'light?) 'accent-3 'accent-0))
        :on-primary-color ,(palette 'accent-1)
        :secondary-color ,(palette 'accent-2)
        :on-secondary-color ,(palette 'accent-3)
        :accent-color ,(palette 'accent-4)
        :on-accent-color ,(palette 'bg)
        :font-family ,(font-name (get-value 'font-sans config))
        :monospace-font-family
        ,(font-name (get-value 'font-monospace config))))))
  (list
   (feature-nyxt-blocker)
   (feature-nyxt-nx-search-engines
    #:default-engine-shortcut "whoogle"
    #:engines extra-nx-search-engines)
   (feature-nyxt-nx-router
    #:routers extra-nx-router-routers)
   (feature-home-extension
    #:name 'nyxt-nx-router-extra-config
    #:service 'nyxt-rde-nx-router-service-type
    #:extension extra-nx-router-config)
   (feature-nyxt-nx-tailor
    #:auto? #f
    #:themes (lambda (config) (list (%nyxt-theme config))))
   (feature-home-extension
    #:name 'nyxt-nx-tailor-extra-config
    #:service 'nyxt-rde-nx-tailor-service-type
    #:extension extra-nx-tailor-config)
   (feature-nyxt-appearance
    #:custom-theme %nyxt-theme)))
