(define-module (conses features nyxt-xyz)
  #:use-module (conses features fontutils)
  #:use-module (conses features web-browsers)
  #:use-module (conses packages emacs-xyz)
  #:use-module (conses utils)
  #:use-module (conses home services lisp)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (guix gexp)
  #:export (feature-nyxt-status
            feature-nyxt-prompt
            feature-nyxt-emacs
            feature-nyxt-mosaic
            feature-nyxt-userscript
            feature-nyxt-blocker
            feature-nyxt-hint
            feature-nyxt-nx-tailor
            feature-nyxt-nx-router
            feature-nyxt-nx-search-engines))

(define* (feature-nyxt-status
          #:key
          (height 24)
          (glyphs? #f)
          (format-status #f)
          (format-status-buttons #f)
          (format-status-load-status #f)
          (format-status-url #f)
          (format-status-tabs #f))
  "Configure Nyxt's status buffer and status area."
  (ensure-pred integer? height)
  (ensure-pred boolean? glyphs?)
  (ensure-pred maybe-list? format-status)
  (ensure-pred maybe-list? format-status-buttons)
  (ensure-pred maybe-list? format-status-load-status)
  (ensure-pred maybe-list? format-status-url)
  (ensure-pred maybe-list? format-status-tabs)

  (define nyxt-f-name 'status)
  (define f-name (symbol-append 'nyxt- nyxt-f-name))

  (define (get-home-services config)
    "Return home services related to the appearance of status items."
    (list
     (rde-nyxt-configuration-service
      nyxt-f-name
      config
      `(,@(if (get-value 'nyxt-nx-tailor config)
              '((define-configuration status-buffer
                  ((style
                    (tailor:with-style 'status-buffer
                      ("#container"
                       :height "100%"
                       :width "100%"
                       :background theme:primary
                       :font-family theme:font-family
                       :align-items "center"
                       :padding "0 5px"
                       :box-sizing "border-box")
                      ("#controls"
                       :background "inherit"
                       :flex-basis "auto")
                      ("#controls button"
                       :color theme:on-background)
                      (".arrow-right, .arrow-left"
                       :clip-path "none"
                       :margin-right 0)
                      ("#url"
                       :background "inherit"
                       :color theme:on-background
                       :font-weight "bold"
                       :padding-left "5px"
                       :flex-basis "auto"
                       :display "flex"
                       :align-items "center")
                      ("#url button"
                       :white-space "nowrap"
                       :text-overflow "ellipsis"
                       :overflow "hidden")
                      ("#modes"
                       :background "inherit"
                       :padding-left "5px")))))
                (define-configuration window
                  ((message-buffer-style
                    (tailor:with-style 'window
                      (body
                       :color theme:on-background
                       :background theme:background
                       :font-family theme:font-family
                       :overflow-x "hidden"))))))
            '())

        (defmethod format-status-back-button ((status status-buffer))
          (spinneret:with-html-string
            (:button :type "button" :class "button"
                     :title "Backwards"
                     :onclick (ps:ps (nyxt/ps:lisp-eval
                                      (:title "history-backwards" :buffer status)
                                      (nyxt/history-mode:history-backwards))) "⊲")))
        (defmethod format-status-reload-button ((status status-buffer))
          (spinneret:with-html-string
            (:button :type "button" :class "button"
                     :title "Reload"
                     :onclick (ps:ps (nyxt/ps:lisp-eval
                                      (:title "reload" :buffer status)
                                      (nyxt:reload-current-buffer))) "↺")))
        (defmethod format-status-forwards-button ((status status-buffer))
          (spinneret:with-html-string
            (:button :type "button" :class "button"
                     :title "Forwards"
                     :onclick (ps:ps (nyxt/ps:lisp-eval
                                      (:title "history-forwards" :buffer status)
                                      (nyxt/history-mode:history-forwards))) "⊳")))
        (defmethod format-status-execute-button ((status status-buffer))
          (spinneret:with-html-string
            (:button :type "button" :class "button"
                     :title "Execute"
                     :onclick (ps:ps (nyxt/ps:lisp-eval
                                      (:title "execute-command" :buffer status)
                                      (nyxt:execute-command))) "≡")))
        (defmethod format-status-close-button ((status status-buffer))
          (spinneret:with-html-string
            (:button :type "button" :class "button"
                     :title "Close Buffer"
                     :onclick (ps:ps (nyxt/ps:lisp-eval
                                      (:title "close current buffer" :buffer status)
                                      (nyxt:delete-current-buffer))) "🞫")))
        (defmethod format-status-switch-buffer-button ((status status-buffer))
          (spinneret:with-html-string
            (:button :type "button" :class "button"
                     :title "Switch Buffers"
                     :onclick (ps:ps (nyxt/ps:lisp-eval
                                      (:title "switch current buffer" :buffer status)
                                      (nyxt:switch-buffer))) "◱")))
        ,@(if format-status-buttons
              `((defmethod format-status-buttons :around ((status status-buffer))
                  (spinneret:with-html-string
                    ,@format-status-buttons)))
              '())
        ,@(if format-status-load-status
              `((defmethod format-status-load-status :around ((status status-buffer))
                  (spinneret:with-html-string
                    ,@format-status-load-status)))
              '())
        ,@(if format-status-url
              `((defmethod format-status-url :around ((status status-buffer))
                  (spinneret:with-html-string
                    ,@format-status-url)))
              '())
        ,@(if format-status-tabs
              `((defmethod format-status-tabs :around ((status status-buffer))
                  (spinneret:with-html-string
                    ,@format-status-tabs)))
              '())
        ,@(if format-status
              `((defmethod format-status :around ((status status-buffer))
                  (let ((buffer (current-buffer (window status))))
                    (spinneret:with-html-string ,@format-status))))
              '())
        ,@(if glyphs?
              '((define-configuration nyxt/blocker-mode:blocker-mode ((glyph "⨂")))
                (define-configuration nyxt/user-script-mode:user-script-mode ((glyph "★"))))
              '())
        (define-configuration status-buffer
          ((glyph-mode-presentation-p ,(if glyphs? 't 'nil))
           (height ,height)))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-nyxt-prompt
          #:key
          (mouse-support? #t))
  "Configure the Nyxt prompt-buffer."
  (ensure-pred boolean? mouse-support?)

  (define nyxt-f-name 'prompt)
  (define f-name (symbol-append 'nyxt- nyxt-f-name))

  (define (get-home-services config)
    "Return home services related to the Nyxt prompt buffer."
    (list
     (rde-nyxt-configuration-service
      nyxt-f-name
      config
      `((define-configuration prompt-buffer
          (,@(append
              `((mouse-support-p ,(if mouse-support? 't 'nil)))
              (if (get-value 'nyxt-nx-tailor config)
                  '((style (tailor:with-style 'prompt-buffer
                             (* :font-family theme:font-family)
                             ("#prompt-area"
                              :background theme:background
                              :color theme:on-secondary)
                             ("#prompt"
                              :padding-left "15px")
                             ("#input"
                              :background theme:background)
                             (".source"
                              :margin 0)
                             (".source-name"
                              :background theme:background
                              :color theme:on-primary
                              :font-style "italic"
                              :padding "5px 15px")
                             (".source-content"
                              :border-collapse "collapse"
                              :margin-left 0)
                             (".source-content td"
                              :padding "5px 15px"
                              :text-overflow "ellipsis")
                             (".source-content th"
                              :padding "5px 15px"
                              :background theme:background
                              :font-weight "bold")
                             ("#selection"
                              :background (str:concat theme:primary "E6")
                              :color theme:on-background)
                             (.marked
                              :background theme:accent
                              :color theme:on-accent))))
                  '()))))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-nyxt-emacs
          #:key
          (emacs-nyxt emacs-nyxt))
  "Configure integration with the GNU Emacs editor in Nyxt."
  (ensure-pred file-like? emacs-nyxt)

  (define nyxt-f-name 'emacs)
  (define f-name (symbol-append 'nyxt- nyxt-f-name))

  (define (get-home-services config)
    "Return home services related to the Emacs integration in Nyxt."

    (define development? (get-value 'nyxt-development? config))
    (define startup-flags (get-value 'nyxt-startup-flags config))

    (list
     (rde-nyxt-configuration-service
      nyxt-f-name
      config
      '((defun eval-in-emacs (&rest s-exps)
          "Evaluate S-EXPS with `emacsclient'."
          (let ((s-exps-string (str:replace-all
                                "nyxt::" "" (write-to-string
                                             `(progn ,@s-exps) :case :downcase))))
            (format *error-output* "Sending to Emacs:~%~a~%" s-exps-string)
            (uiop:run-program
             (list "emacsclient" "-e" s-exps-string))))

        (define-configuration (web-buffer prompt-buffer nyxt/editor-mode:editor-buffer)
          ((default-modes `(nyxt/emacs-mode:emacs-mode ,@%slot-value%))))))
     (rde-elisp-configuration-service
      nyxt-f-name
      config
      `(,@(if (get-value 'emacs-consult config)
              '((with-eval-after-load 'consult
                  (add-to-list
                   'consult-bookmark-narrow
                   `(?n "Nyxt" ,'configure-browse-url-bookmark-jump))))
              '())
        (with-eval-after-load 'nyxt
          ,@(cond
             ((and development? (null? startup-flags))
              `((setq nyxt-path (executable-find "guix"))
                (setq nyxt-startup-flags
                      (when (file-exists-p
                             (expand-file-name
                              "nyxt-dev.desktop"
                              (concat (xdg-data-home) "/applications/")))
                        (thread-last
                         (xdg-data-home)
                         (format "%s/applications/nyxt-dev.desktop")
                         (xdg-desktop-read-file)
                         (gethash "Exec")
                         (split-string)
                         (cdr))))))
             ((not (null? startup-flags))
              `((setq nyxt-startup-flags ',startup-flags)))
             (else '())))
        (with-eval-after-load 'nyxt-autoloads
          (nyxt-default-keybindings)))
      #:elisp-packages (list emacs-nyxt))))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             (emacs-nyxt . ,emacs-nyxt)))
   (home-services-getter get-home-services)))

(define* (feature-nyxt-mosaic)
  "Configure nx-mosaic, an extensible and configurable new-buffer page for Nyxt."

  (define nyxt-f-name 'mosaic)
  (define f-name (symbol-append 'nyxt- nyxt-f-name))

  (define (get-home-services config)
    "Return home services related to nx-mosaic."
    (list
     (rde-nyxt-configuration-service
      nyxt-f-name
      config
      `(())
      #:lisp-packages '(nx-mosaic))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-nyxt-userscript
          #:key
          (userscripts '())
          (userstyles '()))
  "Configure Nyxt's userscript-mode."
  (ensure-pred lisp-config? userscripts)
  (ensure-pred lisp-config? userstyles)

  (define nyxt-f-name 'userscript)
  (define f-name (symbol-append 'nyxt- nyxt-f-name))

  (define (get-home-services config)
    "Return home services related to userscript-mode."
    (list
     (rde-nyxt-configuration-service
      nyxt-f-name
      config
      `((define-configuration web-buffer
          ((default-modes `(nyxt/user-script-mode:user-script-mode ,@%slot-value%))))
        (define-configuration nyxt/user-script-mode:user-script-mode
          ((nyxt/user-script-mode:user-styles (list ,@userstyles))
           (nyxt/user-script-mode:user-scripts (list ,@userscripts))))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-nyxt-blocker
          #:key
          (blocked-hosts '()))
  "Configure Nyxt's blocker mode."
  (ensure-pred list-of-strings? blocked-hosts)

  (define nyxt-f-name 'blocker)
  (define f-name (symbol-append 'nyxt- nyxt-f-name))

  (define (get-home-services config)
    "Return home services related to blocker-mode."
    (list
     (rde-nyxt-configuration-service
      nyxt-f-name
      config
      `((define-configuration web-buffer
          ((default-modes `(nyxt/blocker-mode:blocker-mode ,@%slot-value%))))
        (define-configuration nyxt/blocker-mode:blocker-mode
          ((nyxt/blocker-mode:hostlists (append
                                         (list
                                          (nyxt/blocker-mode:make-hostlist
                                           :hosts ',blocked-hosts))
                                         (list %slot-default%)))))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-nyxt-hint
          #:key
          (hints-alphabet "asdfghjklqwertyuiop"))
  "Configure hint-mode for quicker link navigation in Nyxt."
  (ensure-pred string? hints-alphabet)

  (define nyxt-f-name 'hint)
  (define f-name (symbol-append 'nyxt- nyxt-f-name))

  (define (get-home-services config)
    "Return home services related to hint-mode."
    (list
     (rde-nyxt-configuration-service
      nyxt-f-name
      config
      `((define-configuration nyxt/hint-mode:hint-mode
          ((nyxt/hint-mode:hints-alphabet ,hints-alphabet)))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-nyxt-nx-tailor
          #:key
          (auto? #t)
          (dark-theme? #f)
          (themes '())
          (main-themes #f))
  "Configure nx-tailor, a Nyxt theme manager."
  (ensure-pred maybe-symbol? auto?)
  (ensure-pred boolean? dark-theme?)
  (ensure-pred lisp-config? themes)
  (ensure-pred maybe-pair-or-string? main-themes)

  (define nyxt-f-name 'nx-tailor)
  (define f-name (symbol-append 'nyxt- nyxt-f-name))

  (define (get-home-services config)
    "Return home services related to nx-tailor."
    (define system-timezone (let* ((port (open-input-file "/etc/timezone"))
                                   (res (read-line port)))
                              (close-port port)
                              res))
    (define timezone (get-value 'timezone config system-timezone))
    (define font-sans (and=> (get-value 'font-sans config) font-name))

    (list
     (rde-nyxt-configuration-service
      nyxt-f-name
      config
      `((local-time:reread-timezone-repository)
        (setf local-time:*default-timezone*
              (local-time:find-timezone-by-location-name ,timezone))
        (define-configuration web-buffer
          ((style
            (tailor:with-style 'web-buffer
              ("*, body, div, section, input"
               :font-family theme:font-family
               :background-color theme:background
               :color theme:on-background)
              ("h1,h2,h3,h4,h5,h6"
               :color theme:on-secondary
               :font-family ,font-sans)
              ("p, td , dt, button, .button, a, a:link"
               :font-family ,font-sans
               :color theme:on-background)
              ("button, .button"
               :padding "10px")
              (pre
               :font-family theme:font-family
               :color theme:on-background
               :background theme:background)
              (code
               :font-family theme:font-family
               :color theme:on-background
               :background (if (theme:dark-p theme:theme)
                               theme:secondary
                               theme:primary))))))
        (define-configuration web-buffer
          ((default-modes `(tailor:tailor-mode ,@%slot-value%))))
        (define-configuration nyxt/repl-mode:repl-mode
          ((style (tailor:with-style 'nyxt/repl-mode:repl-mode
                    (.input-buffer
                     :color theme:on-background)))))
        (define-configuration tailor:tailor-mode
          ((tailor:auto-p ,(match auto?
                             ((? symbol? e) e)
                             (#t 't)
                             (#f 'nil)))
           (tailor:main ,(or main-themes (if dark-theme? ':dark ':light)))
           (tailor:themes (list ,@themes)))))
      #:lisp-packages '(nx-tailor))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-nyxt-nx-search-engines
          #:key
          (engines '()))
  "Configure nx-search-engines, a collection of easy-to-setup
search engines for Nyxt."
  (ensure-pred lisp-config? engines)

  (define nyxt-f-name 'nx-search-engines)
  (define f-name (symbol-append 'nyxt nyxt-f-name))

  (define (get-home-services config)
    "Return home services related to nx-search-engines."
    (list
     (rde-nyxt-configuration-service
      nyxt-f-name
      config
      `((define-configuration buffer
          ((search-always-auto-complete-p nil)
           (search-engines
            (append %slot-value% (list ,@engines))))))
      #:lisp-packages '(nx-search-engines))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-nyxt-nx-router
          #:key
          (media-enabled? #t)
          (routes '()))
  "Configure nx-router, a URL routing extension for Nyxt."
  (ensure-pred boolean? media-enabled?)
  (ensure-pred lisp-config? routes)

  (define nyxt-f-name 'nx-router)
  (define f-name (symbol-append 'nyxt- nyxt-f-name))

  (define (get-home-services config)
    "Return home services related to nx-router."
    (list
     (rde-nyxt-configuration-service
      nyxt-f-name
      config
      `((defun make-invidious-instances ()
          "Return a list of Invidious instances."
          (alex:when-let ((instances
                           (handler-case (dex:get "https://api.invidious.io/instances.json")
                             (usocket:ns-host-not-found-error ()
                               (echo-warning "There's no Internet connection to retrieve instances")
                               nil))))
            (mapcar 'first
                    (json:with-decoder-simple-list-semantics
                      (json:decode-json-from-string instances)))))

        (defun make-scribe-instances ()
          "Return a list of Scribe instances."
          (alex:when-let ((instances
                           (handler-case (dex:get "https://git.sr.ht/~edwardloveall/scribe/blob/main/docs/instances.json")
                             (usocket:ns-host-not-found-error ()
                               (echo-warning "There's no Internet connection to retrieve instances")
                               nil))))
            (json:decode-json-from-string instances)))

        (defun make-teddit-instances ()
          "Return a list of Teddit instances."
          (alex:when-let ((instances
                           (handler-case (dex:get "https://teddit.namazso.eu/instances.json")
                             (usocket:ns-host-not-found-error ()
                               (echo-warning "There's no Internet connection to retrieve instances")
                               nil))))
            (mapcar (lambda (instance)
                      (unless (str:emptyp (alex:assoc-value instance :url))
                        (alex:assoc-value instance :url)))
                    (json:with-decoder-simple-list-semantics
                      (json:decode-json-from-string instances)))))

        (defmethod nyxt:on-signal-load-finished :around ((mode nyxt/history-mode:history-mode) url)
          (call-next-method mode (router:trace-url url)))

        (defmethod nyxt/bookmark-mode:bookmark-current-url :around (&optional (buffer (current-buffer)))
          (setf (url buffer) (router:trace-url (url buffer)))
          (call-next-method buffer))

        (define-configuration router:router-mode
          ((router:enforce-p t)
           (router:media-enabled-p ,(if media-enabled? 't 'nil))
           (router:banner-p t)
           (router:routes (list ,@routes))))
        (define-configuration web-buffer
          ((default-modes `(router:router-mode ,@%slot-value%)))))
      #:lisp-packages '(nx-router))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))