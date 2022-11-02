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
  #:use-module (guix gexp)
  #:export (feature-nyxt-status
            feature-nyxt-emacs
            feature-nyxt-userscript
            feature-nyxt-blocker
            feature-nyxt-nx-tailor
            feature-nyxt-nx-router
            feature-nyxt-nx-search-engines))

(define* (feature-nyxt-status
          #:key
          (height 24)
          (glyphs? #f)
          (status-buffer-layout '()))
  "Configure Nyxt's status buffer and status area."
  (ensure-pred integer? height)
  (ensure-pred boolean? glyphs?)
  (ensure-pred lisp-config? status-buffer-layout)

  (define nyxt-f-name 'status)
  (define f-name (symbol-append 'nyxt- nyxt-f-name))

  (define (get-home-services config)
    "Return home services related to the appearance of status items."
    (list
     (rde-nyxt-configuration-service
      nyxt-f-name
      config
      `((define-class configure-status-buffer (status-buffer) ())
        (defmethod format-close-button ((status configure-status-buffer))
          "Buttons used for the custom status buffer."
          (declare (ignorable status))
          (spinneret:with-html-string
              (:div :id "buttons"
                    (:button :type "button"
                             :title "Close Buffer"
                             :onclick (ps:ps (nyxt/ps:lisp-eval
                                              (:title "close current buffer" :buffer status)
                                              (nyxt:delete-current-buffer)))
                             "🞫"))))
        (defmethod format-status ((status configure-status-buffer))
          (let ((buffer (current-buffer (window status))))
            (spinneret:with-html-string ,@status-buffer-layout)))
        (define-mode configure-status-mode ()
          "Apply a custom status buffer.")
        (defmethod enable ((mode configure-status-mode) &key)
          (setf (status-buffer (current-window))
                (make-instance 'configure-status-buffer)))
        (defmethod disable ((mode configure-status-mode) &key)
          (setf (status-buffer (current-window))
                (make-instance 'configure-buffer)))
        (define-configuration status-buffer
          ((glyph-mode-presentation-p ,(if glyphs? 't 'nil))
           (height ,height)))))))

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

        (define-configuration web-buffer
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
        (autoload 'nyxt-default-keybindings "nyxt")
        (nyxt-default-keybindings))
      #:elisp-packages (list emacs-nyxt))))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             (emacs-nyxt . ,emacs-nyxt)))
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
      `((define-configuration buffer
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

(define (%default-cut config)
  "Build a default cut for nx-tailor with CONFIG."
  (define font-sans (font-name (get-value 'font-sans config)))

  `(define-configuration tailor:cut
     ((tailor:name "rde")
      (tailor:prompt
       '((* :font-family theme:font-family)
         ("#prompt-modes"
          :display "none")
         ("#prompt-area"
          :background-color theme:secondary
          :color theme:on-secondary)
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
          :font-family ,font-sans
          :color (tailor:make-important theme:primary))
         ("p,td,dt,button,a,a:link"
          :font-family ,font-sans
          :color theme:on-background)
         (pre
          :font-family theme:font-family
          :color theme:on-background
          :background-color (tailor:make-important theme:background))
         (".button, .button:hover , .button:visited, .button:active"
          :background-color theme:secondary
          :border-color (if (theme:dark-p theme:theme)
                            theme:on-secondary
                            theme:on-background)
          :color theme:on-background)
         (code
          :font-family theme:font-family
          :color (tailor:make-important theme:on-background)
          :background-color (tailor:make-important theme:secondary))))
      (tailor:status
       '((body
          :font-family theme:font-family
          :height "100%"
          :width "100%"
          :line-height "30px"
          :display "flex"
          :flex-direction "column"
          :background theme:secondary
          :flex-wrap "wrap")
         ("#container"
          :display "flex"
          :height "100%"
          :width "100%"
          :line-height "30px"
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
          :font-family theme:font-family))))))

(define* (feature-nyxt-nx-tailor
          #:key
          (auto-theme? #t)
          (dark-theme? #f)
          (default-cut %default-cut)
          (themes '())
          (main-themes #f))
  "Configure nx-tailor, a Nyxt theme manager."
  (ensure-pred maybe-symbol? auto-theme?)
  (ensure-pred boolean? dark-theme?)
  (ensure-pred procedure? default-cut)
  (ensure-pred lisp-config? themes)
  (ensure-pred maybe-pair-or-string? main-themes)

  (define nyxt-f-name 'nx-tailor)
  (define f-name (symbol-append 'nyxt- nyxt-f-name))

  (define (get-home-services config)
    "Return home services related to nx-tailor."
    (list
     (rde-nyxt-configuration-service
      nyxt-f-name
      config
      `((local-time:reread-timezone-repository)
        (setf local-time:*default-timezone*
              (local-time:find-timezone-by-location-name ,(get-value 'timezone config)))
        ,(default-cut config)
        (define-configuration web-buffer
          ((default-modes `(tailor:tailor-mode ,@%slot-value%))))
        (define-configuration tailor:tailor-mode
          ((tailor:auto-p ,(match auto-theme?
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
          ((search-engines
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

        (define-configuration web-buffer
          ((default-modes `(router:router-mode ,@%slot-value%))))
        (define-configuration router:router-mode
          ((router:enforce-p t)
           (router:media-enabled-p ,(if media-enabled? 't 'nil))
           (router:banner-p t)
           (router:routes (list ,@routes)))))
      #:lisp-packages '(nx-router))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
