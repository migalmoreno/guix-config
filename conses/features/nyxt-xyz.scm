(define-module (conses features nyxt-xyz)
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
            feature-nyxt-nx-tailor
            feature-nyxt-nx-router
            feature-nyxt-nx-search-engines))

(define* (feature-nyxt-status)
  "Configure Nyxt's status buffer and status area."

  (define nyxt-f-name 'status)
  (define f-name (symbol-append 'nyxt- nyxt-f-name))

  (define (get-home-services config)
    "Return home services related to the appearance of status items."
    (list
     (rde-nyxt-configuration-service
      nyxt-f-name
      config
      `((define-class configure-status-buffer (status-buffer) ())
        (defmethod format-status-buttons ((status configure-status-buffer))
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
            (spinneret:with-html-string
                (:div :id "container"
                      (:div :id "controls"
                            (:raw (format-status-buttons status)))
                      (:div :id "url"
                            (:raw
                             (format-status-load-status buffer)
                             (format-status-url buffer)))
                      (:div :id "modes"
                            :title (nyxt::modes-string buffer)
                            (:raw (format-status-modes status)))))))
        (define-mode interactive-status-mode ()
          "Apply interactive buttons to the status buffer.")
        (defmethod enable ((mode interactive-status-mode) &key)
          (setf (status-buffer (current-window))
                (make-instance 'custom-status-buffer)))
        (defmethod disable ((mode interactive-status-mode) &key)
          (setf (status-buffer (current-window))
                (make-instance 'status-buffer)))
        (define-configuration status-buffer
          ((glyph-mode-presentation-p t)
           (height 30)))))))

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

        (define-configuration buffer
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

(define* (feature-nyxt-nx-tailor
          #:key
          (auto-theme? #t)
          (dark-theme? #f)
          (default-cut '())
          (themes '())
          (main-themes #f))
  "Configure nx-tailor, a Nyxt theme manager."
  (ensure-pred maybe-symbol? auto-theme?)
  (ensure-pred lisp-config? default-cut)
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
      `(,default-cut
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
          (mapcar
           'first
           (json:with-decoder-simple-list-semantics
               (json:decode-json-from-string
                (dex:get "https://api.invidious.io/instances.json")))))

        (defun make-scribe-instances ()
          "Return a list of Scribe instances."
          (json:decode-json-from-string
           (dex:get
            "https://git.sr.ht/~edwardloveall/scribe/blob/main/docs/instances.json")))

        (defun make-teddit-instances ()
          "Return a list of Teddit instances."
          (mapcar (lambda (instance)
                    (unless (str:emptyp (alex:assoc-value instance :url))
                      (alex:assoc-value instance :url)))
                  (json:with-decoder-simple-list-semantics
                      (json:decode-json-from-string
                       (dex:get "https://teddit.net/instances.json")))))

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
