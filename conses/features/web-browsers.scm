(define-module (conses features web-browsers)
  #:use-module (conses utils)
  #:use-module (conses packages emacs-xyz)
  #:use-module (conses packages web-browsers)
  #:use-module (conses home services lisp)
  #:use-module (conses home services web-browsers)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages web-browsers)
  #:use-module (gnu packages browser-extensions)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 ftw)
  #:export (rde-nyxt-configuration-service
            feature-nyxt
            feature-ungoogled-chromium
            feature-proxy))


;;;
;;; rde nyxt utilities.
;;;

(define* (rde-nyxt-configuration-service
          name config
          #:optional (lisp-expressions '())
          #:key
          (lisp-packages '()))
  (let* ((file-name (string-append "configure-" (symbol->string name)))
         (conf-file (mixed-text-file (string-append file-name ".lisp")
                                     (serialize-lisp-config
                                      #f
                                      `((in-package :nyxt-user)
                                        ,@lisp-expressions))))
         (dirname #~(string-append (dirname #$conf-file) "/"))
         (filename #~(basename #$conf-file)))
    (simple-service
     (symbol-append 'nyxt- name '-configurations)
     home-nyxt-service-type
     (home-nyxt-extension
      (config-lisp `(,#~(string-trim-right
                         (with-output-to-string
                           (lambda ()
                             ((@ (ice-9 pretty-print) pretty-print)
                              `(define-nyxt-user-system-and-load
                                 #$(string->symbol (format #f "nyxt-user/~a" name))
                                 #$@(if (null? lisp-packages)
                                        '()
                                        `(:depends-on (,@lisp-packages)))
                                 :config-directory ,#$dirname
                                 :components (,#$filename)))))
                         #\newline)))))))

(define* (feature-nyxt
          #:key
          (nyxt nyxt-next)
          (default-browser? #f)
          (development? #f)
          (startup-flags '())
          (default-cookie-policy ':no-third-party)
          (development-path "~/local/share/projects/nyxt")
          (extra-config-lisp '())
          (auto-mode-rules '())
          (extra-bindings '())
          (default-new-buffer-url "nyxt:new"))
  "Configure the Nyxt browser."
  (ensure-pred file-like? nyxt)
  (ensure-pred boolean? default-browser?)
  (ensure-pred boolean? development?)
  (ensure-pred list? startup-flags)
  (ensure-pred symbol? default-cookie-policy)
  (ensure-pred path? development-path)
  (ensure-pred lisp-config? extra-config-lisp)
  (ensure-pred lisp-config? auto-mode-rules)
  (ensure-pred list? extra-bindings)
  (ensure-pred string? default-new-buffer-url)

  (define f-name 'nyxt)

  (define (get-home-services config)
    "Return home services related to Nyxt."
    (append
     (if default-browser?
         (list
          (simple-service
           'home-nyxt-environment-variables-service
           home-environment-variables-service-type
           `(("BROWSER" . ,(file-append nyxt "/bin/nyxt")))))
         '())
     (if development?
         (list
          (simple-service
           'home-xdg-desktop-entries
           home-xdg-mime-applications-service-type
           (home-xdg-mime-applications-configuration
            (desktop-entries
             (let ((nyxt-file (format #f "~a/build-scripts/nyxt.scm" development-path)))
               (list
                (xdg-desktop-entry
                 (file "nyxt-dev")
                 (name "Nyxt Development")
                 (type 'application)
                 (config
                  `((exec . ,(format #f "guix shell -D -f ~a -- ~a" nyxt-file
                                     (string-append ((compose dirname dirname) nyxt-file) "/nyxt")))
                    (comment . "Be Productive"))))))))))
         '())
     (list
      (simple-service
       'home-nyxt-xdg-mime-applications-service
       home-xdg-mime-applications-service-type
       (home-xdg-mime-applications-configuration
        (default
         '((x-scheme-handler/http . nyxt.desktop)
           (x-scheme-handler/https . nyxt.desktop)
           (x-scheme-handler/about . nyxt.desktop)))))
      (rde-nyxt-configuration-service
       'rde-nyxt
       config
       `(,@(if (get-value 'lisp config)
               '((let ((sbcl-init (merge-pathnames ".sbclrc" (user-homedir-pathname))))
                   (when (probe-file sbcl-init)
                     (load sbcl-init))))
               '())
         (asdf:ensure-source-registry)
         (reset-asdf-registries)
         (use-nyxt-package-nicknames)
         (defvar *rde-keymap* (make-keymap "rde-map"))
         ,@(if (nil? extra-bindings)
               '()
               `((define-key *rde-keymap* ,@extra-bindings)))
         (defmethod files:resolve ((profile nyxt-profile) (file nyxt:history-file))
           "Store history in a temporary directory."
           (sera:path-join (nfiles:expand (make-instance 'nyxt-temporary-directory))
                           (uiop:relativize-pathname-directory (call-next-method))))
         (define-mode rde-keymap-mode ()
           "Dummy mode to apply key bindings in `*rde-keymap*.'"
           ((keyscheme-map (keymaps:make-keyscheme-map keyscheme:emacs *rde-keymap*))
            (visible-in-status-p nil)))
         (define-configuration document-buffer
           ((smooth-scrolling t)
            (scroll-distance 150)))
         (define-configuration web-buffer
           ((download-engine :renderer)))
         (define-configuration buffer
           ((default-modes `(rde-keymap-mode ,@%slot-value%))))
         (define-configuration browser
           ((default-cookie-policy ,default-cookie-policy)
            (restore-session-on-startup-p nil)
            (default-new-buffer-url (quri:uri ,default-new-buffer-url))))))
      (service
       home-nyxt-service-type
       (home-nyxt-configuration
        (nyxt nyxt)
        (auto-mode-rules-lisp auto-mode-rules)))
      (simple-service
       'home-nyxt-extensions
       home-nyxt-service-type
       (home-nyxt-extension
        (config-lisp extra-config-lisp))))))

  (feature
   (name f-name)
   (values `((,f-name . ,nyxt)
             (nyxt-development? . ,development?)
             (nyxt-startup-flags . ,startup-flags)))
   (home-services-getter get-home-services)))

(define* (feature-ungoogled-chromium
          #:key
          (ungoogled-chromium ungoogled-chromium)
          (default-browser? #f)
          (startup-flags '()))
  "Configure the Chromium browser."
  (ensure-pred any-package? ungoogled-chromium)
  (ensure-pred boolean? default-browser?)
  (ensure-pred list? startup-flags)

  (define f-name 'ungoogled-chromium)

  (define (get-home-services config)
    "Return home services related to Ungoogled Chromium."
    (append
     (if default-browser?
         (list
          (simple-service
           'home-chromium-environment-variables-service
           home-environment-variables-service-type
           `(("BROWSER" . ,(file-append ungoogled-chromium "/bin/chromium")))))
         '())
     (list
      (simple-service
       'home-chromium-profile-service
       home-profile-service-type
       (list
        ungoogled-chromium
        ublock-origin/chromium))
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
                          #$(string-join startup-flags)
                          " %U"))
              (terminal . #f)
              (comment . "Access the Internet"))))))))
      (rde-elisp-configuration-service
       f-name
       config
       `((require 'embark)
         (with-eval-after-load 'browse-url
           (setq browse-url-chromium-arguments ',startup-flags))
         ,@(if (get-value 'emacs-embark config)
               `((define-key embark-url-map "c" 'browse-url-chromium))
               '()))
       #:elisp-packages (if (get-value 'emacs-embark config)
                            (list (get-value 'emacs-embark config))
                            '())))
     (if (and (get-value 'nyxt config) (get-value 'emacs config))
         (list
          (rde-nyxt-configuration-service
           f-name
           config
           `((define-command open-with-chromium ()
               "Open the current page with Chromium."
               (eval-in-emacs
                `(browse-url-chromium ,(render-url (url (current-buffer)))))))))
         '())))

  (feature
   (name 'chromium)
   (values `((ungoogled-chromium . ,ungoogled-chromium)))
   (home-services-getter get-home-services)))

(define* (feature-proxy
          #:key
          (youtube-proxy "https://invidious.snopyta.org")
          (reddit-proxy "https://teddit.net")
          (instagram-proxy "https://iganony.com")
          (quora-proxy "https://quora.vern.cc")
          (google-proxy "https://search.sethforprivacy.com")
          (imgur-proxy "https://imgin.voidnet.tech")
          (medium-proxy "https://scribe.rip")
          (twitter-proxy "https://nitter.namazso.eu")
          (tiktok-proxy "https://tok.artemislena.eu"))
  (ensure-pred maybe-string? youtube-proxy)
  (ensure-pred maybe-string? reddit-proxy)
  (ensure-pred maybe-string? instagram-proxy)
  (ensure-pred maybe-string? quora-proxy)
  (ensure-pred maybe-string? google-proxy)
  (ensure-pred maybe-string? imgur-proxy)
  (ensure-pred maybe-string? medium-proxy)
  (ensure-pred maybe-string? twitter-proxy)
  (ensure-pred maybe-string? tiktok-proxy)

  (feature
   (name 'proxy)
   (values (append
            `((proxy . #t))
            (make-feature-values
             youtube-proxy reddit-proxy instagram-proxy quora-proxy
             google-proxy imgur-proxy medium-proxy twitter-proxy tiktok-proxy)))))
