(define-module (migalmoreno users vega web-browsers)
  #:use-module (migalmoreno utils)
  #:use-module ((migalmoreno presets web-browsers) #:prefix presets:)
  #:use-module (rde features)
  #:use-module (rde features web)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features nyxt-xyz)
  #:use-module (rde features web-browsers)
  #:use-module (rde home services emacs)
  #:use-module (sxml simple))

(define %extra-nyxt-config-lisp
  `(,@presets:extra-nyxt-config-lisp
    (define-configuration nyxt/mode/reduce-tracking:reduce-tracking-mode
      ((nyxt/mode/reduce-tracking:preferred-user-agent
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36
 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36")))

    (defmethod format-status-buttons :around ((status status-buffer))
      (spinneret:with-html-string
        (:nbutton
          :buffer status
          :text (:raw (glyph-left status))
          :title "Backwards"
          '(nyxt/mode/history:history-backwards))
        (:nbutton
          :buffer status
          :text (:raw (glyph-reload status))
          :title "Reload"
          '(nyxt:reload-current-buffer))
        (:nbutton
          :buffer status
          :text (:raw (glyph-right status))
          :title "Forwards"
          '(nyxt/mode/history:history-forwards))
        (:nbutton
          :buffer status
          :text (:raw ,(call-with-output-string
                        (lambda (port)
                          (sxml->xml
                           '(svg (@ (xmlns "http://www.w3.org/2000/svg")
                                    (width "5")
                                    (height "5")
                                    (viewBox "0 0 5 5")
                                    (overflow "visible")
                                    (stroke "currentColor")
                                    (stroke-width "1.2"))
                                 (line (@ (x2 "5") (y2 "5")))
                                 (line (@ (x1 "5") (y2 "5"))))
                           port))))
          :title "Close"
          '(nyxt:delete-current-buffer))))

    (defmethod format-status :around ((status status-buffer))
      (let ((buffer (current-buffer (window status))))
        (spinneret:with-html-string
          (:div :id "container"
                (:div :id "controls"
                      (:raw (format-status-buttons status)))
                (:div :id "url"
                      (:raw
                       (format-status-load-status status)
                       (format-status-url status)))
                (:div :id "modes"
                      :title (nyxt::modes-string buffer)
                      (:raw
                       (format-status-modes status)))))))

    (define-configuration nyxt/mode/blocker:blocker-mode
      ((glyph "⨂")))

    (define-configuration nyxt/mode/user-script:user-script-mode
      ((glyph "★")))

    (define-configuration prompt-buffer
      ((mouse-support-p nil)))))

(define (extra-emacs-nyxt-config config)
  (define nyxt-theme ((get-value 'nyxt-theme config) config))
  (home-elisp-extension
   (config
    `((defun rde-nyxt-load-theme (&optional theme)
        "Load Nyxt theme according to current theme or THEME."
        (interactive)
        (nyxt-load-theme
         '(make-instance 'tailor:user-theme
                         :name ',(nyxt-theme-name nyxt-theme)
                         ,@(nyxt-theme-palette nyxt-theme))))
      ,@(if (get-value 'emacs-modus-themes config)
            `((add-hook 'rde-modus-themes-after-enable-theme-hook
                        'rde-nyxt-load-theme))
            '())))))

(define-public (nyxt-features source palette)
  (append
   (list
    (feature-nyxt
     #:default-new-buffer-url "nyxt:nx-mosaic:mosaic"
     #:scroll-distance 150
     #:temporary-history? #t
     #:autostart-slynk? #t
     #:default-browser? #t
     #:restore-session? #f
     #:extra-config-lisp %extra-nyxt-config-lisp)
    (feature-emacs-nyxt
     #:autostart-delay 5)
    (feature-home-extension
     #:name 'emacs-nyxt-extra-config
     #:service 'emacs-rde-nyxt-service-type
     #:extension extra-emacs-nyxt-config)
    (feature-alternative-frontends
     #:frontends `((google . "http://localhost:5000")
                   (youtube . ,(string-append "https://" %tubo-host))
                   (soundcloud . ,(string-append "https://" %tubo-host))
                   (reddit . "https://redlib.freedit.eu"))))
   (presets:nyxt-features source palette)))
