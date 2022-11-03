(define-module (conses features xorg)
  #:use-module (conses features gtk)
  #:use-module (conses features fontutils)
  #:use-module (conses packages emacs-xyz)
  #:use-module (conses home services xorg)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu services xorg)
  #:use-module (gnu services base)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages suckless)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home-services xorg)
  #:use-module (gnu home-services base)
  #:use-module (guix gexp)
  #:export (feature-xorg))

(define* (feature-xorg
          #:key
          (xrdb xrdb)
          (unclutter unclutter)
          (unclutter-idle-seconds 5))
  "Configure the Xorg display server."
  (ensure-pred file-like? xrdb)
  (ensure-pred file-like? unclutter)
  (ensure-pred integer? unclutter-idle-seconds)

  (define f-name 'xorg)

  (define (get-home-services config)
    "Return home services related to Xorg."
    (list
     (rde-elisp-configuration-service
      f-name
      config
      `((defun configure-xorg-take-screenshot (&optional region)
          "Take a fullscreen or REGION screenshot of the current display."
          (interactive "P")
          (when-let* ((pictures-dir (xdg-user-dir "PICTURES"))
                      (file-name (replace-regexp-in-string
                                  "[[:space:]]" "0" (format-time-string "%Y%m%e-%H%M%S.jpg")))
                      (maim-bin ,(file-append maim "/bin/maim"))
                      (maim-flags (if region
                                      (list "-s" "-p" "-3" "-c"
                                            (mapconcat 'number-to-string
                                                       (hexrgb-hex-to-rgb "#51afef") ",")
                                            (expand-file-name file-name pictures-dir))
                                    (list (expand-file-name file-name pictures-dir)))))
            (make-process
             :name maim-bin
             :buffer nil
             :command (append (list maim-bin) maim-flags)
             :sentinel (lambda (p _e)
                         (when (= (process-exit-status p) 0)
                           (run-at-time
                            2 nil (lambda ()
                                    (message "Screenshot taken"))))))))

        (defun configure-xorg--toggle-x-input-device (name)
          "Toggle active status of an X input device (NAME can be e.g. 'Touchpad') through xinput."
          (let* ((string (shell-command-to-string "xinput"))
                 (id (progn
                       (string-match (format ".*%s.*id=\\([[:digit:]]+\\)[[:space:]].*" name) string)
                       (match-string 1 string)))
                 (device-props (shell-command-to-string
                                (format "xinput list-props %s" id)))
                 (is-enabled
                  (progn
                    (string-match
                     "\\.*Device Enabled (186):[[:space:]]+\\([[:digit:]]\\).*"
                     device-props)
                    (match-string 1 device-props))))
            (if (string-equal is-enabled "1")
                (start-process-shell-command "xinput" nil (concat "xinput disable " id))
              (start-process-shell-command "xinput" nil (concat "xinput enable " id)))))

        (defun configure-xorg--toggle-lsusb-device (device)
          "Toggle active status of lsusb devices, where DEVICE is a device name such as 'Camera' or 'Webcam'."
          (let* ((buses (shell-command-to-string "lsusb"))
                 (bus (progn
                        (string-match (format "^.*ID[[:space:]]\\([[:alnum:]]+:[[:alnum:]]+\\).*%s.*" device) buses)
                        (match-string 1 buses)))
                 (product-id (cadr (split-string bus ":")))
                 (devices (directory-files "/sys/bus/usb/devices" nil ".*[[:digit:]]-[[:digit:]]$"))
                 (device-mapping
                  (with-temp-buffer
                    (let (value)
                      (dolist (mapping devices value)
                        (insert-file-contents
                         (format "/sys/bus/usb/devices/%s/idProduct" mapping) nil nil nil t)
                        (when (equal (substring (buffer-string) 0 -1) product-id)
                          (setq value mapping)))
                      value)))
                 (is-enabled
                  (with-temp-buffer
                    (insert-file-contents
                     (format "/sys/bus/usb/devices/%s/bConfigurationValue" device-mapping))
                    (substring (buffer-string) 0 -1))))
            (if (string-equal is-enabled "1")
                (write-region "0" nil (format "/sudo::/sys/bus/usb/devices/%s/bConfigurationValue" device-mapping))
                (write-region "1" nil (format "/sudo::/sys/bus/usb/devices/%s/bConfigurationValue" device-mapping)))))

        (defun configure-xorg-call-slock ()
          "Invoke a Slock process."
          (interactive)
          (call-process ,(file-append slock "/bin/slock")))

        ,@(if (get-value 'emacs-exwm config)
              '((exwm-input-set-key (kbd "s-l") 'configure-xorg-call-slock))
              '()))
      #:elisp-packages (list emacs-hexrgb)
      #:summary "Helpers for Xorg"
      #:commentary "Provide various helper functions for useful Xorg-related operations.")
     (simple-service
      'xorg-startup-services
      home-shepherd-service-type
      (list
       (shepherd-service
        (provision '(screensaver))
        (requirement '())
        (one-shot? #t)
        (start #~(lambda ()
                   (invoke #$(file-append xset "/bin/xset")
                           "-dpms" "s" "off"))))
       (shepherd-service
        (provision '(cursor))
        (requirement '())
        (one-shot? #t)
        (start #~(lambda ()
                   (invoke #$(file-append xsetroot "/bin/xsetroot")
                           "-cursor_name" "left_ptr"))))))
     (service home-unclutter-service-type
              (home-unclutter-configuration
               (unclutter unclutter)
               (seconds unclutter-idle-seconds)))
     (service home-xresources-service-type
              (home-xresources-configuration
               (package xrdb)
               (config
                `((Xcursor.theme . ,(and=> (get-value 'gtk-cursor config) icon-theme-name))
                  (Emacs.font . ,(and=> (get-value 'font-sans config) font-name))
                  (Emacs.FontBackend . xft)
                  (Xcursor.size . 16)
                  (Xft.autohint . #t)
                  (Xft.antialias . #t)
                  (Xft.hinting . #t)
                  (Xft.hintstyle . hintfull)
                  (Xft.rgba . none)
                  (Xft.lcdfilter . lcddefault)
                  (Xft.dpi . 110)))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
