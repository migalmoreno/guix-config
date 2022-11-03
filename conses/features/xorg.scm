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
  #:use-module (gnu packages video)
  #:use-module (gnu packages linux)
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
      `((require 'hexrgb)
        (defvar configure-xorg-screencast-process nil
          "The current screencast process.")
        (defun configure-xorg-take-screenshot (&optional region)
          "Take a fullscreen or REGION screenshot of the current display."
          (interactive "P")
          (when-let* ((pictures-dir (xdg-user-dir "PICTURES"))
                      (file-name (replace-regexp-in-string
                                  (rx space) "0" (format-time-string "%Y%m%e-%H%M%S.jpg")))
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

        (defun configure-xorg-record-screencast (&optional region)
          "Record a screencast and if REGION, record a portion of the screen."
          (interactive "P")
          (if-let* ((videos-dir (xdg-user-dir "VIDEOS"))
                    (file-name (format-time-string "%Y%m%e-%H%M%S.mp4"))
                    (screen-region
                     (and region
                          (split-string
                           (shell-command-to-string
                            (substring
                             (concat ,(file-append slop "/bin/slop")
                                     "-q -o -b 3 --format=%x,%y,%h,%w -c "
                                     (mapconcat 'number-to-string (hexrgb-hex-to-rgb "#51afef") ","))
                             0 -1))
                           ",")))
                    (pos-x (nth 0 screen-region))
                    (pos-y (nth 1 screen-region))
                    (height (nth 2 screen-region))
                    (width (nth 3 screen-region)))
              (setq configure-xorg-screencast-process
                    (make-process
                     :name "ffmpeg"
                     :buffer nil
                     :program (list ,(file-append ffmpeg "/bin/ffmpeg")
                                    "-s" (format "%sx%s" width height)
                                    "-show_region" "1" "-f" "x11grab"
                                    "-i" (format ":0.0+%s,%s" pos-x pos-y)
                                    "-framerate" "60" "-c:v" "libx264"
                                    "-preset" "ultrafast" "-crf" "17"
                                    "-pix_fmt" "yuv420p" "-vf"
                                    "pad=\"width=ceil(iw/2)*2:height=ceil(ih/2)*2\""
                                    (concat videos-dir "/" file-name))))
            (let ((videos-dir (xdg-user-dir "VIDEOS"))
                  (file-name (format-time-string "%Y%m%e-%H%M%S.mp4"))
                  (frame-width (x-display-pixel-width))
                  (frame-height (x-display-pixel-height)))
              (notifications-notify :app-name "ffmpeg"
                                    :title "Started recording screen"
                                    :timeout 3000)
              (run-at-time
               2 nil
               (lambda ()
                 (setq configure-xorg-screencast-process
                       (make-process
                        :name "ffmpeg"
                        :buffer nil
                        :program (list ,(file-append ffmpeg "/bin/ffmpeg")
                                       "-f" "x11grab" "-framerate" "60" "-video_size"
                                       (format "%sx%s" frame-width frame-height)
                                       "-i" ":0.0" (concat videos-dir "/" file-name)))))))))

        (defun configure-xorg-stop-screencast ()
          "Stop the current screencast process."
          (interactive)
          (when configure-xorg-screencast-process
            (ignore-errors
              (interrupt-process configure-xorg-screencast-process)))
          (setq configure-xorg-screencast-process nil))

        (defun configure-xorg--toggle-xinput-device (device)
          "Toggle active status of an X input DEVICE through xinput, such as 'Touchpad'."
          (let* ((xinput-bin ,(file-append xinput "/bin/xinput"))
                 (id (with-temp-buffer
                       (call-process xinput-bin)
                       (goto-char (point-min))
                       (re-search-forward (rx (* any) (literal device) (* any) "id=" (group (+ num)) space (* any))
                                          nil t)
                       (match-string 1)))
                 (is-enabled (with-temp-buffer
                               (call-process xinput-bin nil nil nil "list-props" id)
                               (goto-char (point-min))
                               (re-search-forward (rx (* any) "Device Enabled (186):" (+ space) (group num) (* any))
                                                  nil t)
                               (match-string 1))))
            (if (string= is-enabled "1")
                (call-process xinput-bin nil nil nil "disable" id)
              (call-process xinput-bin nil nil nil "enable" id))))

        (defun configure-xorg--toggle-lsusb-device (device)
          "Toggle active status of lsusb DEVICE, such as 'Camera' or 'Webcam'."
          (let* ((bus (with-temp-buffer
                        (call-process ,(file-append usbutils "/bin/lsusb"))
                        (goto-char (point-min))
                        (re-search-forward (rx bol (* any) "ID" space (group (+ alnum) ":" (+ alnum))
                                               (* any) (literal device) (* any))
                                           nil t)
                        (match-string 1)))
                 (product-id (cadr (split-string bus ":")))
                 (devices (directory-files "/sys/bus/usb/devices" nil (rx (* any) num "-" num eol)))
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
          (call-process (executable-find "slock")))

        ,@(if (get-value 'emacs-exwm config)
              '((exwm-input-set-key (kbd "s-p") 'configure-xorg-take-screenshot)
                (exwm-input-set-key (kbd "s-v") 'configure-xorg-record-screencast)
                (exwm-input-set-key (kbd "s-l") 'configure-xorg-call-slock))
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
