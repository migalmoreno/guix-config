;; -*- lexical-binding: t; -*-
(require 'hexrgb)
(require 'xdg)
(require 'ednc)

(defgroup eb-desktop nil
  "Desktop-related packages and customizations."
  :group 'eb)

(defcustom eb-desktop-audio-executable (executable-find "pamixer")
  "The audio server executable to be used with audio-related functions."
  :group 'eb-desktop
  :type 'string)

(defcustom eb-desktop-audio-step 5
  "The audio level stepping to be used for volume changes."
  :group 'eb-desktop
  :type 'integer)

(defcustom eb-desktop-display-weather-interval (* 60 60)
  "Seconds between updates of weather in the mode line."
  :group 'eb-desktop
  :type 'integer)

(defvar eb-desktop-screencast-process nil
  "Holds the current screencast process.")

(defvar eb-desktop-display-weather-string nil
  "Current weather string for mode line.")

(defvar eb-desktop-display-volume-string nil
  "Current volume level string for the mode line.")

(defvar eb-desktop-display-weather-timer nil
  "Timer object for weather generating function.")

(defvar eb-desktop-current-geolocation nil
  "Current position coordinates.")

(defun eb-desktop-take-screenshot (&optional region)
  "Takes a fullscreen or REGION screenshot of the current display."
  (interactive "P")
  (when-let* ((pictures-dir (xdg-user-dir "PICTURES"))
              (file-name (replace-regexp-in-string
                          "[[:space:]]" "0" (format-time-string "%Y%m%e-%H%M%S.jpg")))
              (maim-bin (executable-find "maim"))
              (maim-flags (if region
                              (list "-s" "-p" "-3" "-c"
                                    (mapconcat #'number-to-string
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

;; TODO: fix
(defun eb-desktop-record-screencast (&optional region)
  "Records a desktop screencast. If REGION, records portion of the screen."
  (interactive "P")
  (if-let* ((videos-dir (xdg-user-dir "VIDEOS"))
            (file-name (format-time-string "%Y%m%e-%H%M%S.mp4"))
            (ffmpeg-bin (executable-find "ffmpeg"))
            (screen-region
             (and region
                  (split-string
                   (shell-command-to-string
                    (substring
                     (concat "slop -q -o -b 3 --format=%x,%y,%h,%w -c "
                             (mapconcat #'number-to-string (hexrgb-hex-to-rgb "#51afef") ","))
                     0 -1))
                   ",")))
            (pos-x (nth 0 screen-region))
            (pos-y (nth 1 screen-region))
            (height (nth 2 screen-region))
            (width (nth 3 screen-region)))
      (setq eb-desktop-screencast-process
            (make-process
             :name "ffmpeg"
             :buffer nil
             :program (list ffmpeg-bin
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
      (notifications-notify :title "ffmpeg"
                            :body "Started recording screen"
                            :timeout 3000)
      (run-at-time
       2 nil
       (lambda ()
         (setq eb-desktop-screencast-process
               (make-process
                :name "ffmpeg"
                :buffer nil
                :program (list (executable-find "ffmpeg")
                               "-f" "x11grab" "-framerate" "60" "-video_size"
                               (format "%sx%s" frame-width frame-height)
                               "-i" ":0.0" (concat videos-dir "/" file-name)))))))))

(defun eb-desktop-stop-screencast ()
  (interactive)
  (when eb-desktop-screencast-process
    (ignore-errors
      (interrupt-process eb-desktop-screencast-process)))
  (setq eb-desktop-screencast-process nil))

(defun eb-desktop-audio--execute (&rest flags)
  "Execute audio-server command `eb-desktop-audio-executable' with provided FLAGS."
  (apply #'call-process
   eb-desktop-audio-executable nil 0 nil
   (mapcar (lambda (e)
             (cl-typecase e
               (integer (number-to-string e))
               (null "")
               (t e)))
           flags)))

(defun eb-desktop-audio--read (&rest flags)
  "Read from output of command `eb-desktop-audio-executable' with provided FLAGS."
  (shell-command-to-string
   (concat eb-desktop-audio-executable " "
           (mapconcat #'identity flags " "))))

(defun eb-desktop--mute-p (&optional source)
  "Return audio device mute status."
  (string-equal
   (substring
    (if source
        (eb-desktop-audio--read "--default-source" "--get-mute")
      (eb-desktop-audio--read "--get-mute"))
    0 -1)
   "true"))

(cl-defun eb-desktop-display-volume-update (&key source)
  "Update the `eb-desktop-display-volume' information with the default sink
 device volume level, and if SOURCE, the default source device volume level."
  (cl-flet ((compute-volume (&optional source)
                            (string-to-number
                             (substring
                              (if source
                                  (eb-desktop-audio--read "--default-source" "--get-volume")
                                (eb-desktop-audio--read "--get-volume"))
                              0 -1))))
    (prog1
        (setq eb-desktop-display-volume-string
              (concat
               (when source
                 (format " %s %02d%% " (eb-look--position-item
                                        (if (eb-desktop--mute-p t) "" ""))
                         (compute-volume t)))
               (format "%s %02d%% " (eb-look--position-item
                                     (if (eb-desktop--mute-p) "" ""))
                       (compute-volume))))
      (force-mode-line-update t))))

;;;###autoload
(cl-defun eb-desktop-audio-change-volume (&key (decrease nil)
                                               (step eb-desktop-audio-step)
                                               (source nil) (mute nil))
  "Change the default sink device volume level by STEP. If specified,
 DECREASE it, apply it to the default SOURCE device or MUTE the device."
  (interactive
   (when current-prefix-arg
     (list
      :step current-prefix-arg)))
  (cond
   ((and source (not mute))
    (if decrease
        (eb-desktop-audio--execute "--default-source" "-d" step)
      (eb-desktop-audio--execute "--default-source" "-i" step)))
   ((and source mute)
    (eb-desktop-audio--execute "--default-source" "-t")
    (eb-desktop--mute-p))
   (mute (eb-desktop-audio--execute "-t")
         (eb-desktop--mute-p))
   (t
    (if decrease
        (eb-desktop-audio--execute "-d" step)
      (eb-desktop-audio--execute "-i" step))))
  (eb-desktop-display-volume-update :source t))

(defun eb-desktop-change-brightness (&optional decrease)
  "Change monitor/laptop screen brightness through light and ddcutil."
  (interactive "P")
  (let* ((headless-p (length (process-lines "xrandr" "--listmonitors"))))
    (if (>= 1 headless-p)
        (if decrease
            (start-process-shell-command "light" nil "light -U 5")
          (start-process-shell-command "light" nil "light -A 5"))
      (if decrease
          (start-process-shell-command "light" nil "light -s sysfs/backlight/ddcci1 -U 5")
        (start-process-shell-command "light" nil "light -s sysfs/backlight/ddcci1 -A 5")))))

(defun eb-desktop--get-brightness ()
  "Get display brigthness using light."
  (let* ((display (substring (shell-command-to-string "autorandr --current") 0 -1))
         (brightness
          (round
           (string-to-number
            (substring
             (if (string-equal display "standalone")
                 (shell-command-to-string "light -G")
               (shell-command-to-string "light -s sysfs/backlight/ddcci1 -G"))
             0 -1)))))
    brightness))

(defun eb-desktop--toggle-x-input-device (name)
  "Toggle the enabled status of an X input device through xinput.
NAME is a device name such as 'Touchpad' or 'TrackPoint'"
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

(defun eb-desktop--toggle-lsusb-device (device)
  "Toggle enabled status of lsusb devices.
DEVICE is a device name such as 'Camera' or 'Webcam'."
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

(defun eb-desktop--get-geolocation ()
  "Fetches the current location's coordinates."
  (with-current-buffer
      (url-retrieve-synchronously
       "https://location.services.mozilla.com/v1/geolocate?key=geoclue" t)
    (goto-char (point-min))
    (re-search-forward (rx (: bol "\n")) nil t)
    (delete-region (point) (point-min))
    (let* ((location (car (cdr (json-parse-string (buffer-string) :object-type 'plist))))
           (latitude (plist-get location :lat))
           (longitude (plist-get location :lng)))
      (setq eb-desktop-current-geolocation (cons longitude latitude)))))

(defun eb-desktop-display-weather-update ()
  "Fetches weather in current location using `https://wttr.in'."
  (interactive)
  (let ((coordinates (or eb-desktop-current-geolocation
                         (eb-desktop--get-geolocation))))
    (url-retrieve
     (format "https://v2d.wttr.in/~%s,%s?format=%%c%%t\n"
             (cdr coordinates) (car coordinates))
     (lambda (_status)
       (buffer-string)
       (goto-char (point-min))
       (re-search-forward (rx (: bol "\n")) nil t)
       (delete-region (point) (point-min))
       (let ((weather (string-join (mapcar (lambda (s)
                                             (string-trim s (rx (or "+" "-"))))
                                           (split-string
                                            (decode-coding-string (buffer-string) 'utf-8)))
                                   " ")))
         (if (string-match-p (rx (: bol "Unknown" (+ any))) weather)
             (setq eb-desktop-display-weather-string nil)
           (setq eb-desktop-display-weather-string weather))
         (force-mode-line-update t)))
     nil t)))

(defun eb-desktop--notify ()
  "Display the latest EDNC notification."
  (when (ednc-notifications)
    (ednc-format-notification (car (ednc-notifications)))))

;;;###autoload
(defun eb-desktop-show-notification-log ()
  "Switch to EDNC log buffer."
  (interactive)
  (when (bufferp (get-buffer ednc-log-name))
    (switch-to-buffer ednc-log-name)))

;;;###autoload
(defun eb-desktop-close-last-notification ()
  "Close the latest notification provided by the notification daemon."
  (interactive)
  (when-let* ((notification (car (ednc-notifications))))
    (ednc--close-notification notification 2)))

;;;###autoload
(defun eb-desktop-invoke-power-menu ()
  "Prompt the user for power menu options."
  (interactive)
  (let ((power-option (completing-read "System operation: " '("Power off" "Halt" "Restart")))
        (default-directory (format "/sudo::%s" temporary-file-directory)))
    (when (exwm--confirm-kill-emacs "Kill EXWM, Emacs, and execute system action?")
      (kill-emacs)
      (pcase power-option
        ("Power off" (async-shell-command (executable-find "shutdown")))
        ("Halt" (async-shell-command (executable-find "halt")))
        ("Restart" (async-shell-command (executable-find "reboot")))))))

;;;###autoload
(defun eb-desktop-close-all-notifications ()
  "Dismisses all EDNC notifications."
  (interactive)
  (mapc #'ednc-dismiss-notification (cdr ednc--state)))

;;;###autoload
(defun eb-desktop-update-notifications (&rest _)
  "Updates the display of EDNC notifications."
  (interactive)
  (force-mode-line-update t))

;;;###autoload
(define-minor-mode eb-desktop-display-weather-mode
  "Mode that shows the current weather in the mode line."
  :global t :group 'eb-desktop
  (when eb-desktop-display-weather-timer
    (cancel-timer eb-desktop-display-weather-timer))
  (setq eb-desktop-display-weather-timer nil
        eb-desktop-display-weather-string nil)
  (when eb-desktop-display-weather-mode
    (eb-desktop-display-weather-update)
    (setq eb-desktop-display-weather-timer
          (run-at-time t eb-desktop-display-weather-interval
                       #'eb-desktop-display-weather-update))))

;;;###autoload
(define-minor-mode eb-desktop-display-volume-mode
  "Mode that shows the current volume level in the mode line."
  :global t :group 'eb-desktop
  (setq eb-desktop-display-volume-string nil)
  (when eb-desktop-display-volume-mode
    (eb-desktop-display-volume-update :source t)))

(provide 'eb-desktop)
