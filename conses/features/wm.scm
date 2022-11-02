(define-module (conses features wm)
  #:use-module (conses utils)
  #:use-module (conses packages emacs-xyz)
  #:use-module (conses home services xorg)
  #:use-module (conses home services linux)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu services)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu system keyboard)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:export (feature-emacs-exwm))

(define %xorg-libinput-configuration
  "Section \"InputClass\"
  Identifier \"Touchpads\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsTouchpad \"on\"

  Option \"Tapping\" \"on\"
  Option \"TappingDrag\" \"on\"
  Option \"DisableWhileTyping\" \"on\"
  Option \"MiddleEmulation\" \"on\"
  Option \"ScrollMethdod\" \"twofinger\"
EndSection
Section \"InputClass\"
  Identifier \"Keyboards\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsKeyboard \"on\"
EndSection")

(define* (feature-emacs-exwm
          #:key
          (emacs-exwm emacs-exwm)
          (workspaces 5)
          (autostart? #t)
          (window-configuration '()))
  "Configure the Emacs X Window Manager."
  (ensure-pred file-like? emacs-exwm)
  (ensure-pred number? workspaces)
  (ensure-pred boolean? autostart?)

  (define emacs-f-name 'exwm)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to EXWM."
    (require-value 'keyboard-layout config)
    (require-value 'emacs config)

    (define layout (get-value 'keyboard-layout config))
    (define emacs (get-value 'emacs config))

    (append
     (if autostart?
         (list
          (simple-service
           'home-exwm-bash-service
           home-bash-service-type
           (home-bash-extension
            (bash-profile
             (list
              (mixed-text-file
               "bash-profile"
               #~(string-append
                  "[ $(tty) = /dev/tty1 ] && exec "
                  #$(program-file
                     "exwm-start"
                     (xorg-start-command
                      (xinitrc #:wm emacs)
                      (xorg-configuration
                       (keyboard-layout layout)
                       (extra-config
                        (list
                         %xorg-libinput-configuration))))))))))))
         '())
     (list
      (simple-service
       'home-exwm-profile-service
       home-profile-service-type
       (list xrandr))
      (rde-elisp-configuration-service
       emacs-f-name
       config
       `((require 'cl-lib)
         (defvar configure-exwm-default-output nil
           "The name of the default RandR output.")

         (defvar configure-exwm-docked-output nil
           "The name of the external display RandR output.")

         ,@(if (get-value 'emacs-consult-initial-narrowing? config)
               `((defvar configure-exwm-buffer-source
                   `(:name "EXWM"
                           :hidden t
                           :narrow ?x
                           :category buffer
                           :state ,'consult--buffer-state
                           :items ,(lambda () (mapcar 'buffer-name (configure-completion--mode-buffers
                                                                    'exwm-mode))))
                   "Source for EXWM buffers to be set in `consult-buffer-sources'.")
                 (add-to-list 'consult-buffer-sources configure-exwm-buffer-source)
                 (add-to-list 'configure-completion-initial-narrow-alist '(exwm-mode . ?x)))
               '())

         (defun configure-exwm--disable-tab-bar (frame)
           "Disable the tab-bar on new Emacs FRAME."
           (set-frame-parameter frame 'tab-bar-lines 0))

         (defun configure-exwm--docked-p ()
           "Return non-nil if RanR has currently more than one output connected."
           (let ((xrandr-output-regexp (rx "\n" bol (group (+ any)) " connected")))
             (with-temp-buffer
               (call-process ,(file-append xrandr "/bin/xrandr") nil t nil)
               (goto-char (point-min))
               (re-search-forward xrandr-output-regexp nil 'noerror)
               (setq configure-exwm-default-output (match-string 1))
               (forward-line)
               (prog1
                   (null (not (re-search-forward xrandr-output-regexp nil 'noerror)))
                 (setq configure-exwm-docked-output (match-string 1))))))

         (defun configure-exwm-apply-display-settings ()
           "Apply the corresponding display settings after EXWM is enabled."
           (interactive)
           ,@(if (get-value 'emacs-fontaine config)
                 '((require 'fontaine)
                   (if (configure-exwm--docked-p)
                       (fontaine-set-preset 'docked)
                     (fontaine-set-preset 'headless)))
               '())
           (cl-loop for i from 0 upto ,workspaces
                    do (progn
                         (exwm-workspace-switch-create i)
                         (set-frame-parameter (selected-frame) 'internal-border-width
                                              ,(get-value 'emacs-margin config)))
                    finally (progn
                              (exwm-workspace-switch-create 0)
                              (add-hook 'after-make-frame-functions 'configure-exwm--disable-tab-bar))))

         (defun configure-exwm-shorten-buffer-name ()
           "Shorten EXWM buffer names to be more discernible."
           (interactive)
           (exwm-workspace-rename-buffer
            (concat exwm-class-name ": "
                    (if (<= (length exwm-title) 30)
                        exwm-title
                      (concat (substring exwm-title 0 29) "...")))))

         (defun configure-exwm-set-window-by-class ()
           "Apply custom window configuration to EXWM windows."
           (interactive)
           (pcase exwm-class-name
             ,@window-configuration))

         (defun configure-exwm--invoke-xrandr (&rest args)
           "Call `xrandr' with the supplied ARGS."
           (let ((process (apply 'start-process "xrandr" nil ,(file-append xrandr "/bin/xrandr") args)))
             (set-process-sentinel
              process
              (lambda (_p e)
                (when (string= e "finished\n")
                  (add-hook 'exwm-randr-screen-change-hook 'configure-exwm-update-output))))))

         (defun configure-exwm-get-resolution ()
           "Prompt the user for a list of available resolutions."
           (interactive)
           (with-temp-buffer
             (call-process ,(file-append xrandr "/bin/xrandr") nil t nil)
             (goto-char (point-min))
             (unless (re-search-forward
                      (rx "\n" bol (+ any) " connected primary")
                      nil 'noerror)
               (goto-char (point-min))
               (re-search-forward (rx bol (+ any) "connected") nil 'noerror))
             (let ((resolutions
                    (cl-loop while (not (eobp))
                             do (forward-line 1)
                             when (re-search-forward
                                   (rx (+ blank) (group (+ num) "x" (+ num)) (+ blank) (+ num))
                                   nil 'noerror)
                             collect (match-string 1))))
               (completing-read "Select resolution: "
                                (lambda (string pred action)
                                  (if (eq action 'metadata)
                                      `(metadata
                                        ,(cons 'display-sort-function 'identity))
                                    (complete-with-action action resolutions string pred)))))))

         (defun configure-exwm-update-output (&optional change-res-p)
           "Update RandR output configuration, and if CHANGE-RES-P change the resolution of the primary output."
           (interactive "P")
           (when change-res-p
             (remove-hook 'exwm-randr-screen-change-hook 'configure-exwm-update-output))
           (let ((resolution (and change-res-p (configure-exwm-get-resolution)))
                 (xrandr-monitor-regexp "\n .* \\([^ \n]+\\)"))
             (if (not (configure-exwm--docked-p))
                 (progn
                   (apply 'configure-exwm--invoke-xrandr `("--output" ,configure-exwm-default-output
                                                           ,@(if resolution
                                                                 `("--mode" ,resolution)
                                                               '("--auto"))))
                   (with-temp-buffer
                     (call-process ,(file-append xrandr "/bin/xrandr") nil t nil "--listactivemonitors")
                     (goto-char (point-min))
                     (while (not (eobp))
                       (when (and (re-search-forward xrandr-monitor-regexp nil 'noerror)
                                  (not (string= (match-string 1) configure-exwm-default-output)))
                         (call-process ,(file-append xrandr "/bin/xrandr")
                                       nil nil nil "--output" (match-string 1) "--auto")))))
               (apply 'configure-exwm--invoke-xrandr `("--output" ,configure-exwm-docked-output "--primary"
                                                       ,@(if resolution
                                                             `("--mode" ,resolution)
                                                           '("--auto"))
                                                       "--output" ,configure-exwm-default-output "--off"))
               (setq exwm-randr-workspace-monitor-plist (list 0 configure-exwm-docked-output)))))

         (defun configure-exwm-take-screenshot (&optional region)
           "Take a fullscreen or REGION screenshot of the current display."
           (interactive "P")
           (when-let* ((pictures-dir (xdg-user-dir "PICTURES"))
                       (file-name (replace-regexp-in-string
                                   "[[:space:]]" "0" (format-time-string "%Y%m%e-%H%M%S.jpg")))
                       (maim-bin (executable-find "maim"))
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

         (defun configure-exwm--toggle-x-input-device (name)
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

         (defun configure-exwm--toggle-lsusb-device (device)
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

         (setq exwm-workspace-number ,workspaces)
         ,#~"
(setq exwm-input-global-keys
      `((,(kbd \"s-s\") . (lambda () (interactive) (exwm-layout-shrink-window-horizontally 40)))
        (,(kbd \"s-e\") . (lambda () (interactive) (exwm-layout-enlarge-window-horizontally 40)))
        (,(kbd \"s-C-e\") . (lambda () (interactive) (exwm-layout-enlarge-window 40)))
        (,(kbd \"s-C-s\") . (lambda () (interactive) (exwm-layout-shrink-window 40)))
        (,(kbd \"s-m\") . exwm-workspace-move-window)
        (,(kbd \"s-C-r\") . exwm-reset)
        (,(kbd \"<XF86AudioPause>\") . nil)
        (,(kbd \"<XF86AudioPlay>\") . nil)
        (,(kbd \"s-W\") . exwm-workspace-switch)
        (,(kbd \"s-t\") . exwm-floating-toggle-floating)
        (,(kbd \"s-l\") . (lambda () (interactive) (call-process (executable-find \"slock\"))))
        (,(kbd \"s-F\") . exwm-layout-toggle-fullscreen)
        (,(kbd \"C-q\") . (lambda () (interactive) (call-interactively 'exwm-input-send-next-key)))
        (,(kbd \"s-q\") . kill-this-buffer)
        ,@(mapcar (lambda (i)
                    `(,(kbd (format \"s-%d\" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch ,i))))
                (number-sequence 0 exwm-workspace-number))
        (,(kbd \"s-`\") . (lambda () (interactive) (exwm-workspace-switch 0)))))"
         ,@(if (string= (package-name (get-value 'default-application-launcher? config))
                        "app-launcher")
               '((exwm-input-set-key (kbd "s-SPC") 'app-launcher-run-app))
               '())
         (setq exwm-workspace-show-all-buffers nil)
         (setq exwm-layout-show-all-buffers nil)
         (setq exwm-workspace-minibuffer-position nil)
         (setq exwm-workspace-wrap-cursor t)
         (setq exwm-floating-border-color "#f0f0f0")
         (setq exwm-floating-border-width 2)
         (setq exwm-workspace-switch-create-limit 5)
         (add-hook 'exwm-init-hook 'configure-exwm-apply-display-settings)
         (add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
         (add-hook 'exwm-manage-finish-hook 'configure-exwm-set-window-by-class)
         (add-hook 'exwm-update-class-hook 'configure-exwm-shorten-buffer-name)
         (add-hook 'exwm-update-title-hook 'configure-exwm-shorten-buffer-name)
         (autoload 'exwm-enable "exwm")
         (exwm-enable)
         ,#~"
(with-eval-after-load 'exwm
  (setq exwm-input-simulation-keys
        `(([?\\C-b] . [left])
         ([?\\C-f] . [right])
         ([?\\C-p] . [up])
         ([?\\C-n] . [down])
         ([?\\C-a] . [home])
         ([?\\C-e] . [end])
         ([?\\M-v] . [prior])
         ([?\\C-v] . [next])
         ([?\\C-d] . [delete])
         ([?\\C-k] . [S-end delete])))
  (require 'exwm-randr)
  (setq exwm-input-prefix-keys
        (append exwm-input-prefix-keys '(?\\M-o ?\\M-s ?\\s-e)))
  (exwm-randr-enable)
  (add-hook 'after-init-hook 'configure-exwm-update-output))")
       #:elisp-packages (append
                         (list emacs-exwm
                               emacs-hexrgb)
                         (if (get-value 'emacs-fontaine config)
                             (list (get-value 'emacs-fontaine config))
                             '()))
       #:summary "Utilities for EXWM"
       #:commentary "Utilities for EXWM, the Emacs X Window Manager."))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-exwm)))
   (home-services-getter get-home-services)))
