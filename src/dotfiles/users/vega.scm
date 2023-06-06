(define-module (dotfiles users vega)
  #:use-module (dotfiles common)
  #:use-module (dotfiles utils)
  #:use-module (rde features)
  #:use-module (rde features android)
  #:use-module (rde features base)
  #:use-module (rde features documentation)
  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features fontutils)
  #:use-module (rde features keyboard)
  #:use-module (rde features linux)
  #:use-module (rde features gnupg)
  #:use-module (rde features gtk)
  #:use-module (rde features networking)
  #:use-module (rde features nyxt-xyz)
  #:use-module (rde features ssh)
  #:use-module (rde features video)
  #:use-module (rde features virtualization)
  #:use-module (rde features web)
  #:use-module (rde features web-browsers)
  #:use-module (rde home services desktop)
  #:use-module (rde packages)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu home-services xorg)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services xdg)
  #:use-module (gnu system keyboard)
  #:use-module (guix gexp))


;;; Service extensions

(define extra-shell-envs-service
  (simple-service
   'add-missing-shell-envs
   home-environment-variables-service-type
   '(("GPG_TTY" . "$(tty)")
     ("LESSHISTFILE" . "-")
     ("npm_config_userconfig" . "$XDG_CONFIG_HOME/npm-config")
     ("npm_config_cache" . "$XDG_CACHE_HOME/npm")
     ("npm_config_prefix" . "$XDG_DATA_HOME/npm/bin")
     ("PATH" . "$XDG_DATA_HOME/npm/bin:$PATH"))))

(define extra-home-packages-service
  (simple-service
   'add-extra-home-packages
   home-profile-service-type
   (strings->packages
    "ddcutil" "light" "xclip"
    "nasm" "gcc-toolchain" "autoconf"
    "v4l-utils" "binutils" "wireguard-tools"
    "texinfo" "pass-otp" "imagemagick"
    "setxkbmap" "xkeyboard-config" "ffmpeg" "slop")))

(define extra-ssh-config
  (home-ssh-configuration
   (extra-config
    (list
     (ssh-host
      (host "cygnus")
      (options
       `((host-name . ,(getenv "CYGNUS_HOST"))
         (user . "root"))))
     (ssh-host
      (host "deneb")
      (options
       `((host-name . ,(getenv "CYGNUS_HOST"))
         (user . "deneb"))))
     (ssh-host
      (host "hydri")
      (options
       `((host-name . "192.168.0.29")
         (user . "hydri"))))))))

(define extra-xdg-desktop-entries
  (simple-service
   'add-extra-desktop-entries
   home-xdg-mime-applications-service-type
   (home-xdg-mime-applications-configuration
    (desktop-entries
     (list
      (xdg-desktop-entry
       (file "emulator")
       (name "Emulator")
       (type 'application)
       (config
        `((exec . ,#~(string-join
                      (list
                       #$(file-append
                          (@ (gnu packages package-management) guix)
                          "/bin/guix")
                       "time-machine" "-C"
                       #$(project-file "rde/channels-lock.scm") "--"
                       "shell" "-C" "-N" "--emulate-fhs"
                       "--share=/tmp/.X11-unix" "--share=/dev/shm"
                       "--expose=/etc/machine-id" "--share=$HOME"
                       "--preserve='^ANDROID'" "--preserve='^DISPLAY$'"
                       "--preserve='^XAUTHORITY$'" "--share=/dev/kvm"
                       "--share=/dev/video0" "--share=/dev/dri"
                       "-m" #$(project-file
                               "src/dotfiles/manifests/android.scm")
                       "--" "env"
                       #$(string-append "LD_LIBRARY_PATH="
                                        "/lib:/lib/nss:"
                                        "~/.android/emulator/lib64/qt/lib:"
                                        "~/.android/emulator/lib64")
                       "~/.android/emulator/emulator" "-show-kernel" "-gpu"
                       "swiftshader_indirect" "-camera-back" "webcam0"
                       "-avd" "whatsapp_bridge")))
          (icon . "android")
          (comment . "Run an Android emulator")))))))))

(define extra-init-el
  `(,@%base-extra-init-el
    (with-eval-after-load 'password-cache
      (setq password-cache t)
      (setq password-cache-expiry (* 60 10)))
    (with-eval-after-load 'pass
      (setq pass-show-keybindings nil))
    (with-eval-after-load 'epg-config
      (setq epg-pinentry-mode 'loopback))
    (with-eval-after-load 'password-store
      (setq password-store-time-before-clipboard-restore 60))
    (with-eval-after-load 'frame
      (add-to-list 'initial-frame-alist '(fullscreen . maximized)))
    (with-eval-after-load 'prog-mode
      (setq prettify-symbols-unprettify-at-point 'right-edge)
      (setq-default prettify-symbols-alist
                    '((":LOGBOOK:" . "")
                      (":PROPERTIES:" . "")
                      ("# -*-" . "")
                      ("-*-" . ""))))
    (with-eval-after-load 'rde-completion
      (add-to-list 'rde-completion-initial-narrow-alist
                   '(cider-repl-mode . ?c)))
    (with-eval-after-load 'face-remap
      (setq text-scale-mode-step 1.075))
    (setq-default tab-width 2)
    (with-eval-after-load 'indent
      (setq tab-always-indent 'complete))
    (add-hook 'before-save-hook 'delete-trailing-whitespace)
    (with-eval-after-load 'mwheel
      (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)
                                          ((control) . 1)))
      (setq mouse-wheel-progressive-speed nil)
      (setq mouse-wheel-follow-mouse t)
      (setq scroll-conservatively 100)
      (setq mouse-autoselect-window nil)
      (setq what-cursor-show-names t)
      (setq focus-follows-mouse t))
    (winner-mode)
    (define-key ctl-x-map (kbd "C-b") 'ibuffer)
    (with-eval-after-load 'ibuffer
      (setq ibuffer-expert t))
    (with-eval-after-load 'image-mode
      (define-key image-mode-map "q" 'image-kill-buffer)
      (setq image-use-external-converter t))
    (with-eval-after-load 'js
      (setq js-indent-level 2))
    (cl-defun rde-org-do-promote (&optional (levels 1))
      "Allow promoting the current heading LEVELS high up the tree."
      (interactive "p")
      (save-excursion
        (if (org-region-active-p)
            (org-map-region (lambda ()
                              (dotimes (_ levels)
                                (org-promote)))
                            (region-beginning) (region-end))
          (dotimes (_ levels)
            (org-promote))))
      (org-fix-position-after-promote))
    (advice-add 'org-do-promote :override 'rde-org-do-promote)
    (define-key mode-specific-map (kbd "l") 'org-store-link)
    (add-hook 'org-mode-hook 'prettify-symbols-mode)
    (add-hook 'org-mode-hook 'org-fragtog-mode)
    (add-hook 'org-mode-hook 'variable-pitch-mode)
    (org-crypt-use-before-save-magic)
    (with-eval-after-load 'org
      (require 'org-tempo)
      (require 'org-timer)
      (let ((map org-mode-map))
        (define-key map (kbd "M-n") 'org-metaright)
        (define-key map (kbd "M-p") 'org-metaleft))
      (setq org-startup-folded 'content)
      (setq org-startup-with-inline-images t)
      (setq org-startup-with-latex-preview t)
      (setq org-extend-today-until 0)
      (setq org-use-fast-todo-selection 'expert)
      (setq org-log-done 'time)
      (setq org-log-into-drawer t)
      (setq org-special-ctrl-a/e t)
      (setq org-insert-heading-respect-content t)
      (setq org-auto-align-tags t)
      (setq org-tags-exclude-from-inheritance '("todo" "crypt"))
      (setq org-enforce-todo-dependencies t)
      (setq org-enforce-todo-checkbox-dependencies t)
      (setq org-archive-location "~/documents/archive.org::* From %s")
      (setq org-fast-tag-selection-single-key 'expert)
      (setq org-display-remote-inline-images 'cache)
      (setq org-image-actual-width nil)
      (setq org-pretty-entities t)
      (setq org-pretty-entities-include-sub-superscripts nil)
      (setq org-M-RET-may-split-line nil)
      (setq org-highest-priority ?A)
      (setq org-lowest-priority ?C)
      (setq org-default-priority ?B)
      (setq org-fontify-done-headline t)
      (add-to-list 'org-structure-template-alist
                   '("js" . "src js")))
    (with-eval-after-load 'org-capture
      (setq org-capture-bookmark nil))
    (with-eval-after-load 'org-src
      (setq org-src-tab-acts-natively t)
      (setq org-src-window-setup 'current-window)
      (setq org-catch-invisible-edits 'show-and-error)
      (setq org-src-fontify-natively t))
    (with-eval-after-load 'org-keys
      (setq org-return-follows-link t))
    (with-eval-after-load 'org-download
      (setq org-download-image-dir "images")
      (setq org-download-image-org-width 300))
    (with-eval-after-load 'ob-core
      (require 'ob-js)
      (setq org-confirm-babel-evaluate nil))
    (with-eval-after-load 'ol
      (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file))
    (with-eval-after-load 'ox
      (setq org-export-preserve-breaks t)
      (require 'ox-md)
      (require 'ox-haunt))
    (with-eval-after-load 'css-mode
      (setq css-indent-offset 2))
    (with-eval-after-load 'mhtml-mode
      (define-key html-mode-map (kbd "M-o") nil))
    (defun rde-completion-crm-indicator (args)
      "Display a discernible indicator for `completing-read-multiple'."
      (cons (concat "[CRM] " (car args)) (cdr args)))

    (advice-add 'completing-read-multiple
                :filter-args 'rde-completion-crm-indicator)
    (define-key goto-map (kbd "a") 'consult-org-agenda)
    (define-key goto-map (kbd "h") 'consult-org-heading)
    (define-key ctl-x-map "b" 'consult-buffer)
    (define-key help-map "a" 'consult-apropos)
    (define-key ctl-x-map (kbd "M-:") 'consult-complex-command)
    (define-key ctl-x-4-map "b" 'consult-buffer-other-window)
    (with-eval-after-load 'consult
      (setq consult-narrow-key "C-=")
      (setq consult-widen-key "C--"))
    (define-key help-map "b" 'embark-bindings)
    (with-eval-after-load 'embark
      (setq embark-indicators '(embark-minimal-indicator))
      (setq embark-prompter 'embark-keymap-prompter)
      (setq prefix-help-command 'embark-prefix-help-command))
    (autoload 'corfu-history-mode "corfu-history")
    (corfu-history-mode)
    (with-eval-after-load 'corfu
      (let ((map corfu-map))
        (define-key map "\t" 'corfu-next)
        (define-key map (kbd "<tab>") 'corfu-next)
        (define-key map (kbd "<backtab>") 'corfu-previous)
        (define-key map (kbd "S-TAB") 'corfu-previous)
        (define-key map (kbd "M-p") 'corfu-doc-scroll-down)
        (define-key map (kbd "M-n") 'corfu-doc-scroll-up)
        (define-key map (kbd "M-d") 'corfu-doc-toggle))
      (require 'kind-icon)
      (setq corfu-auto-prefix 2)
      (setq corfu-exclude-modes '(org-mode))
      (setq kind-icon-default-face 'corfu-default)
      (add-to-list 'corfu-margin-formatters 'kind-icon-margin-formatter)
      (set-face-attribute 'corfu-default nil :inherit 'fixed-pitch))
    (add-to-list 'auto-mode-alist '("\\.y[a]?ml\\'" . yaml-mode))
    (with-eval-after-load 'yaml-mode
      (define-key yaml-mode-map (kbd "RET") 'newline-and-indent))
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'eww-mode-hook 'eww-toggle-images)
    (with-eval-after-load 'window
      (setq split-window-keep-point t)
      (add-to-list 'display-buffer-alist
                   `(,(rx bos "*Embark Collect "
                          (or "Live" "Completions") "*")
                     nil
                     (window-parameters (mode-line-format . none))))
      (add-to-list 'display-buffer-alist
                   `(,(rx "*help" (* any) "*")
                     (display-buffer-reuse-window
                      display-buffer-same-window)
                     (reusable-frames . t)))
      (add-to-list 'display-buffer-alist
                   `(,(rx "*Geiser" (* any) "*")
                     (display-buffer-reuse-window
                      display-buffer-same-window)
                     (reusable-frames . t)))
      (add-to-list 'display-buffer-alist
                   `(,(rx "*Org Links*")
                     display-buffer-no-window
                     (allow-no-window . t)))
      (add-to-list 'display-buffer-alist
                   `(,(rx "*org-roam*")
                     display-buffer-same-window)))
    (repeat-mode 1)
    (with-eval-after-load 'rde-keymaps
      (define-key rde-toggle-map "f" 'display-fill-column-indicator-mode))
    (defun exwm-modeline-update ()
      "Update EXWM modefine for every frame."
      (interactive)
      (cl-loop for frame in exwm-workspace--list
               do (with-selected-frame frame
                    (set-frame-parameter nil 'exwm-modeline--string
                                         (exwm-modeline--format)))))
    (exwm-modeline-mode)
    (with-eval-after-load 'exwm-modeline
      (setq exwm-modeline-randr nil))
    (defun rde-geiser-autoconnect ()
      "Start a Geiser REPL unless an active connection is already present."
      (require 'geiser-guile)
      (require 'geiser-repl)
      (unless (geiser-repl--connection*)
        (save-window-excursion
         (run-guile))))
    (add-hook 'geiser-mode-hook 'rde-geiser-autoconnect)
    (with-eval-after-load 'geiser-repl
      (define-key geiser-repl-mode-map (kbd "C-M-q") 'indent-sexp)
      (setq geiser-repl-use-other-window nil)
      (setq geiser-repl-per-project-p t))
    (setq copyright-names-regexp
          (format "%s <%s>" user-full-name user-mail-address))
    (add-hook 'after-save-hook (lambda () (copyright-update nil nil)))
    (with-eval-after-load 'cider-repl
      (setq cider-repl-display-in-current-window t))
    (with-eval-after-load 'ange-ftp
      (setq ange-ftp-try-passive-mode t))
    (defvar rde-screencast-process nil)
    (defvar rde-screencast-map nil)
    (define-prefix-command 'rde-screencast-map)
    (with-eval-after-load 'rde-keymaps
      (define-key rde-app-map (kbd "S") 'rde-screencast-map)
      (let ((map rde-screencast-map))
        (define-key map (kbd "f") 'rde-record-screencast)
        (define-key map (kbd "r") 'rde-record-screencast-region)
        (define-key map (kbd "s") 'rde-stop-screencast)))
    (defun rde-record-screencast-region ()
      "Record a portion of the screen as a screencast."
      (interactive)
      (require 'hexrgb)
      (pcase-let ((`(,pos-x ,pos-y ,height ,width)
                   (split-string
                    (shell-command-to-string
                     (substring
                      (concat "slop -q -o -b 3 --format=%x,%y,%h,%w -c "
                              (mapconcat 'number-to-string
                                         (hexrgb-hex-to-rgb "#51afef")
                                         ","))
                      0 -1))
                    ",")))

        (setq rde-screencast-process
              (start-process
               "ffmpeg" nil (executable-find "ffmpeg")
               "-framerate" "60" "-show_region" "1"
               "-s" (format "%sx%s" width height)
               "-f" "x11grab"
               "-i" (format ":0.0+%s,%s" pos-x pos-y)
               "-c:v" "libx264" "-qp" "0"
               "-preset" "ultrafast" "-crf" "17"
               (expand-file-name (format-time-string "%Y%m%d-%H%M%S.mp4")
                                 (xdg-user-dir "VIDEOS"))))))
    (defun rde-record-screencast ()
      "Record a full-screen desktop screencast."
      (interactive)
      (notifications-notify
       :app-name "ffmpeg"
       :title "Started recording screen"
       :timeout 3000)
      (run-at-time 2 nil
                   (lambda ()
                     (setq rde-screencast-process
                           (start-process
                            "ffmpeg" nil (executable-find "ffmpeg")
                            "-framerate" "60" "-f" "x11grab" "-i" ":0.0"
                            "-c:v" "libx264" "-qp" "0"
                            "-preset" "ultrafast"
                            (expand-file-name
                             (format-time-string "%Y%m%d-%H%M%S.mp4")
                             (xdg-user-dir "VIDEOS")))))))
    (defun rde-stop-screencast ()
      (interactive)
      (when rde-screencast-process
        (ignore-errors
          (interrupt-process rde-screencast-process)))
      (setq rde-screencast-process nil))
    (defun rde-increase-brightness ()
      "Change monitor/laptop screen brightness through light and ddcutil."
      (interactive)
      (if (listp (rde-exwm--get-outputs))
          (start-process "light" nil "light"
                         "-s" "sysfs/backlight/ddcci1" "-A" "5")
          (start-process "light" nil "light" "-A" "5")))
    (defun rde-decrease-brightness ()
      (interactive)
      (if (listp (rde-exwm--get-outputs))
          (start-process "light" nil "light"
                         "-s" "sysfs/backlight/ddcci1" "-U" "5")
          (start-process "light" nil "light" "-U" "5")))
    (defvar rde-brightness-map nil
      "Map to bind brightness-related helpers under.")
    (define-prefix-command 'rde-brightness-map)
    (with-eval-after-load 'rde-keymaps
      (define-key rde-app-map (kbd "l") 'rde-brightness-map))
    (let ((map rde-brightness-map))
      (define-key map (kbd "i") 'rde-increase-brightness)
      (define-key map (kbd "d") 'rde-decrease-brightness)
      (map-keymap
       (lambda (_key cmd)
         (when (symbolp cmd)
           (put cmd 'repeat-map 'rde-brightness-map)))
       map))
    (defun rde--get-brightness ()
      "Get display brigthness using light."
      (round
       (string-to-number
        (substring
         (if (listp (rde-exwm--get-outputs))
             (shell-command-to-string "light -s sysfs/backlight/ddcci1 -G")
             (shell-command-to-string "light -G"))
         0 -1))))))

(define extra-elisp-packages
  (strings->packages
   "emacs-tempel-collection" "emacs-ox-haunt" "emacs-pinentry"
   "emacs-yaml-mode" "emacs-json-mode" "emacs-nginx-mode" "emacs-pug-mode"
   "emacs-rainbow-delimiters" "emacs-kind-icon" "emacs-gif-screencast"
   "emacs-exwm-modeline"  "emacs-hexrgb" "emacs-wgrep" "emacs-org-fragtog"
   "emacs-org-download" "emacs-org-make-toc"))

(define extra-gtk-settings
  `((gtk-cursor-blink . #f)
    (gtk-cursor-theme-size . 16)
    (gtk-decoration-layout . "")
    (gtk-dialogs-use-header . #f)
    (gtk-enable-animations . #t)
    (gtk-enable-event-sounds . #f)
    (gtk-enable-input-feedback-sounds . #f)
    (gtk-error-bell . #f)
    (gtk-overlay-scrolling . #t)
    (gtk-recent-files-enabled . #f)
    (gtk-shell-shows-app-menu . #f)
    (gtk-shell-shows-desktop . #f)
    (gtk-shell-shows-menubar . #f)
    (gtk-xft-antialias . #t)
    (gtk-xft-dpi . 92)
    (gtk-xft-hinting . #t)
    (gtk-xft-hintstyle . hintfull)
    (gtk-xft-rgba . none)))

(define (extra-gtk-css config)
  (define dark-theme? (get-value 'gtk-dark-theme? config))

  (let ((bg (if dark-theme? 'black 'white))
        (fg (if dark-theme? 'white 'black))
        (accent-bg (string->symbol (if dark-theme? "#afafef" "#d0d6ff")))
        (accent-fg (if dark-theme? 'black 'white))
        (secondary-bg (string->symbol (if dark-theme? "#323232" "#f8f8f8")))
        (secondary-fg (if dark-theme? 'white 'black))
        (tertiary-bg (string->symbol (if dark-theme? "#6272a4" "#f0f0f0")))
        (selection (string->symbol (if dark-theme? "#afafef" "#e8dfd1"))))
    `(((box button:hover)
       ((background . ,secondary-bg)
        (color . ,fg)
        (outline . none)
        (box-shadow . none)))
      ((widget button)
       ((background . none)
        (border . none)
        (color . ,fg)
        (box-shadow . none)))
      ((widget button:hover)
       ((background . ,secondary-bg)))
      (#((stack box > #{button:not(:checked)}#)
         (.titlebar #{button:not(:checked)}#)
         (row #{button:not(:checked)}#)
         #{button.file:not(:checked)}#
         #{button.lock:not(:checked)}#
         #{button.image-button:not(:checked)}#
         #{button.text-button:not(:checked)}#
         #{button.toggle:not(:checked)}#
         #{button.slider-button:not(:checked)}#)
       ((-gtk-icon-shadow . none)
        (background . none)
        (color . ,fg)
        (border . (1px solid ,secondary-bg))
        (outline . none)
        (box-shadow . none)
        (text-shadow . none)))
      (#((stack box #{button:hover:not(:checked)}#)
         (.titlebar #{button:hover:not(:checked)}#)
         (row #{button:hover:not(:checked)}#)
         #{button.file:hover:not(:checked)}#
         #{button.lock:hover:not(:checked)}#
         #{button.image-button:hover:not(:checked)}#
         #{button.slider-button:hover:not(:checked)}#)
       ((background . none)))
      (button:checked
       ((background . ,tertiary-bg)
        (color . ,fg)
        (border . (1px solid ,tertiary-bg))))
      (image
       ((color . ,fg)))
      (image:disabled
       ((color . ,secondary-fg)))
      ((row:selected image)
       ((color . ,tertiary-bg)))
      ((row:selected button:hover)
       ((background . none)))
      ((button image)
       ((-gtk-icon-effect . none)
        (-gtk-icon-shadow . none)
        (color . ,fg)))
      ((headerbar button.toggle)
       ((border-radius . 20px)))
      (#((headerbar button.toggle)
         (stack stackswitcher button:checked)
         radio:checked
         (header button))
       ((background . ,tertiary-bg)
        (color . ,fg)
        (text-shadow . none)
        (border . (1px solid ,tertiary-bg))
        (box-shadow . none)))
      (#((button.color colorswatch)
         (colorswatch overlay))
       ((box-shadow . none)
        (border . none)))
      ((filechooser widget button)
       ((border . (1px solid ,secondary-bg))))
      (#((combobox entry)
         (combobox box)
         (combobox box button))
       ((background . ,secondary-bg)
        (color . ,fg)
        (border . none)
        (outline . none)
        (box-shadow . none)))
      ((combobox button)
       ((border . none)))
      ((combobox button:checked)
       ((background . none)
        (border . none)))
      (button.emoji-section:checked
       ((border . none)))
      (radiobutton
       ((outline . none)))
      ((radiobutton radio)
       ((color . ,bg)
        (background . ,bg)
        (border . none)))
      ((radiobutton radio:checked)
       ((background . ,accent-bg)
        (color . ,secondary-fg)))
      ((radiobutton box)
       ((background . none)
        (border-color . ,fg)))
      (checkbutton
       ((outline . none)))
      (#(check
         (modelbutton radio))
       ((background . ,secondary-bg)
        (color . ,fg)
        (border . none)))
      (#((checkbutton check:checked)
         (modelbutton check:checked)
         (treeview check:checked)
         (modelbutton radio:checked))
       ((background . ,accent-bg)
        (color . ,accent-fg)))
      (#((stackswitcher #{button:not(:checked)}#)
         #{radio:not(:checked)}#)
       ((background . ,secondary-bg)
        (color . ,secondary-fg)
        (border . (1px solid ,secondary-bg))
        (box-shadow . none)
        (text-shadow . none)))
      (switch
       ((background . ,secondary-bg)))
      (#(switch switch-slider)
       ((border-color . ,tertiary-bg)
        (box-shadow . none)))
      (switch:checked
       ((background . ,accent-bg)))
      ((switch:checked image)
       ((color . ,accent-fg)))
      (#((switch slider) (switch slider:disabled))
       ((background . ,tertiary-bg)))
      (label
       ((background . none)
        (text-shadow . none)))
      (label.keycap
       ((box-shadow . (0 -3px ,secondary-bg inset))
        (border-color . ,secondary-bg)))
      ((label link:link)
       ((color . ,accent-bg)
        (caret-color . ,accent-bg)
        (text-decoration-color . ,accent-bg)
        (text-decoration-line . none)))
      (spinbutton
       ((box-shadow . none)))
      ((spinbutton button)
       ((background . ,secondary-bg)
        (-gtk-icon-shadow . none)
        (border . none)))
      (#{spinbutton:not(:disabled)}#
       ((background . ,secondary-bg)
        (border . none)
        (color . ,fg)))
      (#(expander (expander title:hover > arrow))
       ((color . ,fg)))
      (modelbutton
       ((outline . none)))
      (modelbutton.flat:hover
       ((background . ,secondary-bg)))
      (window.background
       ((background . ,bg)
        (color . ,fg)))
      (decoration
       ((background . ,bg)
        (border-radius . (15px 15px 0 0))
        (border . none)
        (padding . 0)))
      (.titlebar
       ((background . ,bg)
        (color . ,fg)
        (border-color . ,secondary-bg)))
      (box
       ((background . ,bg)))
      ((box label)
       ((color . ,fg)))
      ((box frame border)
       ((border-color . ,secondary-bg)))
      (#(stack separator (filechooser paned separator))
       ((background . ,secondary-bg)))
      ((stack box)
       ((background . transparent)))
      (#(viewport list (viewport grid))
       ((background . ,bg)))
      ((viewport list row)
       ((outline . none)
        (background . none)))
      ((viewport row:selected)
       ((color . ,accent-fg)
        (background . ,accent-bg)
        (outline . none)))
      ((viewport row:selected label)
       ((color . ,accent-fg)))
      ((viewport #{row:hover:not(:selected)}#)
       ((background . none)))
      ((viewport row:selected > box label)
       ((color . ,accent-fg)))
      (treeview.view
       ((background . ,secondary-bg)
        (color . ,fg)
        (border-color . ,secondary-bg)
        (outline . none)))
      ((treeview:selected treeview:active)
       ((background . ,accent-bg)
        (color . ,accent-fg)))
      ((treeview header button)
       ((background . ,bg)
        (border . none)))
      (scrolledwindow
       ((border-color . ,secondary-bg)
        (background . ,tertiary-bg)))
      (#((scrolledwindow overshoot.top)
         (scrolledwindow overshoot.bottom))
       ((outline . none)
        (box-shadow . none)
        (background . none)
        (border . none)))
      (#((stack scrolledwindow viewport)
         (window stack widget))
       ((background . ,bg)
        (color . ,fg)))
      (box.view.vertical
       ((border . none)
        (box-shadow . none)
        (background . ,bg)))
      ((scrolledwindow textview text)
       ((background . ,bg)
        (color . ,fg)))
      (header
       ((background . ,bg)
        (border-color . ,secondary-bg)))
      ((header tabs label)
       ((color . ,fg)))
      ((header tabs tab)
       ((background . ,bg)))
      (#(notebook.frame
         (frame.border-ridge border)
         (frame.border-groove border)
         (frame.border-outset border)
         (frame.border-inset border))
       ((border-color . ,secondary-bg)))
      ((noteboox box)
       ((background . ,bg)
        (color . ,fg)))
      ((notebook header tabs tab)
       ((outline . none)))
      ((header tabs #{tab:not(:checked):hover}#)
       ((box-shadow . none)))
      ((notebook header.left tabs tab:checked)
       ((box-shadow . (-4px 0 ,accent-bg inset))))
      ((notebook header.right tabs tab:checked)
       ((box-shadow . (4px 0 ,accent-bg inset))))
      ((notebook header.bottom tabs tab:checked)
       ((box-shadow . (0 4px ,accent-bg inset))))
      ((notebook header.top tabs tab:checked)
       ((box-shadow . (0 -4px ,accent-bg inset))))
      ((notebook header.left tabs tab)
       ((border-right . (1px solid ,secondary-bg))))
      ((notebook header.right tabs tab)
       ((border-left . (1px solid ,secondary-bg))))
      ((notebook header.bottom tabs tab)
       ((border-top . (1px solid ,secondary-bg))))
      ((notebook header.top tabs tab)
       ((border-bottom . (1px solid ,secondary-bg))))
      (#((searchbar revealer box)
         (revealer frame.app-notification)
         (actionbar revealer box))
       ((background . ,tertiary-bg)
        (border-color . ,secondary-bg)))
      (#((searchbar revealer frame)
         (revealer frame.app-notification))
       ((background . ,tertiary-bg)
        (border . (1px solid ,secondary-bg))))
      (#((searchbar revealer box)
         (revealer frame.app-notification)
         (actionbar revealer box))
       ((background . ,tertiary-bg)
        (border-color . ,secondary-bg)))
      (toolbar
       ((background . ,bg)
        (color . ,fg)))
      (paned
       ((background . ,bg)
        (color . ,fg)))
      (paned
       ((background . ,bg)))
      ((flowboxchild grid)
       ((border-color . ,secondary-bg)))
      (#(menu .menu .context-menu)
       ((margin . 0)
        (padding . 0)
        (box-shadow . none)
        (background-color . ,bg)
        (border . (1px solid ,tertiary-bg))))
      (#((.csd menu)
         (.csd .menu)
         (.csd .context-menu))
       ((border . none)))
      (#((menu menuitem)
         (.menu menuitem)
         (.context-menu menuitem))
       ((transition . (background-color 75ms #{cubic-bezier(0, 0, 0.2, 1)}#))
        (min-height . 20px)
        (min-width . 40px)
        (padding . (4px 8px))
        (color . ,fg)
        (font . initial)
        (text-shadow . none)))
      (#((.menu menuitem:hover)
         (.menu menuitem:hover)
         (.context-menu menuitem:hover))
       ((transition . none)
        (background-color . #{alpha(currentColor, 0.08)}#)))
      (#((menu menuitem:disabled)
         (.menu menuitem:disabled)
         (.context-menu menuitem:disabled))
       ((color . #{alpha(currentColor, 0.5)}#)))
      (menubar
       ((background . ,bg)
        (color . ,fg)))
      ((menubar submenu)
       ((padding . 10px)))
      (scrollbar
       ((background . ,bg)
        (border . none)))
      ((scrollbar slider)
       ((background . ,tertiary-bg)
        (min-width . 6px)
        (min-height . 6px)
        (border . (1px solid ,fg))))
      (calendar
       ((background-color . ,bg)
        (color . ,fg)
        (margin . 0)
        (border-color . ,secondary-bg)))
      (calendar:selected
       ((background . ,accent-bg)
        (color . ,accent-fg)))
      (#(calendar.header calendar.button)
       ((background . ,accent-bg)))
      (calendar.button:hover
       ((color . ,accent-bg)))
      (iconview
       ((background . ,bg)
        (color . ,fg)))
      (iconview:selected
       ((background . ,accent-bg)
        (color . ,accent-fg)))
      ((scale contents trough)
       ((background . ,secondary-bg)
        (border-color . ,secondary-bg)
        (outline . none)))
      ((#{scale:not(.marks-after):not(.marks-before):not(:disabled)}#
        contents trough slider)
       ((background . ,secondary-bg)
        (border-color . ,bg)
        (box-shadow . none)))
      ((scale contents trough highlight)
       ((background . ,accent-bg)
        (border-color . ,accent-bg)))
      ((scale:disabled contents trough slider)
       ((background . ,secondary-bg)
        (border-color . ,bg)))
      (#((scale.marks-after contents trough slider)
         (scale.marks-before trough slider))
       ((background . ,secondary-fg)
        (border-radius . 50%)
        (border . (1px solid ,bg))
        (box-shadow . none)
        (min-height . 20px)
        (min-width . 20px)
        (margin . -7px)))
      (progressbar
       ((color . ,fg)))
      ((progressbar trough)
       ((background . ,secondary-bg)
        (border-color . ,secondary-bg)))
      ((progressbar trough progress)
       ((background . ,accent-bg)
        (border-color . ,accent-bg)))
      ((levelbar trough)
       ((background . ,secondary-bg)
        (border-color . ,secondary-fg)))
      ((levelbar trough block)
       ((border-color . ,secondary-fg)
        (background . ,secondary-fg)))
      ((levelbar trough block.filled)
       ((background . ,accent-bg)
        (border-color . ,accent-bg)))
      (entry
       ((background . ,secondary-bg)
        (color . ,fg)
        (border . none)))
      (entry:focus
       ((box-shadow . (0 0 0 1px ,accent-bg inset))))
      ((entry progress)
       ((border-color . ,accent-bg)))
      ((entry image)
       ((color . ,fg)))
      ((colorswatch overlay)
       ((border . none)))
      (selection
       ((background . ,selection)
        (color . ,fg)))
      (dialog
       ((background . ,bg)
        (color . ,fg)))
      (popover
       ((background . ,bg)
        (color . ,fg)
        (border . none)
        (box-shadow . none)))
      (.dialog-box
       ((background . ,bg)))
      ((.dialog-vbox button)
       ((border-radius . 0))))))

(define nyxt-extra-config-lisp
  `(,@%base-nyxt-extra-config-lisp
    (define-configuration nyxt/reduce-tracking-mode:reduce-tracking-mode
      ((nyxt/reduce-tracking-mode:preferred-user-agent
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36
 (KHTML, like Gecko) Chrome/111.0.0.0 Safari/537.36")))))


;;; User-specific features

(define vega-nyxt-features
  (cons*
   (feature-nyxt
    #:scroll-distance 150
    #:temporary-history? #t
    #:autostart-slynk? #t
    #:default-browser? #t
    #:restore-session? #f
    #:extra-config-lisp nyxt-extra-config-lisp)
   (feature-nyxt-nx-tailor #:auto? #t)
   (feature-nyxt-appearance)
   (feature-nyxt-status
    #:format-status-buttons
    '((:raw
       (format-status-back-button status)
       (format-status-reload-button status)
       (format-status-forwards-button status)
       (format-status-close-button status)))
    #:format-status
    '((:div :id "container"
       (:div :id "controls"
        (:raw (format-status-buttons status)))
       (:div :id "url"
        (:raw
         (format-status-load-status status)
         (format-status-url status)))
       (:div :id "modes"
        :title (nyxt::modes-string buffer)
        (:raw
         (format-status-modes status))))))
   %nyxt-base-features))

(define vega-desktop-features
  (list*
   %desktop-base-features
   (feature-desktop-services
    #:default-desktop-home-services
    (append (@@ (rde features base) %rde-desktop-home-services)
            (list
             extra-shell-envs-service
             extra-home-packages-service
             extra-xdg-desktop-entries
             (service home-udiskie-service-type)
             (service home-redshift-service-type
                      (home-redshift-configuration
                       (dawn-time "07:00")
                       (dusk-time "20:00")))
             (service home-xmodmap-service-type
                      (home-xmodmap-configuration
                       (key-map
                        '(("add mod4" . "Print")
                          "clear lock"
                          "clear control"
                          ("keycode 66" . "Control_L")
                          ("add control" . "Control_L Control_R")))))
             (service home-xresources-service-type
                      (home-xresources-configuration
                       (config
                        '((Xcursor.size . 16)
                          (Xft.autohint . #t)
                          (Xft.antialias . #t)
                          (Xft.hinting . #t)
                          (Xft.hintstyle . hintfull)
                          (Xft.rgba . none)
                          (Xft.lcdfilter . lcddefault)
                          (Xft.dpi . 110)))))
             (service home-unclutter-service-type
               (home-unclutter-configuration
                (idle-timeout 5)))
             (simple-service
              'add-startup-scripts
              home-shepherd-service-type
              (list
               (shepherd-service
                (provision '(screensaver))
                (requirement '())
                (one-shot? #t)
                (start #~(lambda ()
                           (invoke #$(file-append
                                      (@ (gnu packages xorg) xset)
                                      "/bin/xset")
                                   "-dpms" "s" "off"))))
               (shepherd-service
                (provision '(cursor))
                (requirement '())
                (one-shot? #t)
                (start #~(lambda ()
                           (invoke #$(file-append
                                      (@ (gnu packages xorg) xsetroot)
                                      "/bin/xsetroot")
                                   "-cursor_name" "left_ptr")))))))))
   (feature-networking)
   (feature-pipewire)))

(define-public %user-features
  (list*
   (feature-user-info
    #:user-name "vega"
    #:full-name %default-fullname
    #:email %default-email
    #:user-groups '("wheel" "netdev" "audio" "video" "libvirt" "spice")
    #:rde-advanced-user? #t
    #:emacs-advanced-user? #t)
   (feature-gnupg
    #:gpg-primary-key "5F23F458"
    #:ssh-keys `((,%default-ssh-keygrip))
    #:pinentry-flavor 'emacs
    #:default-ttl 34560000)
   (feature-alternative-frontends
    #:google-frontend "http://localhost:5000"
    #:youtube-frontend (string-append "https://" %tubo-host)
    #:reddit-frontend "https://teddit.pussthecat.org")
   (feature-android)
   (feature-manpages)
   (feature-emacs
    #:emacs (@ (gnu packages emacs) emacs-next)
    #:emacs-server-mode? #f
    #:extra-early-init-el %base-extra-early-init-el
    #:extra-init-el extra-init-el
    #:additional-elisp-packages extra-elisp-packages)
   %ui-base-features
   (feature-gtk3
    #:gtk-dark-theme? #t
    #:gtk-theme #f
    #:extra-gtk-settings extra-gtk-settings
    #:extra-gtk-css extra-gtk-css)
   %emacs-base-features
   (feature-emacs-nyxt
    #:autostart-delay 5)
   %emacs-desktop-base-features
   vega-nyxt-features
   (feature-ungoogled-chromium
    #:ungoogled-chromium (@ (gnu packages chromium) ungoogled-chromium)
    #:startup-flags '("--incognito"))
   vega-desktop-features
   %multimedia-base-features
   %mail-base-features
   %security-base-features
   %shell-base-features
   %forge-base-features
   %communication-base-features
   %programming-base-features
   %markup-base-features
   (feature-qmk
    #:keyboard "dztech/dz65rgb/v1"
    #:keymap "custom")
   (feature-ssh
    #:ssh-configuration extra-ssh-config)
   (feature-qemu)))
