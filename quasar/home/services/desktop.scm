(define-module (quasar home services desktop)
  #:use-module (quasar home)
  #:use-module (conses home services linux)
  #:use-module (conses home services glib)
  #:use-module (conses home services emacs)
  #:use-module (conses home services xorg)
  #:use-module (conses packages emacs-xyz)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu home-services shells)
  #:use-module (gnu home-services state)
  #:use-module (gnu home-services base)
  #:use-module (gnu system keyboard)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:export (pulseaudio-service
            exwm-service
            desktop-service))

(define (pulseaudio-service)
  (list
   (home-generic-service 'pulseaudio-packages
                         #:packages (list pulseaudio pamixer))
   (elisp-configuration-service
    `((pulseaudio-control-default-keybindings)
      (with-eval-after-load 'pulseaudio-control
        (custom-set-variables
         '(pulseaudio-control-volume-step "5%")
         '(pulseaudio-control-use-default-sink t))))
    #:elisp-packages (list emacs-pulseaudio-control))))

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
EndSection
")

(define (exwm-service)
  (list
   (simple-service
    'home-bash-autostart-exwm
    home-bash-service-type
    (home-bash-extension
     (bash-profile (list #~(string-append
                            "[ $(tty) = /dev/tty1 ] && exec "
                            #$(program-file
                               "exwm-start"
                               (xorg-start-command (xinitrc #:wm emacs)
                                                   (xorg-configuration
                                                    (keyboard-layout (keyboard-layout "us"))
                                                    (extra-config
                                                     (list
                                                      %xorg-libinput-configuration))))))))))
   (elisp-configuration-service
    `(,#~"
(custom-set-variables
  '(exwm-input-global-keys
    `((,(kbd \"s-s\") . (lambda () (interactive) (exwm-layout-shrink-window-horizontally 40)))
      (,(kbd \"s-e\") . (lambda () (interactive) (exwm-layout-enlarge-window-horizontally 40)))
      (,(kbd \"s-C-e\") . (lambda () (interactive) (exwm-layout-enlarge-window 40)))
      (,(kbd \"s-C-s\") . (lambda () (interactive) (exwm-layout-shrink-window 40)))
      (,(kbd \"s-m\") . exwm-workspace-move-window)
      (,(kbd \"s-SPC\") . app-launcher-run-app)
      (,(kbd \"s-<next>\") . (lambda ()
                             (interactive)
                             (if current-prefix-arg
                                 (eb-desktop-audio-change-volume
                                  :step current-prefix-arg :decrease t)
                                 (eb-desktop-audio-change-volume :decrease t))))
      (,(kbd \"s-<prior>\") . (lambda ()
                              (interactive)
                              (if current-prefix-arg
                                  (eb-desktop-audio-change-volume :step current-prefix-arg)
                                  (eb-desktop-audio-change-volume))))
      (,(kbd \"s-C-r\") . exwm-reset)
      (,(kbd \"<XF86AudioPause>\") . ignore)
      (,(kbd \"<XF86AudioPlay>\") . ignore)
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
      (,(kbd \"s-`\") . (lambda () (interactive) (exwm-workspace-switch 0))))))
"
      (custom-set-variables
       '(exwm-workspace-show-all-buffers nil)
       '(exwm-layout-show-all-buffers nil)
       '(exwm-workspace-minibuffer-position nil)
       '(exwm-workspace-wrap-cursor t)
       '(exwm-floating-border-color "#f0f0f0")
       '(exwm-floating-border-width 2)
       '(exwm-workspace-switch-create-limit 5)
       '(exwm-workspace-number 5))
      ,#~""
      (add-hook 'exwm-init-hook 'eb-exwm-set-workspaces)
      (add-hook 'exwm-init-hook 'eb-look-automatic-theme-mode)
      (add-hook 'exwm-init-hook 'eb-exwm-apply-initial-font-settings)
      (add-hook 'exwm-init-hook (lambda ()
                                  (call-process "xmodmap" nil nil nil (concat (getenv "HOME") "/.config/xmodmap/config"))
                                  (call-process "xset" nil nil nil  "-dpms" "s" "off")))
      (add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
      (add-hook 'exwm-manage-finish-hook 'eb-exwm-configure-window-by-class)
      (add-hook 'exwm-update-class-hook 'eb-exwm-shorten-buffer-name)
      (add-hook 'exwm-update-title-hook 'eb-exwm-shorten-buffer-name)
      (exwm-enable)
      ,#~""
      ,#~"(with-eval-after-load 'exwm
(custom-set-variables
  '(exwm-input-simulation-keys
    `(([?\\C-b] . [left])
     ([?\\C-f] . [right])
     ([?\\C-p] . [up])
     ([?\\C-n] . [down])
     ([?\\C-a] . [home])
     ([?\\C-e] . [end])
     ([?\\M-v] . [prior])
     ([?\\C-v] . [next])
     ([?\\C-d] . [delete])
     ([?\\C-k] . [S-end delete]))))
   (require 'exwm-randr)
   (setq exwm-input-prefix-keys
         (append exwm-input-prefix-keys '(?\\M-o ?\\M-g ?\\M-s ?\\s-e)))
   (exwm-randr-enable)
   (eb-exwm-update-output))")
    #:elisp-packages (list emacs-exwm))))

(define (desktop-service)
  (list
   (home-generic-service 'home-desktop-programs
                         #:packages (list maim
                                          slop
                                          xclip
                                          xrandr
                                          ddcutil
                                          light))
   (service home-dbus-service-type)
   (service home-udiskie-service-type)
   (service home-state-service-type
            (append
             (list
              (state-git (string-append (dirname %channel-root) "/guix/")
                         "https://git.savannah.gnu.org/git/guix.git"))))
   (elisp-configuration-service
    `((define-key mode-specific-map "bl" 'bluetooth-list-devices)
      (with-eval-after-load 'bluetooth
        (add-hook 'kill-emacs-hook 'bluetooth-toggle-powered)
        (define-key bluetooth-mode-map "C" 'bluetooth-connect-profile))
      ,#~""
      (ednc-mode)
      (add-hook 'ednc-notification-presentation-functions 'eb-desktop-update-notifications)
      (with-eval-after-load 'notifications
        (custom-set-variables
         '(notifications-application-icon "")))
      ,#~""
      (let ((map mode-specific-map))
        (define-key map "dc" 'eb-desktop-close-last-notification)
        (define-key map "dd" 'eb-desktop-close-all-notifications)
        (define-key map "dl" 'eb-desktop-show-notification-log)
        (define-key map "ss" 'eb-desktop-take-screenshot))
      ,#~""
      (add-hook 'after-init-hook 'eb-desktop-display-weather-mode)
      (eb-desktop-display-volume-mode)
      ,#~""
      (custom-set-variables
       '(battery-mode-line-format (concat (eb-look--position-item "")
                                          " %b%p%% ")))
      (display-battery-mode))
    #:elisp-packages (list emacs-bluetooth
                           emacs-ednc
                           emacs-hexrgb
                           emacs-app-launcher))))
