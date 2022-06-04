(define-module (quasar home services desktop)
  #:use-module (quasar home)
  #:use-module (efimerspan home services linux)
  #:use-module (efimerspan home services glib)
  #:use-module (efimerspan home services emacs)
  #:use-module (efimerspan packages emacs-xyz)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu home-services state)
  #:use-module (gnu home-services base)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages rust-apps)
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

(define (exwm-service)
  (list
   ;; (home-generic-service 'home-exwm-files
   ;;                       #:files `((".xsession"
   ;;                                  ,(mixed-text-file
   ;;                                    "xsession"
   ;;                                    "dbus-run-session -- emacs -mm --debug-init"))))
   (elisp-configuration-service
    `((custom-set-variables
       '(exwm-workspace-show-all-buffers nil)
       '(exwm-layout-show-all-buffers nil)
       '(exwm-workspace-minibuffer-position nil)
       '(exwm-workspace-wrap-cursor t)
       '(exwm-floating-border-color "#f0f0f0")
       '(exwm-floating-border-width 2)
       '(exwm-workspace-switch-create-limit 5)
       '(exwm-workspace-number 5)
       '(exwm-input-global-keys
         `((,(kbd "s-s") . (lambda () (interactive) (exwm-layout-shrink-window-horizontally 40)))
           (,(kbd "s-e") . (lambda () (interactive) (exwm-layout-enlarge-window-horizontally 40)))
           (,(kbd "s-C-e") . (lambda () (interactive) (exwm-layout-enlarge-window 40)))
           (,(kbd "s-C-s") . (lambda () (interactive) (exwm-layout-shrink-window 40)))
           (,(kbd "s-m") . (lambda () (interactive) (exwm-workspace-move-window)))
           (,(kbd "s-SPC") . (lambda () (interactive) (app-launcher-run-app)))
           (,(kbd "s-<next>") . (lambda ()
                                  (interactive)
                                  (if current-prefix-arg
                                      (eb-desktop-audio-change-volume
                                       :step current-prefix-arg :decrease t)
                                      (eb-desktop-audio-change-volume :decrease t))))
           (,(kbd "s-<prior>") . (lambda ()
                                   (interactive)
                                   (if current-prefix-arg
                                       (eb-desktop-audio-change-volume :step current-prefix-arg)
                                       (eb-desktop-audio-change-volume))))
           (,(kbd "s-C-r") . (lambda () (interactive) (exwm-reset)))
           (,(kbd "s-W") . (lambda () (interactive) (exwm-workspace-switch)))
           (,(kbd "s-t") . (lambda () (interactive) (exwm-floating-toggle-floating)))
           (,(kbd "s-F") . (lambda () (interactive) (exwm-layout-toggle-fullscreen)))
           (,(kbd "C-q") . (lambda () (interactive) (call-interactively 'exwm-input-send-next-key)))
           (,(kbd "s-q") . (lambda() (interactive) (kill-buffer)))
           ,@(mapcar (lambda (i)
                       `(,(kbd (format "s-%d" i)) .
                         (lambda ()
                           (interactive)
                           (exwm-workspace-switch ,i))))
                     (number-sequence 0 exwm-workspace-number))
           (,(kbd "s-`") . (lambda () (interactive) (exwm-workspace-switch 0))))))
      ,#~""
      (add-hook 'exwm-init-hook 'eb-exwm-set-workspaces)
      (add-hook 'exwm-init-hook 'eb-look-automatic-theme-mode)
      (add-hook 'exwm-init-hook 'eb-exwm-apply-initial-settings)
      (add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
      (add-hook 'exwm-manage-finish-hook 'eb-exwm-configure-window-by-class)
      (eb-to-hooks 'eb-exwm-shorten-buffer-name 'exwm-update-class 'exwm-update-title)
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
                 (state-git (string-append (dirname %project-root) "/guix/")
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
        (define-key map "dl" 'eb-desktop-show-notification-log))
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
