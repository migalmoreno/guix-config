(define-module (conses features wm)
  #:use-module (conses utils)
  #:use-module (conses packages emacs-xyz)
  #:use-module (conses home services xorg)
  #:use-module (conses home services linux)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (rde serializers elisp)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu system keyboard)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:export (feature-emacs-exwm
            feature-emacs-exwm-run-on-tty))


;;;
;;; EXWM.
;;;

(define* (feature-emacs-exwm
          #:key
          (emacs-exwm emacs-exwm)
          (workspace-number 3)
          (window-configurations '())
          (floating-window-border-width 0)
          (floating-window-border-color "#212121"))
  "Configure the Emacs X Window Manager."
  (ensure-pred file-like? emacs-exwm)
  (ensure-pred integer? workspace-number)
  (ensure-pred elisp-config? window-configurations)
  (ensure-pred integer? floating-window-border-width)
  (ensure-pred string? floating-window-border-color)

  (define emacs-f-name 'exwm)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to EXWM."
    (require-value 'emacs config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
          (require 'cl-lib))
        (defvar configure-exwm-default-output nil
          "The name of the default RandR output.")
        (defvar configure-exwm-secondary-output nil
          "The name of the secondary RandR output.")

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
          "Return non-nil if RandR has currently more than one output connected."
          (let ((xrandr-output-regexp (rx "\n" bol (group (+ any)) " connected")))
            (with-temp-buffer
              (call-process ,(file-append xrandr "/bin/xrandr") nil t nil)
              (goto-char (point-min))
              (re-search-forward xrandr-output-regexp nil 'noerror)
              (setq configure-exwm-default-output (match-string 1))
              (forward-line)
              (prog1
                  (null (not (re-search-forward xrandr-output-regexp nil 'noerror)))
                (setq configure-exwm-secondary-output (match-string 1))))))

        (defun configure-exwm-apply-display-settings ()
          "Apply the corresponding display settings after EXWM is enabled."
          (interactive)
          ,@(if (get-value 'emacs-fontaine config)
                '((require 'fontaine)
                  (if (configure-exwm--docked-p)
                      (fontaine-set-preset 'docked)
                    (fontaine-set-preset 'headless)))
                '())
          (cl-loop for i from 0 upto (- exwm-workspace-number 1)
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
                (secondary-output-regexp (rx "\n" blank (+ any) blank (group (+ nonl)))))
            (if (not (configure-exwm--docked-p))
                (progn
                 (apply 'configure-exwm--invoke-xrandr
                        "--output" configure-exwm-default-output
                        (if resolution
                            (list "--mode" resolution)
                          (list "--auto")))
                  (with-temp-buffer
                    (call-process ,(file-append xrandr "/bin/xrandr") nil t nil "--listactivemonitors")
                    (goto-char (point-min))
                    (while (not (eobp))
                      (when (and (re-search-forward secondary-output-regexp nil 'noerror)
                                 (not (string= (match-string 1) configure-exwm-default-output)))
                        (call-process ,(file-append xrandr "/bin/xrandr")
                                      nil nil nil "--output" (match-string 1) "--auto")))))
              (apply 'configure-exwm--invoke-xrandr
                     "--output" configure-exwm-secondary-output "--primary"
                     (append
                      (if resolution
                         (list "--mode" resolution)
                        (list "--auto"))
                      (list "--output" configure-exwm-default-output "--off")))
              (setq exwm-randr-workspace-monitor-plist (list 0 configure-exwm-secondary-output)))))

        (add-hook 'exwm-init-hook 'configure-exwm-apply-display-settings)
        (add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
        (add-hook 'exwm-update-class-hook 'configure-exwm-shorten-buffer-name)
        (add-hook 'exwm-update-title-hook 'configure-exwm-shorten-buffer-name)
        (setq exwm-layout-show-all-buffers nil)
        (setq exwm-workspace-number ,workspace-number)
        (setq exwm-workspace-show-all-buffers nil)
        (setq exwm-workspace-minibuffer-position nil)
        (setq exwm-workspace-warp-cursor t)
        (setq exwm-workspace-switch-create-limit ,workspace-number)
        (setq exwm-floating-border-color ,floating-window-border-color)
        (setq exwm-floating-border-width ,floating-window-border-width)
        (setq exwm-manage-configurations ',window-configurations)
        (setq exwm-input-global-keys
              (append
               (list
                (cons (kbd "s-s") '(lambda () (interactive) (exwm-layout-shrink-window-horizontally 40)))
                (cons (kbd "s-e") '(lambda () (interactive) (exwm-layout-enlarge-window-horizontally 40)))
                (cons (kbd "s-C-s") '(lambda () (interactive) (exwm-layout-shrink-window 40)))
                (cons (kbd "s-C-e") '(lambda () (interactive) (exwm-layout-enlarge-window 40)))
                (cons (kbd "s-m") 'exwm-workspace-move-window)
                (cons (kbd "s-C-r") 'exwm-reset)
                (cons (kbd "s-w") 'exwm-workspace-switch)
                (cons (kbd "s-t") 'exwm-floating-toggle-floating)
                (cons (kbd "s-f") 'exwm-layout-toggle-fullscreen)
                (cons (kbd "s-k") 'exwm-input-toggle-keyboard)
                (cons (kbd "s-q") 'kill-this-buffer)
                (cons (kbd "s-`") '(lambda () (interactive) (exwm-workspace-switch 0))))
               (mapcar (lambda (i)
                         (cons (kbd (format "s-%d" i))
                               `(lambda ()
                                  (interactive)
                                  (exwm-workspace-switch ,i))))
                       (number-sequence 0 exwm-workspace-number))))
        ,#~"
(setq exwm-input-prefix-keys (append exwm-input-prefix-keys '(?\\M-s ?\\s-e)))
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
        ([?\\C-k] . [S-end delete])))"
        ,@(if (string= (package-name (get-value 'default-application-launcher? config))
                       "app-launcher")
              '((exwm-input-set-key (kbd "s-SPC") 'app-launcher-run-app))
              '())
        (with-eval-after-load 'exwm-autoloads
          (exwm-enable))
        (with-eval-after-load 'exwm
          (require 'exwm-randr)
          (exwm-randr-enable)
          (add-hook 'after-init-hook 'configure-exwm-update-output)))
      #:elisp-packages (append
                        (list emacs-exwm)
                        (if (get-value 'emacs-fontaine config)
                            (list (get-value 'emacs-fontaine config))
                            '()))
      #:summary "Utilities for EXWM"
      #:commentary "Helpers for EXWM to switch outputs automatically, change resolution
on-the-fly, and set up the initial workspace configuration.")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-exwm)))
   (home-services-getter get-home-services)))


;;;
;;; emacs-exwm-run-on-tty.
;;;

(define %default-xorg-libinput-configuration
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

(define* (feature-emacs-exwm-run-on-tty
          #:key
          (emacs-exwm-tty-number 2)
          (xorg-libinput-configuration
           %default-xorg-libinput-configuration)
          (extra-xorg-config '())
          (launch-arguments '()))
  "Launch EXWM on a specified TTY upon user login and
automatically switch to EXWM-TTY-NUMBER on boot."
  (ensure-pred tty-number? emacs-exwm-tty-number)
  (ensure-pred string? xorg-libinput-configuration)
  (ensure-pred list? extra-xorg-config)
  (ensure-pred list? launch-arguments)

  (define (get-home-services config)
    "Return home services related to EXWM run on TTY."
    (require-value 'emacs-exwm config)
    (list
     (simple-service
      'run-exwm-on-login-tty
      home-bash-service-type
      (home-bash-extension
       (bash-profile
        (list
         (mixed-text-file
          "bash-profile"
          #~(format #f "[ $(tty) = /dev/tty~a ] && exec ~a"
                    #$emacs-exwm-tty-number
                    #$(program-file
                       "exwm-start"
                       (xorg-start-command
                        (xinitrc #:wm (get-value 'emacs config) #:args launch-arguments)
                        (xorg-configuration
                         (keyboard-layout (get-value 'keyboard-layout config))
                         (extra-config
                          (append
                           (list xorg-libinput-configuration)
                           extra-xorg-config)))))))))))))

  (define (get-system-services _)
    "Return system services related to EXWM run on TTY."
    (list
     (simple-service
      'switch-to-exwm-tty-after-boot
      shepherd-root-service-type
      (list
       (shepherd-service
        (provision '(switch-to-exwm-tty))
        (requirement '(virtual-terminal))
        (start #~(lambda ()
                   (invoke #$(file-append kbd "/bin/chvt")
                           #$(format #f "~a" emacs-exwm-tty-number))))
        (one-shot? #t))))))

  (feature
   (name 'emacs-exwm-run-on-tty)
   (values (make-feature-values emacs-exwm-tty-number))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))
