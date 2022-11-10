(define-module (conses features wm)
  #:use-module (conses utils)
  #:use-module (conses packages emacs-xyz)
  #:use-module (conses home services xorg)
  #:use-module (conses home services linux)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (rde home services shells)
  #:use-module (rde serializers elisp)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu system keyboard)
  #:use-module (gnu home services)
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
          (require 'cl-macs))
        (defgroup configure-exwm nil
          "Helpers for EXWM."
          :group 'configure)
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

        (defun configure-exwm--get-outputs ()
          "Return the currently-connected RandR outputs."
          (let ((xrandr-output-regexp (rx "\n" bol (group (+ any)) " connected"))
                outputs)
            (with-temp-buffer
              (call-process ,(file-append xrandr "/bin/xrandr") nil t nil)
              (goto-char (point-min))
              (re-search-forward xrandr-output-regexp nil 'noerror)
              (setq outputs (match-string 1))
              (forward-line)
              (while (re-search-forward xrandr-output-regexp nil 'noerror)
                (setq outputs (append (list outputs) (list (match-string 1)))))
              outputs)))

        (defun configure-exwm-apply-display-settings ()
          "Apply the corresponding display settings after EXWM is enabled."
          (interactive)
          ;; TODO: Remove once I figure out how to invoke user services after Xorg is launched
          (call-process ,(file-append xsetroot "/bin/xsetroot") nil nil nil "-cursor_name" "left_ptr")
          (start-process "unclutter" nil ,(file-append unclutter "/bin/unclutter") "-display" ":0")
          ,@(if (get-value 'emacs-fontaine config)
                '((require 'fontaine)
                  (if (listp (configure-exwm--get-outputs))
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
                            ,@(if (get-value 'emacs-dashboard config)
                                  '((configure-dashboard-open))
                                  '())
                            (add-hook 'after-make-frame-functions 'configure-exwm--disable-tab-bar))))

        (defun configure-exwm-shorten-buffer-name ()
          "Shorten EXWM buffer names to be more discernible."
          (interactive)
          (exwm-workspace-rename-buffer
           (concat exwm-class-name ": "
                   (if (<= (length exwm-title) 30)
                       exwm-title
                     (concat (substring exwm-title 0 29) "...")))))

        (defun configure-exwm--call-xrandr (&rest args)
          "Call `xrandr' with the supplied ARGS."
          (apply 'start-process "xrandr" nil ,(file-append xrandr "/bin/xrandr") args))

        (defun configure-exwm--get-resolution ()
          "Prompt the user for a list of available resolutions."
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

        (defun configure-exwm-change-resolution ()
          "Change the resolution of the primary RandR output."
          (interactive)
          (configure-exwm-automatic-output-mode -1)
          (when-let ((resolution (configure-exwm--get-resolution))
                     (outputs (configure-exwm--get-outputs)))
            (set-process-sentinel
             (if (listp outputs)
                 (configure-exwm--call-xrandr "--output" (cadr outputs) "--primary"
                                              "--mode" resolution "--output" (car outputs) "--off")
               (configure-exwm--call-xrandr "--output" outputs "--mode" resolution))
             (lambda (_process event)
               (when (string= event "finished\n")
                 (configure-exwm-automatic-output-mode))))))

        (defun configure-exwm-update-output ()
          "Update RandR output configuration."
          (interactive)
          (when-let ((secondary-output-regexp (rx "\n" blank (+ any) blank (group (+ nonl))))
                     (outputs (configure-exwm--get-outputs)))
            (if (listp outputs)
                (progn
                  (configure-exwm--call-xrandr "--output" (cadr outputs) "--primary"
                                               "--auto" "--output" (car outputs) "--off")
                  (setq exwm-randr-workspace-monitor-plist (list 0 (cadr outputs))))
              (configure-exwm--call-xrandr "--output" outputs "--auto")
              (with-temp-buffer
                (call-process ,(file-append xrandr "/bin/xrandr") nil t nil "--listactivemonitors")
                (goto-char (point-min))
                (while (not (eobp))
                  (when (and (re-search-forward secondary-output-regexp nil 'noerror)
                             (not (string= (match-string 1) outputs)))
                    (configure-exwm--call-xrandr "--output" (match-string 1) "--auto")))))))

        (define-minor-mode configure-exwm-automatic-output-mode
          "Set up automatic handling of RandR outputs."
          :global t :group 'configure-exwm
          (if configure-exwm-automatic-output-mode
              (add-hook 'exwm-randr-screen-change-hook 'configure-exwm-update-output)
            (remove-hook 'exwm-randr-screen-change-hook 'configure-exwm-update-output)))

        (add-hook 'exwm-init-hook 'configure-exwm-apply-display-settings)
        (add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
        (add-hook 'exwm-update-class-hook 'configure-exwm-shorten-buffer-name)
        (add-hook 'exwm-update-title-hook 'configure-exwm-shorten-buffer-name)
        (setq exwm-input-global-keys
              (append
               (list
                (cons (kbd "s-s") '(lambda () (interactive) (exwm-layout-shrink-window-horizontally 40)))
                (cons (kbd "s-e") '(lambda () (interactive) (exwm-layout-enlarge-window-horizontally 40)))
                (cons (kbd "s-C-s") '(lambda () (interactive) (exwm-layout-shrink-window 40)))
                (cons (kbd "s-C-e") '(lambda () (interactive) (exwm-layout-enlarge-window 40)))
                (cons (kbd "s-f") 'exwm-layout-toggle-fullscreen)
                (cons (kbd "s-r") 'exwm-reset)
                (cons (kbd "s-x") 'configure-exwm-change-resolution)
                (cons (kbd "s-t") 'exwm-floating-toggle-floating)
                (cons (kbd "s-i") 'exwm-input-toggle-keyboard)
                (cons (kbd "s-q") 'kill-this-buffer)
                (cons (kbd "s-<return>") 'split-window-horizontally)
                (cons (kbd "s-m") 'exwm-workspace-move-window)
                (cons (kbd "s-w") 'exwm-workspace-switch)
                (cons (kbd "s-`") '(lambda () (interactive) (exwm-workspace-switch 0))))
               (mapcar (lambda (i)
                         (cons (kbd (format "s-%d" i))
                               `(lambda ()
                                  (interactive)
                                  (exwm-workspace-switch ,i))))
                       (number-sequence 0 ,workspace-number))))
        ,@(if (string= (or (and=> (get-value 'default-application-launcher? config) package-name) "")
                       "emacs-app-launcher")
              '((with-eval-after-load 'exwm-autoloads
                  (exwm-input-set-key (kbd "s-SPC") 'app-launcher-run-app)))
              '())
        (with-eval-after-load 'exwm-autoloads
          (exwm-enable))
        (with-eval-after-load 'exwm
          (setq exwm-input-prefix-keys (append exwm-input-prefix-keys `(,(kbd "M-s") ,(kbd "s-e"))))
          (setq exwm-input-simulation-keys
                (list
                 (cons (kbd "C-b") (kbd "<left>"))
                 (cons (kbd "C-f") (kbd "<right>"))
                 (cons (kbd "C-p") (kbd "<up>"))
                 (cons (kbd "C-n") (kbd "<down>"))
                 (cons (kbd "C-a") (kbd "<home>"))
                 (cons (kbd "C-e") (kbd "<end>"))
                 (cons (kbd "M-v") (kbd "<prior>"))
                 (cons (kbd "C-v") (kbd "<next>"))
                 (cons (kbd "C-d") (kbd "<delete>"))
                 (cons (kbd "C-k") (kbd "<S-end> <delete>"))))
          (setq exwm-layout-show-all-buffers nil)
          (setq exwm-workspace-number ,workspace-number)
          (setq exwm-workspace-show-all-buffers nil)
          (setq exwm-workspace-minibuffer-position nil)
          (setq exwm-workspace-warp-cursor t)
          (setq exwm-workspace-switch-create-limit ,workspace-number)
          (setq exwm-floating-border-color ,floating-window-border-color)
          (setq exwm-floating-border-width ,floating-window-border-width)
          (setq exwm-manage-configurations ',window-configurations)
          (require 'exwm-randr)
          (exwm-randr-enable)
          (configure-exwm-update-output)
          (add-hook 'exwm-randr-screen-change-hook 'configure-exwm-update-output)))
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
      home-shell-profile-service-type
      (list
       #~(format #f "[ $(tty) = /dev/tty~a ] && exec ~a"
                 #$emacs-exwm-tty-number
                 #$(program-file
                    "exwm-start"
                    (xorg-start-command
                     (xinitrc #:command (file-append (get-value 'emacs config) "/bin/emacs")
                              #:args launch-arguments)
                     (xorg-configuration
                      (keyboard-layout (get-value 'keyboard-layout config))
                      (extra-config
                       (interpose
                        (append
                         (list xorg-libinput-configuration)
                         extra-xorg-config)))))))))))

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
