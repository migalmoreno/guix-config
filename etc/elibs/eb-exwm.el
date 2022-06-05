;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'eb-desktop)
(require 'exwm)
(require 'consult)

(defgroup eb-exwm nil
  "EXWM customizations."
  :group 'eb-desktop)

;;;###autoload
(defvar eb-exwm-buffer-source
  `(:name "EXWM"
          :hidden t
          :narrow ?x
          :category buffer
          :state ,#'consult--buffer-state
          :items ,(lambda () (mapcar #'buffer-name (eb-exwm--list-all-buffers))))
  "Source for EXWM buffers to be set in `consult-buffer-sources'.")

(defvar eb-exwm-default-output nil
  "The name of the default RandR output.")

(defvar eb-exwm-docked-output nil
  "The name of the external display RandR output.")

(defun eb-exwm--list-all-buffers ()
  "Lists all currently opened EXWM buffers."
  (cl-remove-if-not
   (lambda (buffer)
     (eq 'exwm-mode (buffer-local-value 'major-mode buffer)))
   (buffer-list)))

;;;###autoload
(defun eb-exwm-shorten-buffer-name ()
  "Shortens EXWM buffer names to be more discernible."
  (interactive)
  (exwm-workspace-rename-buffer
   (concat exwm-class-name ": "
           (if (<= (length exwm-title) 30)
               exwm-title
             (concat (substring exwm-title 0 29) "...")))))

(defun eb-exwm--disable-tab-bar (frame)
  "Disables the tab bar mode on new Emacs frames."
  (set-frame-parameter frame 'tab-bar-lines 0))

;;;###autoload
(defun eb-exwm-configure-window-by-class ()
  "Assigns custom behavior to given EXWM windows."
  (interactive)
  (pcase exwm-class-name
    ("Nyxt"
     (exwm-workspace-move-window 2)
     (exwm-input-set-local-simulation-keys nil)
     (exwm-layout-hide-mode-line))))

(defun eb-exwm--run-in-background (command)
  "Runs COMMAND as a background process."
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

;;;###autoload
(defun eb-exwm-set-workspaces ()
  "Sets up EXWM workspaces."
  (interactive)
  (cl-loop for i from 0 upto exwm-workspace-number
           do (exwm-workspace-switch-create i)
           finally (exwm-workspace-switch-create 0)))

;;;###autoload
(defun eb-exwm--docked-p ()
  "Returns non-`nil' if the current setup is docked to an external display."
  (let ((xrandr-output-regexp (rx (: "\n" bol (group (+ any)) " connected"))))
    (with-temp-buffer
      (call-process (executable-find "xrandr") nil t nil)
      (goto-char (point-min))
      (re-search-forward xrandr-output-regexp nil 'noerror)
      (setq eb-exwm-default-output (match-string 1))
      (forward-line)
      (prog1
          (null (not (re-search-forward xrandr-output-regexp nil 'noerror)))
        (setq eb-exwm-docked-output (match-string 1))))))

;;;###autoload
(defun eb-exwm-apply-initial-settings ()
  "Applies the corresponding display settings after EXWM is enabled."
  (interactive)
  (if (eb-exwm--docked-p)
      (setq eb-look-default-font-size eb-look-docked-font-size)
    (setq eb-look-default-font-size eb-look-headless-font-size)))

;;;###autoload
(defun eb-exwm-update-output (&optional change-res-p)
  "Updates RandR output configuration. If CHANGE-RES-P, it allows to change
the resolution of the primary output."
  (interactive "P")
  (when change-res-p
    (remove-hook 'exwm-randr-screen-change-hook #'eb-exwm-update-output))
  (let ((resolution (when change-res-p
                      (eb-exwm-get-resolution)))
        (xrandr-monitor-regexp "\n .* \\([^ \n]+\\)"))
    (if (not (eb-exwm--docked-p))
        (progn
          (apply #'eb-exwm--invoke-xrandr `("--output" ,eb-exwm-default-output
                                            ,@(if resolution
                                                  `("--mode" ,resolution)
                                                '("--auto"))))
          (with-temp-buffer
            (call-process "xrandr" nil t nil "--listactivemonitors")
            (goto-char (point-min))
            (while (not (eobp))
              (when (and (re-search-forward xrandr-monitor-regexp nil 'noerror)
                         (not (string= (match-string 1) eb-exwm-default-output)))
                (call-process "xrandr" nil nil nil "--output" (match-string 1) "--auto")))))
      (apply #'eb-exwm--invoke-xrandr `("--output" ,eb-exwm-docked-output "--primary"
                                        ,@(if resolution
                                              `("--mode" ,resolution)
                                            '("--auto"))
                                        "--output" ,eb-exwm-default-output "--off"))
      (setq exwm-randr-workspace-monitor-plist (list 0 eb-exwm-docked-output)))))

(defun eb-exwm--invoke-xrandr (&rest args)
  "Calls `xrandr' with the supplied ARGS."
  (let ((process (apply #'start-process "xrandr" nil (executable-find "xrandr") args)))
    (set-process-sentinel
     process
     (lambda (_p e)
       (when (string= e "finished\n")
         (add-hook 'exwm-randr-screen-change-hook #'eb-exwm-update-output))))))

(defun eb-exwm-get-resolution ()
  "Prompts the user for a list of available resolutions in the current
 primary output and returns it."
  (interactive)
  (with-temp-buffer
    (call-process (executable-find "xrandr") nil t nil)
    (goto-char (point-min))
    (unless (re-search-forward
             (rx (: "\n" bol (+ any) " connected primary"))
             nil 'noerror)
      (goto-char (point-min))
      (re-search-forward (rx (: bol (+ any) "connected")) nil 'noerror))
    (let ((resolutions
           (cl-loop while (not (eobp))
                    do (forward-line 1)
                    when (re-search-forward
                          (rx (: (+ blank) (group (+ num) "x" (+ num))
                                 (+ blank) (+ num)))
                          nil 'noerror)
                    collect (match-string 1))))
      (consult--read
       resolutions
       :prompt "Select resolution: "
       :sort nil))))

(provide 'eb-exwm)
