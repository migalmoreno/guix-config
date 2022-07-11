;; -*- lexical-binding: t; -*-
(require 'appt)
(require 'dashboard)
(require 'pdf-tools)
(require 'modus-themes)
(require 'all-the-icons)
(require 'eb-exwm)
(require 'eb-files)
(require 'eb-util)
(require 'eb-web )
(require 'eb-media)

(defgroup eb-look nil
  "Visual and stylistic settings."
  :group 'eb)

(defcustom eb-look-fixed-font nil
  "The default fixed pitch font."
  :group 'eb-look
  :type 'string)

(defcustom eb-look-variable-font nil
  "The default variable pitch font."
  :group 'eb-look
  :type 'string)

(defcustom eb-look-light-theme nil
  "The light theme to use."
  :group 'eb-look
  :type 'string)

(defcustom eb-look-dark-theme nil
  "The dark theme to use."
  :group 'eb-look
  :type 'string)

(defcustom eb-look-headless-font-size 70
  "The font size when the setup is headless."
  :group 'eb-look
  :type 'integer)

(defcustom eb-look-docked-font-size 113
  "The font size when the setup is docked into an external display."
  :group 'eb-look
  :type 'integer)

(defcustom eb-look-default-font-size 70
  "Holds the initial default font size for faces. This is not
 meant to change within the current Emacs session."
  :group 'eb-look
  :type 'integer)

(defcustom eb-look-light-theme-threshold "06:00"
  "The time at which to apply a light theme automatically."
  :group 'eb-look
  :type 'string)

(defcustom eb-look-dark-theme-threshold "21:00"
  "The time at which to apply a dark theme automatically."
  :group 'eb-look
  :type 'string)

(defvar eb-look-light-theme-timer nil
  "Timer to automatically set a light theme based on the time of the day.")

(defvar eb-look-dark-theme-timer nil
  "Timer to automatically set a dark theme based on the time of the day.")

;;;###autoload
(cl-defun eb-look--position-item (string &optional (factor (- 0.125)))
  "Raises or lowers the text in STRING, mostly to fix icons' quirkiness."
  (when string
    (propertize string 'display `(raise ,factor))))

(defun eb-look--build-emojis ()
  "Create an emoji list by looping over the corresponding range of characters."
  (delete
   nil
   (cl-loop with range = '(#x1f000 . #x1f9ff)
            for i upto (- (cdr range) (car range))
            collect (when-let* ((codepoint (+ (car range) i))
                                (name (get-char-code-property codepoint 'name)))
                      (thread-last
                        (replace-regexp-in-string " " "-" (downcase name))
                        (format ":%s:")
                        (format "%s %s" (char-to-string (char-from-name name))))))))

(defconst eb-look-emoji-list (eb-look--build-emojis)
  "Cached list of emojis.")

;;;###autoload
(defun eb-look-emoji-insert ()
  "Inserts an emoji character to the current buffer."
  (interactive)
  (thread-first
    (completing-read "Select emoji: " eb-look-emoji-list)
    (substring 0 1)
    (insert)))

(defun eb-look--theme-dark-p ()
  "Outputs whether the current theme is of a light or dark variant."
  (string= (car custom-enabled-themes) eb-look-dark-theme))

;;;###autoload
(defun eb-look-change-theme ()
  "Changes the theme on the fly and applies corresponding changes to
currently-running applications."
  (interactive)
  (if (eb-look--theme-dark-p)
      (progn
        (eb-faces
         '((tab-bar nil :box (:line-width 1 :color "#a8a8a8" :style unspecified))
           (vertical-border nil :foreground "#000000")))
        (setenv "GTK_THEME" ":dark")
        (when (get-process "nyxt")
          (eb-web-change-theme eb-look-dark-theme))
        (cl-loop for buffer in (eb-files--list-pdf-buffers)
                 do (with-current-buffer buffer
                      (pdf-view-themed-minor-mode 1))))
    (eb-faces
     '((tab-bar nil :box (:line-width 1 :color "#505050" :style unspecified))
       (vertical-border nil :foreground "#ffffff")))
    (setenv "GTK_THEME" ":light")
    (when (get-process "nyxt")
      (eb-web-change-theme eb-look-light-theme))
    (cl-loop for buffer in (eb-files--list-pdf-buffers)
             do (with-current-buffer buffer
                  (pdf-view-themed-minor-mode -1))))
  (eb-media-mpv-change-theme)
  (cl-loop for buffer in (org-buffer-list)
           do (with-current-buffer buffer
                (eb-org-tweak-faces)))
  (eb-faces
   `((default nil :family ,eb-look-fixed-font :height ,eb-look-default-font-size)
     (fixed-pitch nil :family ,eb-look-fixed-font :height ,eb-look-default-font-size)
     (variable-pitch nil :family ,eb-look-variable-font :height ,eb-look-default-font-size))))

(defun eb-look-tweak-info-faces ()
  "Applies some extra appearance settings to `Info-mode' and `Info+-mode'."
  (interactive)
  (when (eb-look--theme-dark-p)
    (eb-faces
     '((info-reference-item nil :background unspecified :foreground "#00d3d0")
       (info-function-ref-item nil :background unspecified :foreground "#b6a0ff")
       (info-quoted-name nil :foreground "#b0d6f5")
       (info-double-quoted-name nil :foreground "#79a8ff")
       (info-xref nil :foreground "#00bcff" :underline t)
       (info-command-ref-item nil :background unspecified)
       (info-macro-ref-item nil :background unspecified)
       (info-variable-ref-item nil :background unspecified)
       (info-string nil :foreground "#79a8ff")))))

(defun eb-look-set-variable-pitched ()
  "Sets a variable-pitched font for the default face in a buffer."
  (face-remap-add-relative 'default :inherit 'variable-pitch))

;;;###autoload
(defun eb-look-dashboard-open ()
  "Jumps to a dashboard buffer, creating one if it doesn't exist."
  (interactive)
  (when (get-buffer-create dashboard-buffer-name)
    (switch-to-buffer dashboard-buffer-name)
    (dashboard-mode)
    (dashboard-insert-startupify-lists)
    (dashboard-refresh-buffer)))

;;;###autoload
(defun eb-look-set-automatic-theme ()
  "Sets the theme automatically based on the time of the day."
  (interactive)
  (let ((current-timestamp (decode-time (current-time))))
    (if (> (+ (decoded-time-second current-timestamp)
              (* 60 (decoded-time-minute current-timestamp))
              (* 3600 (decoded-time-hour current-timestamp)))
           (* 60 (appt-convert-time eb-look-dark-theme-threshold)))
        (progn
          (setenv "GTK_THEME" ":light")
          (modus-themes-load-operandi))
      (setenv "GTK_THEME" ":dark")
      (modus-themes-load-vivendi))))

;;;###autoload
(define-minor-mode eb-look-automatic-theme-mode
  "Configures the appropriate appearance settings."
  :global t :group 'eb-look
  (if eb-look-automatic-theme-mode
      (progn
        (when (and (display-graphic-p)
                   (not (find-font (font-spec :name "all-the-icons"))))
          (all-the-icons-install-fonts t))
        (modus-themes-load-themes)
        (eb-look-set-automatic-theme)
        (add-hook 'modus-themes-after-load-theme-hook #'eb-look-change-theme)
        (setq eb-look-light-theme-timer (run-at-time eb-look-light-theme-threshold
                                                     (* 60 60 24) #'modus-themes-load-operandi))
        (setq eb-look-dark-theme-timer (run-at-time eb-look-dark-theme-threshold
                                                    (* 60 60 24) #'modus-themes-load-vivendi)))
    (mapc (lambda (timer)
            (when timer
              (cancel-timer timer)))
          (list eb-look-light-theme-timer eb-look-dark-theme-timer))
    (remove-hook 'modus-themes-after-load-theme-hook #'eb-look-change-theme)))

(provide 'eb-look)
