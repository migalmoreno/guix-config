;; -*- lexical-binding: t; -*-
(require 'eb-look)
(require 'cl-lib)
(require 'org)
(require 'org-roam)
(require 'org-indent)
(require 'org-mime)
(require 'consult)

(defgroup eb-org nil
  "Personal Org mode customizations."
  :group 'eb)

(defvar eb-org-agenda-appt-timer nil
  "Timer to update `appt-time-msg-list' from Agenda entries.")

;;;###autoload
(defun eb-org-agenda-to-appt ()
  "Reset the `appt-mode' list and initializes it from Agenda entries."
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

(defun eb-org-agenda-appt-reset ()
  "Initialize the `appt-mode' list for today and reset the timer to
run again for tomorrow."
  (interactive)
  (eb-org-agenda-to-appt)
  (setq eb-org-agenda-appt-timer
        (run-at-time "24:01" nil
                     #'eb-org-agenda-appt-reset)))

;;;###autoload
(defun eb-org-mime-darken-codeblocks ()
  "Apply a dark background to email body codeblocks."
  (org-mime-change-element-style
   "pre"
   "color: #E6E1Dc; background-color: #232323; padding: 0.5em;"))

;;;###autoload
(defun eb-org-mime-indent-quotes ()
  "Add padding to block quotes in email body."
  (org-mime-change-element-style
   "blockquote"
   "border-left: 2px solid gray; padding-left: 4px;"))

;;;###autoload
(defun eb-org-timer-reset ()
  "Set `org-timer-mode-line-string' to nil."
  (interactive)
  (setq org-timer-mode-line-string nil))

(defun eb-org-timer-update-mode-line ()
  "Update the timer in the mode line without adding surrounding angle brackets."
  (if org-timer-pause-time
      nil
    (setq org-timer-mode-line-string (substring (org-timer-value-string) 0 -1))
    (force-mode-line-update)))

(defun eb-org-fix-inline-images ()
  "Display inline images automatically."
  (interactive)
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

;;;###autoload
(defun eb-org-set-org-faces ()
  "Set Org mode's faces for `eb-org-minimal-mode'."
  (interactive)
  (set-face-attribute 'org-block nil
                      :inherit 'modus-themes-fixed-pitch
                      :weight 'normal)
  (set-face-attribute 'org-verbatim nil
                      :inherit '(fixed-pitch modus-themes-markup-verbatim)
                      :weight 'normal)
  (set-face-attribute 'org-code nil
                      :inherit '(shadow fixed-pitch modus-themes-markup-code)
                      :weight 'normal)
  (set-face-attribute 'org-document-info nil :weight 'bold)
  (set-face-attribute 'org-document-info-keyword nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-ellipsis nil
                      :inherit '(font-lock-comment-face)
                      :weight 'normal
                      :height (or (eb-look--get-current-preset :default-height) 1.0))
  (set-face-attribute 'org-link nil :underline t)
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-headline-done nil :strike-through t)
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face))
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (cl-loop for (face . height) in '((org-level-1 . 1.15)
                                    (org-level-2 . 1.1)
                                    (org-level-3 . 1.1)
                                    (org-level-4 . 1.0)
                                    (org-level-5 . 1.0)
                                    (org-level-6 . 1.0)
                                    (org-level-7 . 0.9)
                                    (org-level-8 . 0.9))
           do (set-face-attribute face nil :height height)))

;;;###autoload
(defun eb-org-update-buffers-faces ()
  "Go through the current buffer list and apply the appropriate faces
to Org mode buffers."
  (cl-loop for buffer in (org-buffer-list)
           do (with-current-buffer buffer
                (eb-org-set-org-faces))))

(defun eb-org-set-poly-block-faces ()
  "Correctly set a fixed pitch face for polymode source blocks."
  (interactive)
  (let* ((fix-pitch (face-attribute 'fixed-pitch :family))
         (fix-font (face-attribute 'fixed-pitch :font))
         (fix-height (face-attribute 'fixed-pitch :height))
         (props `(:extend t
                          :height ,fix-height
                          :family ,fix-pitch
                          :font ,fix-font)))
    (oset pm/chunkmode adjust-face props)))

;;;###autoload
(defun eb-org-agenda-open-dashboard ()
  "Invoke a custom Org agenda dispatcher for a block agenda view."
  (interactive)
  (org-agenda nil "d"))

(cl-defun eb-org-do-promote (&optional (levels 1))
  "Allow promoting the current heading a number of LEVELS high up the tree."
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
(advice-add #'org-do-promote :override #'eb-org-do-promote)

;;;###autoload
(defun eb-org-roam-switch-to-buffer ()
  "Switch to the current Org-roam buffer."
  (interactive)
  (select-window (get-buffer-window org-roam-buffer))
  (org-roam-buffer-refresh))

(cl-defun eb-org-roam-node-find (&optional other-window initial-input filter-fn &key templates)
  "Find and open an org-roam node by its title or alias in the current window."
  (interactive)
  (let ((display-buffer-alist '(("CAPTURE.*" (display-buffer-same-window)))))
    (org-roam-node-find other-window initial-input filter-fn templates)))

;;;###autoload
(defun eb-org-roam-open-ref ()
  "List all ROAM_REFS in the current buffer and allow you to open them."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (if-let* ((refs (org-property-values "ROAM_REFS"))
              (formatted-refs (consult--read
                               (mapcar
                                (lambda (x)
                                  (org-unbracket-string "[[" "]]" x))
                                (split-string (car (org-property-values "ROAM_REFS")) " "))
                               :prompt "Refs: "
                               :category 'org-roam-ref)))
        formatted-refs)
    (error "No roam refs in this node")))

;;;###autoload
(defun eb-org-roam-node-insert-immediate (arg &rest args)
  "Immediately insert new Org Roam node and insert its link in the buffer."
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list
                                     (append
                                      (car org-roam-capture-templates)
                                      '(:immediate-finish)))))
    (apply #'org-roam-node-insert args)))

;;;###autoload
(define-minor-mode eb-org-agenda-appt-mode
  "Set up `appt-mode' integration for Agenda items."
  :global t :group 'eb-org
  (if eb-org-agenda-appt-mode
      (progn
        (setq eb-org-agenda-appt-timer
              (eb-org-agenda-appt-reset))
        (add-hook 'org-agenda-finalize-hook #'eb-org-agenda-to-appt))
    (progn
      (remove-hook 'org-agenda-finalize-hook #'eb-org-agenda-to-appt)
      (cancel-timer eb-org-agenda-appt-timer))))

;;;###autoload
(define-minor-mode eb-org-minimal-mode
  "Provide a minimal interface to Org mode."
  :global t :group 'eb-org
  (if eb-org-minimal-mode
      (progn
        (corfu-mode -1)
        (org-indent-mode)
        (org-superstar-mode)
        (visual-line-mode)
        (variable-pitch-mode 1)
        (prettify-symbols-mode)
        (flyspell-mode)
        (display-line-numbers-mode -1)
        (org-appear-mode)
        (org-make-toc-mode)
        (setq-local fill-prefix "")
        (eb-org-set-org-faces))
    (corfu-mode 1)
    (org-indent-mode -1)
    (org-superstar-mode -1)
    (visual-line-mode -1)
    (variable-pitch-mode -1)
    (prettify-symbols-mode -1)
    (flyspell-mode -1)
    (display-line-numbers-mode 1)
    (org-appear-mode -1)
    (org-make-toc-mode -1)
    (setq-local fill-prefix nil)))

(provide 'eb-org)
