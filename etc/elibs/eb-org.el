;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'eb-look)
(require 'org)
(require 'org-roam)
(require 'consult)

(defgroup eb-org nil
  "Org-mode and Org Roam customizations."
  :group 'eb)

(defvar eb-org-agenda-appt-timer nil
  "Timer to update `appt-time-msg-list' from Agenda entries.")

(autoload #'org-buffer-list "org")

;;;###autoload
(defvar eb-org-buffer-source
  `(:name "Org"
          :narrow ?o
          :category buffer
          :state ,#'consult--buffer-state
          :items ,(lambda () (mapcar #'buffer-name (org-buffer-list)))))

;;;###autoload
(defun eb-org-agenda-to-appt ()
  "Resets the `appt-mode' list and initializes it from Agenda entries."
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

(defun eb-org-agenda-appt-reset ()
  "Initializes the `appt-mode' list for today and resets the timer to
run again for tomorrow."
  (interactive)
  (eb-org-agenda-to-appt)
  (setq eb-org-agenda-appt-timer
        (run-at-time "24:01" nil
                     #'eb-org-agenda-appt-reset)))

;;;###autoload
(defun eb-org-timer-reset ()
  "Sets `org-timer-mode-line-string' to nil."
  (interactive)
  (setq org-timer-mode-line-string nil))

;;;###autoload
(defun eb-org-tweak-faces ()
  "Tweaks Org-mode's various faces."
  (interactive)
  (eb-faces
   `((org-block nil :inherit modus-themes-fixed-pitch :weight normal)
     (org-verbatim nil :inherit (fixed-pitch modus-themes-markup-verbatim) :weight normal)
     (org-code nil :inherit (shadow fixed-pitch modus-themes-markup-code) :weight normal)
     (org-document-info nil :weight bold)
     (org-document-info-keyword nil :inherit (shadow fixed-pitch))
     (org-ellipsis nil :inherit (font-lock-comment-face) :weight normal
                   :height ,eb-look-default-font-size)
     (org-link nil :underline t)
     (org-meta-line nil :inherit (font-lock-comment-face fixed-pitch))
     (org-headline-done nil :strike-through t)
     (org-special-keyword nil :inherit (font-lock-comment-face))
     (org-table nil :inherit fixed-pitch)))
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 0.9)
                  (org-level-8 . 0.9)))
    (set-face-attribute (car face) nil :height (cdr face)))
  (with-eval-after-load 'org-indent
    (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))))

(defun eb-org-set-poly-block-faces ()
  "Correctly sets a fixed pitch face for polymode source blocks."
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
  "Allows promoting the current heading a number of LEVELS high up the tree."
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
  "Switches to current Org-roam buffer."
  (interactive)
  (if (get-buffer org-roam-buffer)
      (switch-to-buffer org-roam-buffer)
    (progn
      (org-roam-buffer-toggle)
      (switch-to-buffer org-roam-buffer))))

(cl-defun eb-org-roam-node-find (&optional other-window initial-input filter-fn &key templates)
  "Find and open an org-roam node by its title or alias in the current window."
  (interactive)
  (let ((display-buffer-alist '(("CAPTURE.*" (display-buffer-same-window)))))
    (org-roam-node-find other-window initial-input filter-fn templates)))

;;;###autoload
(defun eb-org-roam-open-ref ()
  "Lists all ROAM_REFS in current buffer and lets you open them."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (let ((ref (consult--read
                (mapcar
                 (lambda (x)
                   (org-unbracket-string "[[" "]]" x))
                 (split-string (car (org-property-values "ROAM_REFS")) " "))
                :prompt "Refs: "
                :category 'org-roam-ref)))
      ref)))

;;;###autoload
(defun eb-org-roam-node-insert-immediate (arg &rest args)
  "Immediately inserts new Org Roam node and then inserts its link in the buffer."
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list
                                     (append
                                      (car org-roam-capture-templates)
                                      '(:immediate-finish)))))
    (apply #'org-roam-node-insert args)))

;;;###autoload
(define-minor-mode eb-org-agenda-appt-mode
  "Mode that sets up `appt-mode' integration for Agenda items."
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
(define-minor-mode eb-org-minimal-ui-mode
  "Provides a minimal interface to `org-mode'."
  :global t :group 'eb-org
  (if eb-org-minimal-ui-mode
      (progn
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
        (eb-org-tweak-faces))
    (progn
        (org-indent-mode -1)
        (org-superstar-mode -1)
        (visual-line-mode -1)
        (variable-pitch-mode -1)
        (prettify-symbols-mode -1)
        (flyspell-mode -1)
        (display-line-numbers-mode 1)
        (org-appear-mode -1)
        (org-make-toc-mode -1)
        (setq-local fill-prefix nil))))

(provide 'eb-org)
