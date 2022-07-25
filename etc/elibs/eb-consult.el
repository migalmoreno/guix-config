;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'consult)

(defgroup eb-consult nil
  "Personal tweaks to the built-in Emacs completion via `consult'."
  :group 'eb)

(defcustom eb-consult-initial-narrow-alist '()
  "Alist of MODE . KEY to present an initial completion narrowing via `consult'."
  :group 'eb-consult
  :type 'list)

(defun eb-consult--mode-buffers (&rest modes)
  "Return a list of buffers that are derived from MODES
in `buffer-list'."
  (cl-remove-if-not
   (lambda (buffer)
     (with-current-buffer buffer
       (cl-some #'derived-mode-p modes)))
   (buffer-list)))

(autoload #'org-buffer-list "org")

;;;###autoload
(defvar eb-consult-org-buffer-source
  `(:name "Org"
          :narrow ?o
          :category buffer
          :preview-key ,(kbd "M-.")
          :state ,#'consult--buffer-state
          :items ,(lambda () (mapcar #'buffer-name (org-buffer-list)))))

(autoload #'erc-buffer-list "erc")

;;;###autoload
(defvar eb-consult-erc-buffer-source
  `(:name "ERC"
          :narrow ?e
          :category buffer
          :preview-key ,(kbd "M-.")
          :state ,#'consult--buffer-state
          :items ,(lambda () (mapcar #'buffer-name (erc-buffer-list))))
  "Source for ERC buffers to be set in `consult-buffer-sources'.")

(autoload #'telega-root--buffer "telega")

;;;###autoload
(defvar eb-consult-telega-buffer-source
  `(:name "Telega"
          :narrow ?t
          :category buffer
          :preview-key ,(kbd "M-.")
          :state ,#'consult--buffer-state
          :items ,(lambda () (mapcar #'buffer-name
                                     (append (list (telega-root--buffer)) (telega-chat-buffers)))))
  "Source for Telega buffers to be set in `consult-buffer-sources'.")

;;;###autoload
(defvar eb-consult-ement-buffer-source
  `(:name "Ement"
          :narrow ?n
          :category buffer
          :preview-key ,(kbd "M-.")
          :state ,#'consult--buffer-state
          :items ,(lambda ()
                    (mapcar #'buffer-name (eb-consult--mode-buffers 'ement-room-mode 'ement-room-list-mode))))
  "Source for Telega buffers to be set in `consult-buffer-sources'.")

;;;###autoload
(defvar eb-consult-comint-buffer-source
  `(:name "Comint"
          :narrow ?c
          :category buffer
          :preview-key ,(kbd "M-.")
          :state ,#'consult--buffer-state
          :items ,(lambda ()
                    (mapcar #'buffer-name (eb-consult--mode-buffers
                                           'comint-mode 'cider-repl-mode))))
  "Source for `comint-mode' buffers to be set in `consult-buffer-sources'.")

;;;###autoload
(defvar eb-consult-exwm-buffer-source
  `(:name "EXWM"
          :hidden t
          :narrow ?x
          :category buffer
          :state ,#'consult--buffer-state
          :items ,(lambda () (mapcar #'buffer-name (eb-exwm--list-all-buffers))))
  "Source for EXWM buffers to be set in `consult-buffer-sources'.")

;;;###autoload
(defun eb-consult-crm-indicator (args)
  "Display a discernible indicator for `completing-read-multiple'."
  (cons (concat "[CRM] " (car args)) (cdr args)))

;;;###autoload
(defun eb-consult-initial-narrow ()
  "Sets buffer initial narrowing for buffers under a specific mode."
  (when-let* ((buffer-mode-assoc eb-consult-initial-narrow-alist)
              (key (and (eq this-command #'consult-buffer)
                        (or (alist-get (buffer-local-value
                                        'major-mode (window-buffer (minibuffer-selected-window)))
                                       buffer-mode-assoc)
                            (cdr (cl-find-if (lambda (mode)
                                               (with-current-buffer (window-buffer (minibuffer-selected-window))
                                                 (derived-mode-p (car mode))))
                                             buffer-mode-assoc))))))
    (setq unread-command-events (append unread-command-events (list key 32)))))

(provide 'eb-consult)
