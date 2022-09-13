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

;;;###autoload
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
          :items ,(lambda () (mapcar #'buffer-name (org-buffer-list))))
  "Source for Org buffers to be set in `consult-buffer-sources'.")

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
                                     (eb-consult--mode-buffers 'telega-chat-mode 'telega-root-mode))))
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
  "Source for Ement buffers to be set in `consult-buffer-sources'.")

;;;###autoload
(defvar eb-consult-slack-buffer-source
  `(:name "Slack"
          :narrow ?s
          :category buffer
          :preview-key ,(kbd "M-.")
          :state ,#'consult--buffer-state
          :items ,(lambda ()
                    (mapcar #'buffer-name (eb-consult--mode-buffers 'slack-message-buffer-mode 'slack-thread-message-buffer-mode))))
  "Source for Slack buffers to be set in `consult-buffer-sources'.")

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
  "Set buffer initial narrowing for buffers under a specific mode."
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

(defun eb-consult--build-emojis ()
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

(defconst eb-consult-emoji-list (eb-consult--build-emojis)
  "Cached list of emojis.")

;;;###autoload
(defun eb-consult-insert-emoji ()
  "Insert an emoji character to the current buffer."
  (interactive)
  (thread-first
    (completing-read "Select emoji: " eb-consult-emoji-list)
    (substring 0 1)
    (insert)))

(provide 'eb-consult)
