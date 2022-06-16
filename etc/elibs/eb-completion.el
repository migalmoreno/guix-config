;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'consult)
(require 'telega)
(require 'erc)
(require 'exwm)

(defgroup eb-completion nil
  "Tweaks to the built-in Emacs completion framework."
  :group 'eb)

(defcustom eb-completion-initial-narrow-alist '()
  "Alist of MODE . KEY to present an initial completion narrowing via `consult'."
  :group 'eb-completion
  :type 'list)

;;;###autoload
(defun eb-completion-crm-indicator (args)
  "Display a discernible indicator for `completing-read-multiple'."
  (cons (concat "[CRM] " (car args)) (cdr args)))

;;;###autoload
(defun eb-completion-consult-initial-narrow ()
  "Sets buffer initial narrowing for buffers under a specific mode."
  (when-let* ((buffer-mode-assoc eb-completion-initial-narrow-alist)
              (key (and (eq this-command #'consult-buffer)
                        (or (alist-get (buffer-local-value
                                        'major-mode (window-buffer (minibuffer-selected-window)))
                                       buffer-mode-assoc)
                            (cdr (cl-find-if (lambda (mode)
                                               (with-current-buffer (window-buffer (minibuffer-selected-window))
                                                 (derived-mode-p (car mode))))
                                             buffer-mode-assoc))))))
    (setq unread-command-events (append unread-command-events (list key 32)))))

(provide 'eb-minibuffer)
