;; -*- lexical-binding: t; -*-
(require 'gnus)
(require 'message)

(defgroup eb-gnus nil
  "Personal customizations for the Gnus newsreader."
  :group 'eb)

(defcustom eb-gnus-topic-topology nil
  "Topics topology for Gnus."
  :group 'eb-gnus
  :type 'list)

(defcustom eb-gnus-topic-alist nil
  "Alist of Gnus topics."
  :group 'eb-gnus
  :type 'list)

(defvar eb-gnus-subscribed-p nil
  "Whether you're currently subscribed to Gnus groups.")

(defun eb-gnus--get-topic-groups ()
  "Return a flattened list of groups from `eb-gnus-topic-alist'."
  (flatten-list (mapcar (lambda (topic)
                          (cdr topic))
                        eb-gnus-topic-alist)))

;;;###autoload
(defun eb-gnus-get-article-participants ()
  "Retrieve the participants from the current article."
  (when (buffer-live-p gnus-article-buffer)
    (with-current-buffer gnus-article-buffer
      (string-join
       (remove-if
        (lambda (address)
          (string-match user-mail-address address))
        (append
         (split-string (message-fetch-field "from") ", ")
         (split-string (message-fetch-field "to") ", ")))
       ", "))))

;;;###autoload
(define-minor-mode eb-gnus-topic-mode
  "Custom mode to apply Gnus topics topology declaratively and subscribe
automatically to the defined groups in them."
  :global t :group 'eb-gnus
  (setq gnus-topic-topology eb-gnus-topic-topology)
  (setq gnus-topic-alist eb-gnus-topic-alist)
  (unless eb-gnus-subscribed-p
    (mapc (lambda (topic)
            (gnus-subscribe-hierarchically topic))
          (eb-gnus--get-topic-groups)))
  (setq eb-gnus-subscribed-p t))

(provide 'eb-gnus)
