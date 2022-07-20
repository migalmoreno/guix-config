;; -*- lexical-binding: t; -*-
(require 'org)
(require 'org-mime)

(defgroup eb-mail nil
  "MUA and other email-related settings."
  :group 'eb)

(defcustom eb-mail-gnus-topic-topology nil
  "Topics topology for Gnus."
  :group 'eb-mail
  :type 'list)

(defcustom eb-mail-gnus-topic-alist nil
  "Alist of Gnus topics."
  :group 'eb-mail
  :type 'list)

(defvar eb-mail-gnus-subscribed-p nil
  "Only subscribe to Gnus groups once.")

;;;###autoload
(defun eb-mail-org-mime-darken-codeblocks ()
  "Changes to a dark code block background in email bodies."
  (org-mime-change-element-style
   "pre" (format "color: %s; background-color: %s; padding: 0.5em;" "#E6E1DC" "#232323")))

;;;###autoload
(defun eb-mail-org-mime-indent-quotes ()
  "Nicely offset block quotes in email bodies."
  (org-mime-change-element-style
   "blockquote" "border-left: 2px solid gray; padding-left: 4px;"))

;;;###autoload
(defun eb-mail-message-add-gcc-header ()
  "Prompt for a Gcc header from `eb-mail-gnus-topic-alist'. This will allow
the message to be stored in the right directory of the IMAP server (usually
\"Sent\"). If this header is missing, the outgoing message will go through,
but it won't appear on the right Maildir directory."
  (if (gnus-alive-p)
      (unless (message-fetch-field "Gcc")
        (message-add-header (format "Gcc: %s"
                                    (let ((groups
                                           (cl-remove-if-not
                                            (lambda (group)
                                              (string-match (rx (: "Sent" eol)) group))
                                            (eb-mail--gnus-get-topic-groups))))
                                      (if (> 1 (length groups))
                                          (completing-read "Account: " groups)
                                        (car groups))))))
    (user-error "Gnus is not running. No GCC header will be inserted.")))

(defun eb-mail--gnus-get-topic-groups ()
  "Return a flattened list of groups from `eb-mail-gnus-topic-alist'."
  (flatten-list (mapcar (lambda (topic)
                          (cdr topic))
                        eb-mail-gnus-topic-alist)))

;;;###autoload
(define-minor-mode eb-mail-gnus-topic-mode
  "Custom mode to apply Gnus topics topology declaratively and subscribe
automatically to the defined groups in them."
  :global t :group 'eb-mail
  (setq gnus-topic-topology eb-mail-gnus-topic-topology)
  (setq gnus-topic-alist eb-mail-gnus-topic-alist)
  (unless eb-mail-gnus-subscribed-p
    (mapc (lambda (topic)
            (gnus-subscribe-hierarchically topic))
          (eb-mail--gnus-get-topic-groups)))
  (setq eb-mail-gnus-subscribed-p t))

;;;###autoload
(define-minor-mode eb-mail-message-mode
  "Custom mode to tweak the appearance of `message-mode' buffers."
  :global t :group 'eb-mail
  (if eb-mail-message-mode
      (display-line-numbers-mode -1)
    (display-line-numbers-mode 1)))

(provide 'eb-mail)
