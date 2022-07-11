;; -*- lexical-binding: t; -*-
(require 'org)
(require 'org-mime)

(defgroup eb-mail nil
  "MUA and other email-related settings."
  :group 'eb)

(defvar eb-mail-gnus-subscribe-groups-done nil
  "Only subscribe to groups once, else Gnus won't restart.")

;;;###autoload
(defun eb-mail-group-list-subscribed-groups ()
  "Lists all the subscribed groups regardless of their unread messages status."
  (interactive)
  (gnus-group-list-all-groups 5))

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
(define-minor-mode eb-mail-message-mode
  "Custom mode to tweak the appearance of `message-mode' buffers."
  :global t :group 'eb-mail
  (if eb-mail-message-mode
      (display-line-numbers-mode -1)
    (display-line-numbers-mode 1)))

(provide 'eb-mail)
