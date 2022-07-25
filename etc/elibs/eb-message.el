;; -*- lexical-binding: t; -*-
(require 'eb-gnus)

(defgroup eb-message nil
  "Personal Message mode tweaks."
  :group 'eb)

;;;###autoload
(defun eb-message-add-gcc-header ()
  "Prompt for a Gcc header from `eb-gnus-topic-alist'. This will allow
a message to be stored in the right directory of the IMAP server (usually
\"Sent\"). If this header is missing, the outgoing message will go through,
but it won't appear on the right Maildir directory."
  (if (gnus-alive-p)
      (unless (message-fetch-field "Gcc")
        (message-add-header (format "Gcc: %s"
                                    (let ((groups
                                           (cl-remove-if-not
                                            (lambda (group)
                                              (string-match (rx (: "Sent" eol)) group))
                                            (eb-gnus--get-topic-groups))))
                                      (if (> 1 (length groups))
                                          (completing-read "Account: " groups)
                                        (car groups))))))
    (user-error "Gnus is not running. No GCC header will be inserted.")))

;;;###autoload
(define-minor-mode eb-message-mode
  "Custom mode to tweak the appearance of `message-mode' buffers."
  :global t :group 'eb-message
  (if eb-message-mode
      (display-line-numbers-mode -1)
    (display-line-numbers-mode 1)))

(provide 'eb-message)
