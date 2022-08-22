;; -*- lexical-binding: t; -*-
(require 'sly)

(defgroup eb-lisp nil
  "Lisp tools and extensions."
  :group 'eb)

;;;###autoload
(defun eb-lisp-sly-autoconnect ()
  "Start a SLY REPL unless an active connection is already present."
  (unless (sly-connected-p)
    (save-excursion
      (sly))))

;;;###autoload
(defun eb-lisp-sly-custom-prompt (_package nickname error-level next-entry-idx _condition)
  "Construct a custom SLY prompt."
  (let ((dir (propertize (abbreviate-file-name default-directory) 'font-lock-face 'diff-mode))
        (nick (propertize nickname 'font-lock-face 'sly-mode-line))
        (idx (propertize (number-to-string next-entry-idx) 'font-lock-face 'diff-mode))
        (err-level (when (cl-plusp error-level)
                     (concat (sly-make-action-button
                               (format " [%d]" error-level)
                              #'sly-db-pop-to-debugger-maybe)
                             ""))))
    (concat "(" dir ")\n"
            (propertize "<" 'font-lock-face 'sly-mrepl-prompt-face)
            idx
            (propertize ":" 'font-lock-face 'sly-mrepl-prompt-face)
            nick
            err-level
            (propertize "> " 'font-lock-face 'sly-mrepl-prompt-face))))

(provide 'eb-lisp)
