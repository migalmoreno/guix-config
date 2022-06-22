;; -*- lexical-binding: t; -*-
(require 'geiser)
(require 'geiser-guile)
(require 'geiser-repl)
(require 'sly)

(defgroup eb-lisp nil
  "Lisp tools and extensions."
  :group 'eb)

;;;###autoload
(defun eb-lisp-sly-autoconnect ()
  "Starts a SLY REPL unless an active connection is already present."
  (unless (sly-connected-p)
    (save-excursion
      (sly))))

;;;###autoload
(defun eb-lisp-sly-custom-prompt (_package nickname error-level next-entry-idx _condition)
  "Constructs a custom SLY prompt."
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

;;;###autoload
(defun eb-lisp-geiser-autoconnect ()
  "Starts a Geiser REPL unless an active connection is already present."
  (unless (geiser-repl--connection*)
    (save-window-excursion
      (run-guile))))

;;;###autoload
(defun eb-lisp-embellish-mrepl-output ()
  "Translates the output codes in the pre-output filters
for `sly-mrepl-mode' buffers."
  (add-hook 'comint-preoutput-filter-functions #'ansi-color-apply nil t))

;;;###autoload
(defun eb-lisp-daemons-root ()
  "Invokes the `daemons' command as `root' to get the list of system daemons."
  (interactive)
  (let ((default-directory (format "/sudo::%s" (make-temp-file nil t))))
    (daemons)))

(provide 'eb-lisp)
