;; -*- lexical-binding: t; -*-
(require 'daemons)

(defgroup eb-guix nil
  "Personal utilities to work with the GNU Guix package manager."
  :group 'eb)

(defcustom eb-guix-home-configuration-dir nil
  "Directory that holds the GNU Guix home configuration."
  :type 'directory
  :group 'eb-guix)

;;;###autoload
(defun eb-guix-daemons-root ()
  "Invoke the `daemons' command as `root' to get the list of system daemons."
  (interactive)
  (let ((default-directory (format "/sudo::%s" (make-temp-file nil t))))
    (daemons)))

;;;###autoload
(defun eb-guix-compile-configuration ()
  "Compile the project in `eb-guix-home-configuration-dir'."
  (interactive)
  (let ((default-directory eb-guix-home-configuration-dir)
        (compilation-read-command nil)
        (compile-command "make home")
        (display-buffer-alist '(("\\*compilation\\*"
                                 (display-buffer-no-window)))))
    (call-interactively #'compile)))

(provide 'eb-guix)
