;; -*- lexical-binding: t; -*-
(defgroup eb-dired nil
  "Personal utilities for the Directory Editor."
  :group 'eb)

;;;###autoload
(defun eb-dired-open-externally ()
  "Opens files in Dired through their corresponding external program."
  (interactive)
  (let ((files (dired-get-marked-files)))
    (mapc #'consult-file-externally files)))

(provide 'eb-dired)
