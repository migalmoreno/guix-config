;; -*- lexical-binding: t; -*-
(defgroup eb-tempel nil
  "TempEL extensions."
  :group 'eb)

;;;###autoload
(defun eb-tempel-setup-capf ()
  "Add the Tempel Capf `tempel-complete' to
 `completion-tat-point-functions'."
  (setq-local completion-at-point-functions
              (cons #'tempel-complete
                    completion-at-point-functions)))
