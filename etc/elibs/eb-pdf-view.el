;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(defgroup eb-pdf-view nil
  "Personal tweaks for `pdf-view'."
  :group 'eb)

(defun eb-pdf-view--list-buffers ()
  "List all currently-opened `pdf-view' mode buffers."
  (cl-remove-if-not
   (lambda (buffer)
     (with-current-buffer buffer
       (derived-mode-p 'pdf-view-mode)))
   (buffer-list)))

(defun eb-pdf-view-update-buffers ()
  "Apply `eb-pdf-view-mode' to currently opened
`pdf-view' mode buffers."
  (cl-loop for buffer in (eb-pdf-view--list-buffers)
           do (with-current-buffer buffer
                (eb-pdf-view-mode 1))))

(defun eb-pdf-view-run-with-pdf-view (fun &rest args)
  "Run FUN with ARGS in the context of the first `pdf-view' mode buffer."
  (if (cl-find (current-buffer) (eb-pdf-view--list-buffers))
      (apply fun args)
    (let ((buffer (car (eb-pdf-view--list-buffers))))
      (save-window-excursion
        (with-selected-window (get-buffer-window buffer)
          (apply fun args))))))

;;;###autoload
(define-minor-mode eb-pdf-view-mode
  "Apply `pdf-view' settings based on the current theme."
  :global t :group 'eb-pdf-view
  (if eb-pdf-view-mode
      (if (eb-look--theme-dark-p)
          (pdf-view-themed-minor-mode 1)
        (pdf-view-themed-minor-mode -1))
    (pdf-view-themed-minor-mode -1)))

(provide 'eb-pdf-view)
