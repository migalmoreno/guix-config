;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(defgroup eb-files nil
  "File editing tools."
  :group 'eb)

;;;###autoload
(defun eb-files--list-pdf-buffers ()
  "List all currently-opened `pdf-view-mode' buffers."
  (cl-remove-if-not
   (lambda (buffer)
     (with-current-buffer buffer
       (derived-mode-p 'pdf-view-mode)))
   (buffer-list)))

(defun eb-files--parse-sconfig-hosts ()
  "Parses SSH configuration file and returns a list of its host defebions."
  (when-let ((file (expand-file-name "~/.ssh/config")))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (goto-char (point-min))
      (delete
       nil
       (cl-loop
        while (not (eobp))
        when (re-search-forward
              (rx (: bol (* (any "\s\t")) "Host" space
                     (group (+ (any "a-z" "A-Z" "0-9" "_.%-*")))))
              (point-at-eol) t)
        collect (match-string 1)
        unless (> (skip-chars-forward "\t") 0)
        do (forward-line 1))))))

(defun eb-files--with-tramp (read command &rest args)
  "Execute COMMAND with ARGS in TRAMP session and
read remote directory/file depending on READ."
  (let* ((host (completing-read "SSH host: " (eb-files--parse-sconfig-hosts)))
         (action
          (pcase read
            ('dir (read-directory-name
                   (format "Directory (%s): " host)
                   (format "/-:%s:" host)))
            ('file (read-file-name (format "File (%s): " host) (format "/-:%s:" host)))
            (_ (format "/-:%s:" host))))
         (default-directory action))
    (if args
        (apply command args)
      (if read
          (funcall command action)
        (funcall command)))))

;;;###autoload
(defun eb-files-shell (&optional arg)
  "Opens a shell buffer inside a TRAMP host."
  (interactive "P")
  (eb-files--with-tramp nil #'shell arg))

;;;###autoload
(defun eb-files-eshell (&optional arg)
  "Opens a eshell buffer inside a TRAMP host."
  (interactive "P")
  (eb-files--with-tramp nil #'eshell arg))

;;;###autoload
(defun eb-files-dired ()
  "Quickly opens a Dired buffer in a TRAMP host."
  (interactive)
  (eb-files--with-tramp 'dir #'dired))

;;;###autoload
(defun eb-files-find-file ()
  "Quickly opens a file in a TRAMP host."
  (interactive)
  (eb-files--with-tramp 'file #'find-file))

;;;###autoload
(defun eb-files-open-externally ()
  "Opens files in Dired through their corresponding external program."
  (interactive)
  (require 'consult)
  (let ((files (dired-get-marked-files)))
    (mapc #'consult-file-externally files)))

(provide 'eb-files)
