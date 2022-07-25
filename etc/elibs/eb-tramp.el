;; -*- lexical-binding: t; -*-
(require 'cl-lib)

(defgroup eb-tramp nil
  "Personal TRAMP utilities."
  :group 'eb)

(defun eb-tramp--parse-sconfig-hosts ()
  "Parse SSH configuration file and return a list of host definitions."
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

(defun eb-tramp-run-with-tramp (read command &rest args)
  "Execute COMMAND with ARGS in TRAMP session and
read remote directory/file depending on READ."
  (let* ((host (completing-read "SSH host: " (eb-tramp--parse-sconfig-hosts)))
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
(defun eb-tramp-shell (&optional arg)
  "Opens a shell buffer inside a TRAMP host."
  (interactive "P")
  (eb-tramp-run-with-tramp nil #'shell arg))

;;;###autoload
(defun eb-tramp-eshell (&optional arg)
  "Opens an eshell buffer inside a TRAMP host."
  (interactive "P")
  (eb-tramp-run-with-tramp nil #'eshell arg))

;;;###autoload
(defun eb-tramp-dired ()
  "Opens a Dired buffer inside a TRAMP host."
  (interactive)
  (eb-tramp-run-with-tramp 'dir #'dired))

;;;###autoload
(defun eb-tramp-find-file ()
  "Opens a file inside a TRAMP host."
  (interactive)
  (eb-tramp-run-with-tramp 'file #'find-file))

(provide 'eb-tramp)
