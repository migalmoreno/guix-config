;; -*- lexical-binding: t; -*-
(require 'consult)
(require 'cl-lib)

(defgroup eb-project nil
  "Personal `project.el' additions."
  :group 'eb)

(cl-defmethod project-root ((project (head explicit)))
  (cdr project))

;;;###autoload
(defun eb-project-custom-root (dir)
  "Add a custom project root function."
  (let ((root (locate-dominating-file dir ".project.el")))
    (and root (cons 'explicit root))))

;;;###autoload
(defun eb-project-add-commands (commands)
  "Bind COMMANDS in `project-prefix-map' and adds new entries
to `project-switch-commands'."
  (cl-loop for (key doc action) in commands
           do (progn
                (define-key project-prefix-map (format "%c" key) action)
                (add-to-list 'project-switch-commands `(,action ,doc)))))

;;;###autoload
(defun eb-project-ripgrep ()
  "Run `consult-ripgrep' in the current project root."
  (interactive)
  (when-let ((default-dir (project-root (project-current t))))
    (consult-ripgrep default-dir)))

;;;###autoload
(defun eb-project-org-capture ()
  "Run `org-capture' in the current project root."
  (interactive)
  (when-let ((default-dir (project-root (project-current t))))
    (dir-locals-read-from-dir default-dir)
    (org-capture)))

;;;###autoload
(defun eb-project-compile (&optional comint)
  "Compile the current project and if COMINT, the buffer will be
in Comint mode."
  (interactive "P")
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function)))
    (call-interactively #'compile nil (and comint (vector (list 4))))))

(provide 'eb-project)
