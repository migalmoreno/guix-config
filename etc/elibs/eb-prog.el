;; -*- lexical-binding: t; -*-
(require 'consult)
(require 'tuareg)
(require 'merlin)

(defgroup eb-prog nil
  "General programming languages modes and tools."
  :group 'eb)

(defcustom eb-prog-configuration-project (expand-file-name "~/src/dotfiles")
  "Project used to make changes to the system configuration."
  :type 'string
  :group 'eb-prog)

;;;###autoload
(defun eb-prog--add-project-commands (commands)
  "Binds COMMANDS in `project-prefix-map' and adds new entries
to `project-switch-commands'."
  (cl-loop for (key doc action) in commands
           do (progn
                (define-key project-prefix-map (format "%c" key) action)
                (add-to-list 'project-switch-commands `(,action ,doc)))))

;;;###autoload
(defun eb-prog-project-ripgrep ()
  "Run `consult-ripgrep' in the current project root."
  (interactive)
  (when-let ((default-dir (project-root (project-current t))))
    (consult-ripgrep default-dir)))

;;;###autoload
(defun eb-prog-project--compile-configuration ()
  "Compiles `eb-prog-configuration-project'."
  (let ((default-directory eb-prog-configuration-project)
        (compilation-read-command nil)
        (compile-command "make home")
        (display-buffer-alist '(("\\*compilation\\*"
                                 (display-buffer-no-window)))))
    (call-interactively #'compile)))

;;;###autoload
(defun eb-prog-project-compile (&optional comint)
  "Compiles the current project and if COMINT, the buffer will be
in Comint mode."
  (interactive "P")
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function)))
    (call-interactively #'compile nil (and comint (vector (list 4))))))

;;;###autoload
(defun eb-prog-org-capture ()
  "Run `org-capture' in the current project root."
  (interactive)
  (when-let ((default-dir (project-root (project-current t))))
    (dir-locals-read-from-dir default-dir)
    (org-capture)))

(cl-defmethod project-root ((project (head explicit)))
  (cdr project))

;;;###autoload
(defun eb-prog-project-custom-root (dir)
  "Adds a custom root find function for projects."
  (let ((root (locate-dominating-file dir ".project.el")))
    (and root (cons 'explicit root))))

(defun eb-prog-ocaml-load-merlin ()
  "Sets up `merlin-mode' for OCaml."
  (let ((opam-share (when (executable-find "opam")
                      (car (process-lines "opam" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
      (autoload #'merlin-mode "merlin" nil t nil))
    (add-hook 'tuareg-mode-hook #'merlin-mode)))

(defun eb-prog-ocaml-set-environment ()
  "Sets OPAM environment variables."
  (when (executable-find "opam")
    (dolist (var (car (read-from-string
                       (shell-command-to-string "opam config env --sexp"))))
      (setenv (car var) (cadr var)))))

;;;###autoload
(defun eb-prog-output-embellisher ()
  "Embellishes output codes from `comint-mode' buffers."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;;;###autoload
(defun eb-prog--truncate-lines ()
  "Truncates long lines in buffers."
  (setq-local truncate-lines t))

;;;###autoload
(define-minor-mode eb-prog-ocaml-mode
  "Sets up convenient tweaks for `tuareg-mode' and `merlin-mode'. Namely,
it prevents a leading star from appearing on each comment line
in a multi-line OCaml comment, and it enables `prettify-symbols-mode'."
  :global t :group 'eb-prog
  (if eb-prog-ocaml-mode
      (progn
        (setq-local comment-style 'multiline
                    comment-continue "   ")
        (when (fboundp #'prettify-symbols-mode)
          (prettify-symbols-mode 1))
        (eb-prog-ocaml-load-merlin)
        (eb-prog-ocaml-set-environment))))

(provide 'eb-prog)
