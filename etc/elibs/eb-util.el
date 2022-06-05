;; -*- lexical-binding: t; -*-
(require 'cl-lib)

;;;###autoload
(defun eb-to-hook (hook &rest funs)
  "Adds functions FUNS to hook HOOK."
  (cl-loop for fun in funs
           do (add-hook (if (not (string-match-p "-hook" (symbol-name hook)))
                            (intern (concat (symbol-name hook) "-hook"))
                          hook)
                        fun)))

;;;###autoload
(defun eb-to-hooks (fun &rest hooks)
  "Adds function FUN to hooks HOOKS."
  (cl-loop for hook in hooks
           do (add-hook (if (not (string-match-p "-hook" (symbol-name hook)))
                            (intern (concat (symbol-name hook) "-hook"))
                          hook)
                        fun)))

;;;###autoload
(defun eb-auto-modes (modes)
  "Adds multiple MODES to `auto-mode-alist'."
  (cl-loop for mode in modes
           do (map-put! auto-mode-alist (car mode) (cdr mode))))

;;;###autoload
(defun eb-local-keys (map key def &rest bindings)
  "Allow defining many local bindings at once."
  (while key
    (define-key map (if (equal (type-of key) 'string) (kbd key) key) def)
    (setq key (pop bindings)
          def (pop bindings))))

;;;###autoload
(defun eb-faces (faces)
  "Defines multiple FACES at once."
  (cl-loop for face in faces
           do (apply #'set-face-attribute face)))

;;;###autoload
(defun eb-fonts (fonts)
  "Defines multiple FONTS at once."
  (cl-loop for font in fonts
           do (apply #'set-fontset-font font)))

(provide 'eb-util)
