((nil . ((eval . (setq-local compile-command "make home"))))
 (scheme-mode . ((eval . (with-eval-after-load 'geiser-guile
                           (let ((project-root (directory-file-name
                                                (locate-dominating-file default-directory ".dir-locals.el"))))
                             (unless (member project-root geiser-guile-load-path)
                               (setq-local geiser-guile-load-path (cons project-root geiser-guile-load-path))))))
                 (eval . (add-to-list 'geiser-guile-load-path "~/.config/guix/current/share/guile/site/3.0"))
                 (eval . (add-to-list 'geiser-guile-load-path "~/src/guile/guix/"))
                 (eval . (add-to-list 'geiser-guile-load-path "~/src/guile/rde/src/"))
                 (eval . (add-to-list 'geiser-guile-load-path "~/src/guile/nonguix/"))
                 (eval . (put 'with-eval-after-load 'scheme-indent-function 1))
                 (eval . (put 'dolist 'scheme-indent-function 1))
                 (eval . (put 'embark-define-keymap 'scheme-indent-function 1)))))
