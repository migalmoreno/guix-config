(define-module (quasar home services shells)
  #:use-module (efimerspan home services emacs)
  #:use-module (gnu home-services shells)
  #:use-module (gnu home-services base)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages base)
  #:use-module (gnu packages shellutils)
  #:use-module (guix gexp)
  #:export (shell-service))

(define (shell-service)
  (list
   (home-generic-service 'home-shell-packages
                         #:packages (list
                                     gnu-make
                                     direnv))
   (elisp-configuration-service
    `((with-eval-after-load 'sh-script
        (custom-set-variables
         '(sh-basic-offset 2)
         '(sh-indentation 2)
         '(smie-indent-basic 2)
         '(sh-indent-comment nil)
         '(sh-first-lines-indent nil)))
      ,#~""
      (add-hook 'eshell-mode-hook 'eb-shell-mode-setup)
      (add-hook 'eshell-mode-hook 'eb-shell--set-bookmark-handler)
      (with-eval-after-load 'eshell
        (custom-set-variables
         '(eshell-banner-message "\n\n"))
        (eshell-syntax-highlighting-global-mode)
        (add-hook 'comint-mode 'capf-autosuggest-mode))
      ,#~""
      (with-eval-after-load 'em-prompt
        (autoload 'epe-theme-lambda "eshell-prompt-extras")
        (custom-set-variables
         '(eshell-prompt-function 'epe-theme-lambda)
         '(eshell-highlight-prompt nil)))
      ,#~""
      (add-to-list 'display-buffer-alist
                   '("\\*Async Shell Command\\*.*" .
                     (display-buffer-no-window)))
      ,#~""
      (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
      (add-to-list 'org-babel-load-languages '(shell . t))
      ,#~""
      (add-hook 'after-init-hook 'envrc-global-mode)
      (with-eval-after-load 'envrc
        (define-key envrc-mode-map (kbd "C-c E") 'envrc-command-map)))
    #:elisp-packages (list emacs-eshell-syntax-highlighting
                           emacs-eshell-prompt-extras
                           emacs-envrc))
   (service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)
             (environment-variables
              `(("QT_XCB_GL_INTEGRATION" . "none")
                ("LESSHISTFILE" . "-")
                ("HISTFILE" . "$XDG_DATA_HOME/bash/history")))
             (bashrc
              (list "eval \"$(opam env)\""))))))
