(define-module (quasar home services version-control)
  #:use-module (quasar home)
  #:use-module (efimerspan home services emacs)
  #:use-module (efimerspan home services web-browsers)
  #:use-module (gnu services)
  #:use-module (gnu home-services version-control)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:export (git-service))

(define (git-service)
  (list
   (service home-git-service-type
            (home-git-configuration
             (config
              `((user
                 ((name . ,(password-store-get "vc/github/nickname"))
                  (email . ,(password-store-get "vc/github/username"))))
                (core
                 ((editor . emacsclient)
                  (excludesfile . "~/.config/git/gitignore")))
                (github
                 ((user . ,(password-store-get "vc/github/nickname"))))))
             (ignore
              '("**/.direnv"
                "node_modules"
                ".log"))))
   (elisp-configuration-service
    `((define-key ctl-x-map "g" 'magit)
      (with-eval-after-load 'magit
        (define-key magit-mode-map "q" 'magit-kill-this-buffer)
        (custom-set-variables
         '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))
        (magit-define-popup-switch 'magit-branch-popup ?o "Create an orphan branch" "--orphan")
        (require 'forge))
      ,#~""
      (with-eval-after-load 'vc
        (custom-set-variables
         '(vc-follow-symlinks t)
         '(vc-ignore-dir-regexp (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))))
      ,#~""
      (with-eval-after-load 'ediff
        (custom-set-variables
         '(ediff-window-setup-function 'ediff-setup-windows-plain))))
    #:elisp-packages (list emacs-magit emacs-forge))
   (nyxt-configuration-service
    `((define-command-global magit-clone ()
        "Clones the current repository with Magit."
        (let ((path (sera:path-join
                     (user-homedir-pathname)
                     "src/"
                     (car (last
                           (str:split
                            "/" (quri:uri-path (url (current-buffer)))))))))
          (if (uiop:directory-exists-p path)
              (echo "Error: Directory ~a already exists." (namestring path))
              (eval-in-emacs
               `(magit-clone-internal
                 ,(quri:render-uri (url (current-buffer)))
                 ,(namestring path)
                 nil)))))))))
