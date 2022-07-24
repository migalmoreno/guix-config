(define-module (quasar home services version-control)
  #:use-module (quasar home)
  #:use-module (conses home services emacs)
  #:use-module (conses home services web-browsers)
  #:use-module (conses packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages version-control)
  #:use-module (gnu home-services version-control)
  #:use-module (gnu home-services base)
  #:use-module (guix gexp)
  #:export (git-service))

(define (git-service)
  (list
   (home-generic-service 'home-git-packages #:packages
                         (list (list git "send-email")))
   (service home-git-service-type
            (home-git-configuration
             (config
              `((user
                 ((name . ,(getenv "SOURCEHUT_USERNAME"))
                  (email . ,(getenv "SOURCEHUT_EMAIL"))
                  (signing-key . ,(getenv "GPG_PUBLIC_KEY"))))
                (sendemail
                 ((annotate . "yes")))
                (commit
                 ((gpg-sign . #t)))
                (github
                 ((user . ,(getenv "GITHUB_USERNAME"))))))
             (ignore
              '("**/.direnv"
                "node_modules"
                "*.elc"
                ".log"))))
   (elisp-configuration-service
    `((define-key ctl-x-map "g" 'magit)
      (add-hook 'magit-mode-hook 'toggle-truncate-lines)
      (with-eval-after-load 'magit
        (define-key magit-mode-map "q" 'magit-kill-this-buffer)
        (custom-set-variables
         '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
         '(magit-pull-or-fetch t))
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
         '(ediff-window-setup-function 'ediff-setup-windows-plain)))
      ,#~""
      (with-eval-after-load 'srht
        (custom-set-variables
         '(srht-username (password-store-get-field "vc/sourcehut" "username"))))
      ,#~""
      (with-eval-after-load 'git-email
        (custom-set-variables
         '(git-email-format-patch-default-args "-o ~/src/patches"))
        (define-key vc-prefix-map "W" 'git-email-format-patch)
        (git-email-gnus-mode 1))
      ,#~""
      (with-eval-after-load 'eb-vc
        (custom-set-variables
         '(eb-vc-srht-token (password-store-get-field "vc/sourcehut" "oauth2-token")))))
    #:elisp-packages (list
                      emacs-magit
                      emacs-forge
                      emacs-srht
                      emacs-git-email))
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
