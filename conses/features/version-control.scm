(define-module (conses features version-control)
  #:use-module (conses packages emacs-xyz)
  #:use-module (conses features web-browsers)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home-services version-control)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages version-control)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (forge-account
            this-forge-account
            feature-forge-settings
            feature-sourcehut
            feature-git))

(define-record-type* <forge-account>
  forge-account make-forge-account
  forge-account?
  this-forge-account
  (id forge-account-id)
  (forge forge-account-forge)
  (username forge-account-username)
  (full-name
   forge-account-full-name
   (thunked)
   (default (forge-account-username this-forge-account)))
  (email forge-account-email)
  (token forge-account-token))

(define (list-of-forge-accounts? lst)
  (and (list? lst) (every forge-account? lst)))

(define* (feature-forge-settings
          #:key
          (forge-accounts #f))
  (ensure-pred list-of-forge-accounts? forge-accounts)

  (feature
   (name 'forge-settings)
   (values `((forge-settings . #t)
             (forge-accounts . ,forge-accounts)))))

(define* (feature-sourcehut
          #:key
          sourcehut-id
          (emacs-srht emacs-srht))
  "Configure Sourcehut, the hacker's forge."
  (ensure-pred file-like? emacs-srht)

  (define f-name 'sourcehut)

  (define (get-home-services config)
    "Return home services related to Sourcehut."
    (require-value 'forge-accounts config)

    (define sourcehut-account
      (car (filter (if sourcehut-id
                       (lambda (forge-acc)
                         (equal? (forge-account-id forge-acc)
                                 sourcehut-id))
                       (lambda (forge-acc)
                         (equal? (forge-account-forge forge-acc)
                                 'sourcehut)))
                   (get-value 'forge-accounts config))))

    (list
     (rde-elisp-configuration-service
      f-name
      config
      `((require 'request)
        (defun configure-sourcehut-repo-id (name)
          "Return the ID associated with the sourcehut repository NAME."
          (interactive "sRepo name: ")
          (let* ((srht-token ,(forge-account-token sourcehut-account))
                 (oauth2-token (concat "Bearer " srht-token))
                 (id (assoc-default
                      'id
                      (assoc-default
                       'repository
                       (assoc-default
                        'me
                        (assoc-default
                         'data
                         (request-response-data
                          (request
                            "https://git.sr.ht/query"
                            :params (list (cons "query" (concat "query { me { repository(name:\"" name "\") { id } } }")))
                            :type "GET"
                            :headers (list (cons "Authorization" oauth2-token))
                            :parser 'json-read
                            :sync t
                            :timeout 2
                            :error (cl-function
                                    (lambda (&key error-thrown &allow-other-keys) (message "Error %S" error-thrown)))))))))))
            id))

        (defun configure-sourcehut-set-readme (id)
          "Export the current file to html and set it as readme for the Sourcehut repo ID."
          (interactive
           (list (if-let* ((project (project-current t))
                           (dir (project-root project))
                           (name (string-match (rx "/" (group (+ (not "/"))) "/" eol) dir)))
                     (configure-sourcehut-repo-id (match-string 1 dir))
                   (call-interactively 'configure-sourcehut-repo-id))))
          (let* ((srht-token ,(forge-account-token sourcehut-account))
                 (oauth2-token (concat "Bearer " srht-token))
                 (readme (if (derived-mode-p 'html-mode)
                             (buffer-substring-no-properties (point-min) (point-max))
                           (org-export-as (org-export-get-backend 'html) nil nil t)))
                 (json-object-type 'hash-table)
                 (json-array-type 'list)
                 (json-key-type 'string)
                 (query (make-hash-table))
                 (variables (make-hash-table)))
            (puthash "id" id variables)
            (puthash "readme" readme variables)
            (puthash
             "query"
             (concat
              "mutation UpdateRepo($id: Int!, $readme: String!) "
              "{ updateRepository(id: $id, input: { readme: $readme }) { id } }")
             query)
            (puthash "variables" variables query)
            (request
              "https://git.sr.ht/query"
              :type "POST"
              :data (json-serialize query)
              :headers (list (cons "Content-Type" "application/json") (cons "Authorization" oauth2-token))
              :parser 'json-read
              :complete (cl-function (lambda (&key symbol-status &allow-other-keys)
                                       (message "Set: %S" symbol-status))))))
        (with-eval-after-load 'srht
          (setq srht-username ,(forge-account-username sourcehut-account))))
      #:elisp-packages (list emacs-srht
                             emacs-request))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-git
          #:key
          primary-forge-account-id
          (git git)
          (git-send-email? #f)
          (git-gpg-sign-key #f)
          (sign-commits? #t)
          (extra-config '())
          (global-ignores '()))
  "Configure Git, a fast, scalable, distributed revision control system."
  (ensure-pred symbol? primary-forge-account-id)
  (ensure-pred any-package? git)
  (ensure-pred boolean? git-send-email?)
  (ensure-pred maybe-string? git-gpg-sign-key)
  (ensure-pred boolean? sign-commits?)
  (ensure-pred alist? extra-config)
  (ensure-pred list? global-ignores)

  (define f-name 'git)

  (define (get-home-services config)
    "Return home services related to the Git VCS."
    (require-value 'forge-accounts config)

    (define main-account (car (filter (lambda (forge-acc)
                                   (equal? (forge-account-id forge-acc)
                                           primary-forge-account-id))
                                 (get-value 'forge-accounts config))))
    (define full-name (forge-account-full-name main-account))
    (define email (forge-account-email main-account))
    (define gpg-sign-key (or git-gpg-sign-key (get-value 'gpg-primary-key config)))
    (define github-user (let ((gh-account (filter (lambda (forge-acc)
                                                    (equal? (forge-account-forge forge-acc)
                                                            'github))
                                                  (get-value 'forge-accounts config))))
                          (unless (nil? gh-account)
                            (forge-account-username (car gh-account)))))

    (append
     (list
      (simple-service
       'home-git-profile-service
       home-profile-service-type
       (if git-send-email?
           (list (list git "send-email"))
           '()))
      (service home-git-service-type
               (home-git-configuration
                (config
                 `((user
                    ((name . ,full-name)
                     (email . ,email)
                     ,@(if sign-commits?
                           `((signing-key . ,gpg-sign-key))
                           '())))
                   (sendemail
                    ((annotate . "yes")
                     (cc . ,email)
                     (thread . #f)))
                   (commit
                    ((gpg-sign . #t)))
                   ,@(if (and github-user (not (unspecified? github-user)))
                         `((github
                            ((user . ,github-user))))
                         '())))
                (ignore global-ignores)))
      (rde-elisp-configuration-service
       f-name
       config
       `((define-key ctl-x-map "g" 'magit)
         (add-hook 'magit-mode-hook 'toggle-truncate-lines)
         ,@(if (get-value 'emacs-project config)
               '((define-key project-prefix-map "m" 'magit-status)
                 (add-to-list 'project-switch-commands '(magit-status "Show Magit Status")))
               '())
         (with-eval-after-load 'magit
           (require 'git-email-magit)
           (define-key magit-mode-map "q" 'magit-kill-this-buffer)
           (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
           (setq magit-pull-or-fetch t)
           (magit-define-popup-switch 'magit-branch-popup ?o "Create an orphan branch" "--orphan")
           (require 'forge))
         (with-eval-after-load 'vc
           (setq vc-follow-symlinks t)
           (setq vc-ignore-dir-regexp (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp)))
         (with-eval-after-load 'ediff
           (setq ediff-window-setup-function 'ediff-setup-windows-plain))
         (define-key vc-prefix-map "W" 'git-email-format-patch)
         (with-eval-after-load 'git-email
           (require 'git-email-magit)
           (setq git-email-format-patch-default-args "-o ~/src/patches")
           ,@(if (get-value 'emacs-gnus config)
                 '((git-email-gnus-mode 1))
                 '())))
       #:elisp-packages (list
                         emacs-magit
                         emacs-forge
                         emacs-git-email)))
     (if (get-value 'nyxt config)
         (list
          (rde-nyxt-configuration-service
           f-name
           config
           `((define-command-global magit-clone ()
               "Clone and open the current repository with Magit."
               (let ((path (sera:path-join
                            (user-homedir-pathname)
                            "src/"
                            (car (last
                                  (str:split
                                   "/" (quri:uri-path (url (current-buffer)))))))))
                 (if (uiop:directory-exists-p path)
                     (echo "Error: Directory ~a already exists." (namestring path))
                     (eval-in-emacs
                      '(require 'magit)
                      `(magit-clone-internal
                        ,(quri:render-uri (url (current-buffer)))
                        ,(namestring path)
                        nil))))))))
         '())))

  (feature
   (name f-name)
   (values (make-feature-values f-name git-send-email?
                                primary-forge-account-id))
   (home-services-getter get-home-services)))
