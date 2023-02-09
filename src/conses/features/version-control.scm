(define-module (conses features version-control)
  #:use-module (conses packages emacs-xyz)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (rde features web-browsers)
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
  (email forge-account-email))

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
          (sourcehut-id #f)
          (emacs-srht emacs-srht))
  "Configure Sourcehut, the hacker's forge.
SOURCEHUT-ID is the id of the @code{forge-account} that you wish to set
this feature up for."
  (ensure-pred maybe-symbol? sourcehut-id)
  (ensure-pred file-like? emacs-srht)

  (define f-name 'sourcehut)

  (define (get-home-services config)
    "Return home services related to Sourcehut."
    (require-value 'forge-accounts config)

    (define srht-account
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
        (defconst rde-sourcehut-patch-control-codes
          '("PROPOSED" "NEEDS_REVISION" "SUPERSEDED"
            "APPROVED" "REJECTED" "APPLIED")
          "Control codes for SourceHut patches.  See
`rde-message-srht-add-email-control-code' for how to apply them.")

        (defun rde-sourcehut--get-repo-id (name)
          "Return the ID associated with the sourcehut repository NAME."
          (interactive "sRepo name: ")
          (let* ((srht-token (auth-source-pick-first-password
                              :host ,(symbol->string
                                      (forge-account-forge srht-account))
                              :user ,(forge-account-username srht-account)))
                 (oauth2-token (concat "Bearer " srht-token))
                 (id
                  (assoc-default
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
                         :params (list
                                  (cons
                                   "query"
                                   (concat "query { me { repository(name:\""
                                           name "\") { id } } }")))
                         :type "GET"
                         :headers (list (cons "Authorization" oauth2-token))
                         :parser 'json-read
                         :sync t
                         :timeout 2
                         :error (cl-function
                                 (lambda (&key error-thrown &allow-other-keys)
                                   (message "Error %S" error-thrown)))))))))))
            id))

        (defun rde-sourcehut-set-readme (id)
          "Export the current file to html and set it as readme for the
Sourcehut repo ID."
          (interactive
           (list (if-let* ((project (project-current t))
                           (dir (project-root project))
                           (name (string-match
                                  (rx "/" (group (+ (not "/"))) "/" eol) dir)))
                          (rde-sourcehut--get-repo-id (match-string 1 dir))
                   (call-interactively 'rde-sourcehut--get-repo-id))))
          (let* ((srht-token (auth-source-pick-first-password
                              :host ,(symbol->string
                                      (forge-account-forge srht-account))
                              :user ,(forge-account-username srht-account)))
                 (oauth2-token (concat "Bearer " srht-token))
                 (readme (if (derived-mode-p 'html-mode)
                             (buffer-substring-no-properties
                              (point-min) (point-max))
                           (org-export-as
                            (org-export-get-backend 'html) nil nil t)))
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
              "{ updateRepository(id: $id, input: {readme: $readme}) { id } }")
             query)
            (puthash "variables" variables query)
            (request
              "https://git.sr.ht/query"
              :type "POST"
              :data (json-serialize query)
              :headers (list (cons "Content-Type" "application/json")
                             (cons "Authorization" oauth2-token))
              :parser 'json-read
              :complete (cl-function
                         (lambda (&key symbol-status &allow-other-keys)
                           (message "Set: %S" symbol-status))))))

        (defun rde-sourcehut-add-email-control-code (control-code)
          "Add custom header for SourceHut email controls.  The CONTROL-CODE
is among `rde-notmuch-patch-control-codes'."
          (interactive
           (list (completing-read "Select control code: "
                                  rde-sourcehut-patch-control-codes nil t)))
          (if (member control-code rde-sourcehut-patch-control-codes)
              (unless (message-fetch-field "X-Sourcehut-Patchset-Update")
                (message-add-header (format "X-Sourcehut-Patchset-Update: %s"
                                            control-code)))
            (user-error "%s is not specified in
`rde-notmuch-patch-control-codes'" control-code)))

        (with-eval-after-load 'srht
          (setq srht-username ,(forge-account-username srht-account))))
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

    (define main-account
      (car (filter (lambda (forge-acc)
                     (equal? (forge-account-id forge-acc)
                             primary-forge-account-id))
                   (get-value 'forge-accounts config))))
    (define full-name (forge-account-full-name main-account))
    (define email (forge-account-email main-account))
    (define gpg-sign-key
      (or git-gpg-sign-key (get-value 'gpg-primary-key config)))
    (define github-user
      (let ((gh-account (filter (lambda (forge-acc)
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
                   (merge
                    ;; diff3 makes it easier to solve conflicts with smerge, zdiff3
                    ;; should make a conflict scope smaller, but guile-git fails if
                    ;; this option is set.
                    ((conflictStyle . diff3)))
                   (diff
                    ;; histogram should be smarter about diff generation.
                    ((algorithm . histogram)))
                   (commit
                    (,@(if sign-commits?
                           '((gpgsign . #t))
                           '())))
                   ,@(if (and github-user (not (unspecified? github-user)))
                         `((github
                            ((user . ,github-user))))
                         '())
                   (sendemail
                    ((annotate . #t)))

                   ,@extra-config))
                (ignore global-ignores)))
      (if (get-value 'emacs config)
          (list
           (rde-elisp-configuration-service
            f-name
            config
            `((defun rde-git-email--get-current-project ()
                "Return the path of the current project.
Falls back to `default-directory'."
                (let ((dir (or (and (bound-and-true-p projectile-known-projects)
                                    (projectile-project-root))
                               (and (bound-and-true-p project-list-file)
                                    (if (and (not (consp (project-current)))
                                             (> (length (project-current)) 2))
                                        (car (last (project-current)))
                                      (cdr (project-current))))
                               (vc-root-dir)
                               default-directory)))
                  dir))
              (advice-add 'git-email--get-current-project
                          :override 'rde-git-email--get-current-project)
              (define-key ctl-x-map "g" 'magit)
              (add-hook 'magit-mode-hook 'toggle-truncate-lines)
              ,@(if (get-value 'emacs-project config)
                    '((define-key project-prefix-map "m" 'magit-status)
                      (add-to-list 'project-switch-commands
                                   '(magit-status "Show Magit Status")))
                  '())
              (with-eval-after-load 'magit
                (require 'git-email-magit)
                (define-key magit-mode-map "q" 'magit-kill-this-buffer)
                (setq magit-display-buffer-function
                      'magit-display-buffer-same-window-except-diff-v1)
                (setq magit-pull-or-fetch t)
                (require 'forge))
              (with-eval-after-load 'vc
                (setq vc-follow-symlinks t)
                (setq vc-ignore-dir-regexp
                      (format "%s\\|%s"
                              vc-ignore-dir-regexp tramp-file-name-regexp)))
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
                              emacs-git-email
                              emacs-piem)))
          '()))
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
                                   "/" (quri:uri-path
                                        (url (current-buffer)))))))))
                 (if (uiop:directory-exists-p path)
                     (echo "Error: Directory ~a already exists."
                           (namestring path))
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
