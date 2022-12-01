(define-module (conses features messaging)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (slack-account
            slack-account?
            slack-account-workspace
            slack-account-token
            slack-account-cookie
            feature-slack-settings
            feature-emacs-slack))

(define-configuration/no-serialization slack-account
  (workspace
   (string #f)
   "The Slack workspace to authenticate with. It should take the subdomain
of the complete host, as in @code{clojurians} for @code{\"clojurians.slack.com\"}.")
  (token
   (string #f)
   "The API token for your Slack account. See
@uref{https://github.com/yuya373/emacs-slack#how-to-get-token-and-cookie} for how to retrieve it.")
  (cookie
   (maybe-string #f)
   "The browser cookie used when retrieving your API token. See
@uref{https://github.com/yuya373/emacs-slack#how-to-get-token-and-cookie} for instructions on
how to retrieve it. This is only needed for tokens that start with @code{xoxc}."))

(define (list-of-slack-accounts? lst)
  (and (list? lst) (not (null? lst)) (every slack-account? lst)))

(define* (feature-slack-settings
          #:key
          (slack-accounts #f))
  (ensure-pred list-of-slack-accounts? slack-accounts)

  (feature
   (name 'slack-settings)
   (values (append
            `((slack-settings . #t)
              (slack-accounts . ,slack-accounts))))))

(define* (feature-emacs-slack
          #:key
          (emacs-slack emacs-slack)
          (slack-key "s"))
  "Configure the Slack.el Emacs client."
  (ensure-pred file-like? emacs-slack)
  (ensure-pred string? slack-key)

  (define emacs-f-name 'slack)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to the Slack.el client."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'cl-seq)
        (require 'configure-rde-keymaps)
        (defgroup configure-slack nil
          "Utilities for slack.el, the Emacs Slack client."
          :group 'configure)
        (cl-defstruct configure-slack-team workspace token cookie)
        (defcustom configure-slack-teams '()
          "List of `configure-slack-team' structs that hold Slack accounts."
          :type '(repeat configure-slack-team)
          :group 'configure-slack)
        (defvar rde-slack-map nil
          "Map to bind `slack' commands under.")
        (define-prefix-command 'rde-slack-map)

        ,@(if (get-value 'emacs-consult-initial-narrowing? config)
              '((defvar configure-slack-buffer-source
                  `(:name "Slack"
                          :narrow ?s
                          :category buffer
                          :preview-key ,(kbd "M-.")
                          :state ,'consult--buffer-state
                          :items ,(lambda ()
                                    (mapcar 'buffer-name (configure-completion--mode-buffers
                                                          'slack-message-buffer-mode
                                                          'slack-thread-message-buffer-mode))))
                  "Source for Slack buffers to be set in `consult-buffer-sources'.")
                (add-to-list 'consult-buffer-sources configure-slack-buffer-source)
                (add-to-list 'configure-completion-initial-narrow-alist '(slack-message-buffer-mode . ?s))
                (add-to-list 'configure-completion-initial-narrow-alist '(slack-thread-message-buffer-mode . ?s)))
              '())

        (defun configure-slack-connect (team)
          "Connect to Slack TEAM with personal credentials."
          (interactive
           (list (cl-find (completing-read "Team: " (mapcar 'configure-slack-team-workspace
                                                            configure-slack-teams))
                          configure-slack-teams :key 'configure-slack-team-workspace :test 'string=)))
          (slack-register-team
           :name (configure-slack-team-workspace team)
           :token (configure-slack-team-token team)
           :cookie (configure-slack-team-cookie team))
          (slack-change-current-team))

        (setq configure-slack-teams
              (list
               ,@(map
                  (lambda (slack-acc)
                    `(make-configure-slack-team
                      :workspace ,(slack-account-workspace slack-acc)
                      :token ,(slack-account-token slack-acc)
                      :cookie ,(or (slack-account-cookie slack-acc) 'nil)))
                  (get-value 'slack-accounts config))))
        (with-eval-after-load 'slack
          (setq slack-buffer-emojify t)
          (setq slack-prefer-current-team t)
          (setq slack-buffer-function 'switch-to-buffer)
          (define-key rde-app-map (kbd ,slack-key) 'rde-slack-map)
          (let ((map rde-slack-map))
            (define-key map "c" 'configure-slack-connect)
            (define-key map "s" 'slack-channel-select)
            (define-key map "t" 'slack-change-current-team))
          (set-face-attribute 'slack-preview-face nil :background 'unspecified)))
      #:elisp-packages (list emacs-slack
                             (get-value 'emacs-configure-rde-keymaps config)))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-slack)))
   (home-services-getter get-home-services)))
