(define-module (conses features irc)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (feature-irc-settings
            feature-emacs-erc
            irc-account
            irc-account?
            irc-account-id
            irc-account-nick
            irc-account-network
            irc-account-bouncer?))

(define-configuration/no-serialization irc-account
  (id
   (symbol #f)
   "A simple identifier for IRC accounts.")
  (network
   (string "irc.libera.chat")
   "The IRC network to connect to.")
  (bouncer?
   (boolean #f)
   "Whether the IRC account should connect to a bouncer server.")
  (nick
   (string #f)
   "The nick that is registered under the IRC network."))

(define (list-of-irc-accounts? lst)
  (and (list? lst) (not (null? lst)) (every irc-account? lst)))

(define* (feature-irc-settings
          #:key
          (irc-accounts #f))
  "Configure IRC accounts."
  (ensure-pred list-of-irc-accounts? irc-accounts)

  (feature
   (name 'irc-settings)
   (values (append
            `((irc-settings . #t)
              (irc-accounts . ,irc-accounts))))))

(define* (feature-emacs-erc
          #:key
          (autojoin-channels-alist '()))
  "Configure the extensible IRC client for Emacs."
  (ensure-pred list? autojoin-channels-alist)

  (define emacs-f-name 'erc)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to ERC."
    (require-value 'irc-accounts config)

    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'erc)
        (require 'erc-status-sidebar)
        (require 'configure-rde-keymaps)

        (defgroup configure-erc nil
          "Extra customizations for ERC."
          :group 'configure)

        (defcustom configure-erc-users '()
          "A list of `configure-erc-user' structs that hold IRC accounts."
          :type 'list
          :group 'configure-erc)

        ,@(if (get-value 'emacs-consult-initial-narrowing? config)
              '((defvar configure-erc-buffer-source
                  `(:name "ERC"
                          :narrow ?i
                          :category buffer
                          :preview-key ,(kbd "M-.")
                          :state ,'consult--buffer-state
                          :items ,(lambda () (mapcar 'buffer-name (erc-buffer-list))))
                  "Source for ERC buffers to be set in `consult-buffer-sources'.")
                (add-to-list 'consult-buffer-sources configure-erc-buffer-source)
                (add-to-list 'configure-completion-initial-narrow-alist '(erc-mode . ?i)))
              '())

        (cl-defstruct configure-erc-user
          "An ERC user."
          id network nick bouncer-p)

        (defun configure-erc-connect (user)
          "Connect USER to IRC network via tls."
          (interactive
           (list (cl-find (intern (completing-read "User: " (mapcar 'configure-erc-user-id
                                                                    configure-erc-users)))
                          configure-erc-users :key 'configure-erc-user-id)))
          (let ((network (configure-erc-user-network user))
                (nick (configure-erc-user-nick user)))
            (when (configure-erc-user-bouncer-p user)
              (let* ((irc-network (completing-read
                                   "Network: "
                                   (mapcar 'configure-erc-user-network
                                           (cl-remove network configure-erc-users
                                                      :key 'configure-erc-user-network
                                                      :test 'string=))))
                     (irc-network-nick (configure-erc-user-nick
                                        (cl-find irc-network configure-erc-users
                                                 :key 'configure-erc-user-network
                                                 :test 'string=))))
                (setq erc-email-userid (format "%s/%s" irc-network-nick irc-network))))
            (erc-tls
             :server network
             :port 6697
             :nick nick
             :password (auth-source-pick-first-password :host network))))

        (defun configure-erc-close-buffers ()
          "Close all ERC buffers when ERC server is closed by killing a buffer."
          (interactive)
          (mapc 'kill-buffer (erc-buffer-list nil erc-server-process)))

        (defun configure-erc-toggle-timestamps ()
          "Refresh the current ERC buffer after toggling the timestamps."
          (interactive)
          (erc-toggle-timestamps)
          (force-window-update (selected-window)))

        (defun configure-erc-window-reuse-condition (buf-name action)
          "Set up a condition for ERC buffers to be reused."
          (with-current-buffer buf-name
            (when (eq major-mode 'erc-mode)
              (not action))))

        (defun configure-erc-status-sidebar-toggle ()
          "Toggle the status sidebar killing its buffer when closed."
          (interactive)
          (if (get-buffer-window erc-status-sidebar-buffer-name nil)
              (progn
                (erc-status-sidebar-close)
                (kill-buffer erc-status-sidebar-buffer-name))
            (erc-status-sidebar-open)))

        (defun configure-erc-status-add-padding (fun channame &optional num-messages erc-face)
          "Add left padding on the sidebar formatted channels list."
          (concat " " (funcall fun channame num-messages erc-face)))

        (define-key rde-app-map "i" 'configure-erc-connect)
        (setq configure-erc-users
              (list
               ,@(map
                  (lambda (irc-acc)
                    `(make-configure-erc-user
                      :id ',(irc-account-id irc-acc)
                      :network ,(irc-account-network irc-acc)
                      :nick ,(irc-account-nick irc-acc)
                      :bouncer-p ,(if (irc-account-bouncer? irc-acc) 't 'nil)))
                  (get-value 'irc-accounts config))))
        (autoload 'erc-buffer-list "erc")
        (with-eval-after-load 'erc
          (let ((map erc-mode-map))
            (define-key map (kbd "C-c C-q") 'configure-erc-close-buffers)
            (define-key map (kbd "C-c C-t") 'configure-erc-toggle-timestamps)
            (define-key map (kbd "C-c C-s") 'configure-erc-status-sidebar-toggle))
          (dolist (module '(keep-place services notifications hl-nicks image spelling log))
            (add-to-list 'erc-modules module))
          (erc-update-modules)
          (require 'erc-image)
          (require 'erc-hl-nicks)
          (require 'erc-join)
          (erc-autojoin-enable)
          (erc-spelling-mode 1)
          (erc-fill-disable)
          (setq erc-server "irc.libera.chat")
          (setq erc-default-port 6697)
          (setq erc-user-full-name nil)
          (setq erc-hide-list '("NICK" "JOIN" "PART" "QUIT" "MODE" "AWAY"))
          (setq erc-hide-prompt t)
          (setq erc-hide-timestamps t)
          (setq erc-echo-timestamps nil)
          (setq erc-kill-buffer-on-part t)
          (setq erc-kill-server-buffer-on-quit t)
          (setq erc-kill-queries-on-quit t)
          (setq erc-rename-buffers t)
          (setq erc-auto-query 'bury)
          (setq erc-header-line-format nil)
          (setq erc-query-display 'buffer)
          (setq erc-join-buffer 'bury)
          (setq erc-timestamp-format "%H:%M")
          (setq erc-prompt-for-password nil))
        (add-to-list
         'display-buffer-alist
         '(configure-erc-window-reuse-condition . (display-buffer-reuse-mode-window
                                                   (inhibit-same-window . t)
                                                   (inhibit-switch-frame . t)
                                                   (mode . erc-mode))))
        (with-eval-after-load 'erc-status-sidebar
          (advice-add 'erc-status-sidebar-default-chan-format :around 'configure-erc-status-add-padding)
          (setq erc-status-sidebar-header-line-format
                (concat " " erc-status-sidebar-mode-line-format))
          (setq erc-status-sidebar-width 22)
          (setq erc-status-sidebar-mode-line-format nil))
        (with-eval-after-load 'erc-log
          (setq erc-log-insert-log-on-open t)
          (setq erc-log-channels-directory (expand-file-name "erc-logs" (xdg-cache-home))))
        (with-eval-after-load 'erc-join
          (setq erc-autojoin-timing 'connect)
          (setq erc-autojoin-delay 5)
          (setq erc-autojoin-channels-alist ',autojoin-channels-alist)
          (setq erc-autojoin-domain-only t))
        (with-eval-after-load 'erc-fill
          (setq erc-fill-function 'erc-fill-static)
          (setq erc-fill-static-center 14)
          (setq erc-fill-column 82))
        (with-eval-after-load 'erc-track
          (setq erc-track-exclude-server-buffer t)
          (setq erc-track-enable-keybindings t)
          (setq erc-track-shorten-start 8)
          (setq erc-track-exclude-types '("324" "329" "JOIN" "MODE" "NICK" "PART" "QUIT")))
        (with-eval-after-load 'erc-backends
          (setq erc-server-reconnect-timeout 3)
          (setq erc-server-reconnect-attempts t))
        (with-eval-after-load 'erc-services
          (setq erc-prompt-for-nickserv-password nil))
        (with-eval-after-load 'erc-image
          (setq erc-image-inline-rescale 100)))
      #:elisp-packages (list emacs-erc-image
                             emacs-erc-hl-nicks
                             (get-value 'emacs-configure-rde-keymaps config))
      #:summary "ERC helpers"
      #:commentary "Provide helpers to ERC, the extensible IRC client.")))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))
