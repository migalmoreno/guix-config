(define-module (quasar home services chat)
  #:use-module (conses home services emacs)
  #:use-module (conses home services matrix)
  #:use-module (conses packages emacs-xyz)
  #:use-module (gnu home-services base)
  #:use-module (gnu services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:export (matrix-service
            irc-service
            telegram-service
            slack-service))

(define (matrix-service)
  (list
   (service home-pantalaimon-service-type
            (home-pantalaimon-configuration
             (config
              `((Default
                  ((log-level . debug)))
                (local-matrix
                 ((homeserver . ,(string-append "https://matrix." (getenv "DOMAIN")))
                  (listen-address . localhost)
                  (listen-port . 8009)
                  (ssl . #f)
                  (ignore-verification . #t)
                  (use-keyring . #f)))))))
   (elisp-configuration-service
    `((let ((map mode-specific-map))
        (define-key map "ed" 'ement-disconnect)
        (define-key map "ee" 'eb-ement-connect))
      (with-eval-after-load 'eb-ement
        (setq eb-ement-username (password-store-get-field "chat/matrix" "username")
              eb-ement-password (password-store-get "chat/matrix")))
      (with-eval-after-load 'ement
        (let ((map ement-room-mode-map))
          (define-key map "c" 'ement-room-compose-message)
          (define-key map "E" 'ement-room-edit-message))
        (custom-set-variables
         '(ement-room-send-message-filter 'ement-room-send-org-filter)
         '(ement-save-session t)
         '(ement-room-send-read-receipts nil)
         '(warning-suppress-log-types
           '((ement-room-send-event-callback)
             (ement-room-send-event-callback))))))
    #:elisp-packages (list emacs-ement-next))))

(define (irc-service)
  (list
   (elisp-configuration-service
    `((let ((map mode-specific-map))
        (define-key map "Ic" 'eb-erc-connect)
        (define-key map "Il" 'eb-erc-bouncer-connect-libera)
        (define-key map "Io" 'eb-erc-bouncer-connect-oftc))
      (with-eval-after-load 'eb-erc
        (setq eb-erc-bouncer-nick (password-store-get-field "chat/irc/chat.sr.ht" "username")
              eb-erc-bouncer-password (password-store-get "chat/irc/chat.sr.ht")
              eb-erc-oftc-nick (password-store-get-field "chat/irc/oftc.net" "username")
              eb-erc-oftc-password (password-store-get "chat/irc/oftc.net")
              eb-erc-libera-nick (password-store-get-field "chat/irc/libera.chat" "username")
              eb-erc-libera-password (password-store-get "chat/irc/libera.chat")))
      (with-eval-after-load 'erc
        (let ((map erc-mode-map))
          (define-key map (kbd "C-c C-q") 'eb-erc-close-buffers)
          (define-key map (kbd "C-c C-t") 'eb-erc-toggle-timestamps)
          (define-key map (kbd "C-c C-s") 'eb-erc-status-sidebar-toggle))
        (dolist (module '(keep-place services notifications hl-nicks image spelling log))
                (add-to-list 'erc-modules module))
        (erc-update-modules)
        (require 'erc-image)
        (require 'erc-hl-nicks)
        (require 'erc-join)
        (erc-autojoin-enable)
        (erc-spelling-mode 1)
        (erc-fill-disable)
        (custom-set-variables
         '(erc-server "irc.libera.chat")
         '(erc-default-port 6697)
         '(erc-user-full-name nil)
         '(erc-hide-list '("NICK" "JOIN" "PART" "QUIT" "MODE" "AWAY"))
         '(erc-hide-prompt t)
         '(erc-hide-timestamps t)
         '(erc-echo-timestamps nil)
         '(erc-kill-buffer-on-part t)
         '(erc-kill-server-buffer-on-quit t)
         '(erc-kill-queries-on-quit t)
         '(erc-rename-buffers t)
         '(erc-auto-query 'bury)
         '(erc-header-line-format nil)
         '(erc-query-display 'buffer)
         '(erc-join-buffer 'bury)
         '(erc-timestamp-format "%H:%M")
         '(erc-prompt-for-password nil)))
      ,#~""
      (add-to-list 'display-buffer-alist
                   '(eb-erc-window-reuse-condition .
                     (display-buffer-reuse-mode-window
                      (inhibit-same-window . t)
                      (inhibit-switch-frame . t)
                      (mode . erc-mode))))
      (with-eval-after-load 'erc-status-sidebar
        (advice-add 'erc-status-sidebar-default-chan-format :around 'eb-erc-status-add-padding)
        (setq erc-status-sidebar-header-line-format
              (concat " " erc-status-sidebar-mode-line-format))
        (custom-set-variables
         '(erc-status-sidebar-width 22)
         '(erc-status-sidebar-mode-line-format nil)))
      ,#~""
      (with-eval-after-load 'erc-log
        (custom-set-variables
         '(erc-log-insert-log-on-open t)
         '(erc-log-channels-directory (expand-file-name "erc-logs" (xdg-cache-home)))))
      ,#~""
      (with-eval-after-load 'erc-join
        (custom-set-variables
         '(erc-autojoin-timing 'connect)
         '(erc-autojoin-delay 5)
         '(erc-autojoin-channels-alist '((Libera.Chat
                                          "#nyxt" "#emacs" "#org-mode" "#guix"
                                          "#clojure" "#commonlisp" "#scheme")
                                         (OFTC "#postmarketos")))
         '(erc-autojoin-domain-only t)))
      ,#~""
      (with-eval-after-load 'erc-fill
        (custom-set-variables
         '(erc-fill-function 'erc-fill-static)
         '(erc-fill-static-center 14)
         '(erc-fill-column 82)))
      ,#~""
      (with-eval-after-load 'erc-track
        (custom-set-variables
         '(erc-track-exclude-server-buffer t)
         '(erc-track-enable-keybindings t)
         '(erc-track-shorten-start 8)
         '(erc-track-exclude-types '("324" "329" "JOIN" "MODE" "NICK" "PART" "QUIT"))))
      ,#~""
      (with-eval-after-load 'erc-backends
        (custom-set-variables
         '(erc-server-reconnect-timeout 3)
         '(erc-server-reconnect-attempts t)))
      ,#~""
      (with-eval-after-load 'erc-services
        (custom-set-variables
         '(erc-prompt-for-nickserv-password nil)))
      ,#~""
      (with-eval-after-load 'erc-image
        (custom-set-variables
         '(erc-image-inline-rescale 300))))
    #:elisp-packages (list emacs-erc-image emacs-erc-hl-nicks))))

(define (telegram-service)
  (list
   (elisp-configuration-service
    `((let ((map mode-specific-map))
        (define-key map "t" telega-prefix-map)
        (define-key map "tt" 'telega))
      (add-hook 'telega-load-hook 'telega-notifications-mode)
      (custom-set-variables
       '(telega-directory (expand-file-name "telega" user-emacs-directory)))
      (with-eval-after-load 'telega
        (custom-set-variables
         '(telega-completing-read-function 'completing-read))))
    #:elisp-packages (list emacs-telega))))

(define (slack-service)
  (list
   (elisp-configuration-service
    '((with-eval-after-load 'eb-slack
        (setq eb-slack-workspace (password-store-get-field "chat/slack" "workspace"))
        (setq eb-slack-token (password-store-get-field "chat/slack" "token"))
        (setq eb-slack-cookie (password-store-get-field "chat/slack" "cookie")))
      (with-eval-after-load 'slack
        (custom-set-variables
         '(slack-buffer-emojify t)
         '(slack-prefer-current-team t)
         '(slack-buffer-function 'switch-to-buffer))
        (define-key mode-specific-map "sc" 'slack-channel-select)
        (define-key mode-specific-map "st" 'slack-change-current-team)
        (set-face-attribute 'slack-preview-face nil :background 'unspecified)))
    #:elisp-packages (list emacs-slack-next))))
