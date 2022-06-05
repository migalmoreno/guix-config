;; -*- lexical-binding: t; -*-
(require 'ement)
(require 'telega)
(require 'consult)
(require 'password-store)
(require 'erc)
(require 'erc-status-sidebar)

(defgroup eb-chat nil
  "Chat-oriented applications and customizations."
  :group 'eb)

(autoload #'erc-buffer-list "erc")

;;;###autoload
(defvar eb-chat-erc-buffer-source
  `(:name "ERC"
          :narrow ?e
          :category buffer
          :state ,#'consult--buffer-state
          :items ,(lambda () (mapcar #'buffer-name (erc-buffer-list))))
  "Source for ERC buffers to be set in `consult-buffer-sources'.")

(autoload #'telega-root--buffer "telega")

;;;###autoload
(defvar eb-chat-telega-buffer-source
  `(:name "Telega"
          :narrow ?t
          :category buffer
          :state ,#'consult--buffer-state
          :items ,(lambda () (mapcar #'buffer-name
                                     (append (list (telega-root--buffer)) (telega-chat-buffers)))))
  "Source for Telega buffers to be set in `consult-buffer-sources'.")

;;;###autoload
(defun eb-chat-ement-connect ()
  "Connect to Ement with personal credentials."
  (interactive)
  (ement-connect
   :user-id (password-store-get-field "chat/matrix" "username")
   :password (password-store-get "chat/matrix")
   :uri-prefix "http://localhost:8010"))

(defun eb-media-ement-org-compose-message ()
  "Splits current ement window and invokes `ement-room-compose-message'."
  (interactive)
  (when ement-room
    (select-window (split-window nil 200 t))
    (funcall-interactively #'ement-room-compose-message ement-room ement-session)))

;;;###autoload
(defun eb-chat-erc-connect ()
  "Connects to IRC networks via TLS."
  (interactive)
  (erc-tls
   :server "irc.libera.chat"
   :port 6697
   :nick (password-store-get-field "chat/irc/libera" "username")
   :password (password-store-get "chat/irc/libera"))
  (erc-tls
   :server "irc.oftc.net"
   :port 6697
   :nick (password-store-get-field "chat/irc/oftc" "username")))

;;;###autoload
(defun eb-chat-erc-bouncer-connect-libera ()
  "Connects to Libera via an IRC bouncer."
  (interactive)
  (setq erc-email-userid (format "%s/irc.libera.chat"
                                 (password-store-get-field "chat/irc/libera" "username")))
  (erc-tls
   :server "chat.sr.ht"
   :port 6697
   :nick (password-store-get-field "chat/irc/chat.sr.ht" "username")
   :password (password-store-get "chat/irc/chat.sr.ht")))

(defun eb-chat-erc-status-sidebar-toggle ()
  "Toggles the status sidebar killing its buffer when closed."
  (interactive)
  (if (get-buffer-window erc-status-sidebar-buffer-name nil)
      (progn
        (erc-status-sidebar-close)
        (kill-buffer erc-status-sidebar-buffer-name))
    (erc-status-sidebar-open)))

(defun eb-chat-erc-window-reuse-condition (buf-name action)
  "Sets up a condition for ERC buffers to be reused."
  (with-current-buffer buf-name
    (when (eq major-mode 'erc-mode)
      (not action))))

;;;###autoload
(defun eb-chat-erc-bouncer-connect-oftc ()
  "Connects to OFTC via an IRC bouncer."
  (interactive)
  (setq erc-email-userid (format "%s/irc.oftc.net"
                                 (password-store-get-field "chat/irc/oftc" "username")))
  (erc-tls
   :server "chat.sr.ht"
   :port 6697
   :nick (password-store-get-field "chat/irc/chat.sr.ht" "username")
   :password (password-store-get "chat/irc/chat.sr.ht")))

;;;###autoload
(defun eb-chat-erc-close-buffers ()
  "Closes all ERC buffers when ERC server is closed by killing a buffer."
  (interactive)
  (mapc #'kill-buffer (erc-buffer-list nil erc-server-process)))

(provide 'eb-chat)
