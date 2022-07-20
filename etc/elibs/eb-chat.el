;; -*- lexical-binding: t; -*-
(require 'ement)
(require 'telega)
(require 'consult)
(require 'erc)
(require 'auth-source)
(require 'erc-status-sidebar)
(require 'eb-util)

(defgroup eb-chat nil
  "Chat-oriented applications and customizations."
  :group 'eb)

(defcustom eb-chat-irc-bouncer-nick
  (or (plist-get (car (auth-source-search :host "chat.sr.ht"))
                 :user)
      "")
  "The nick to use for the IRC bouncer connection."
  :type 'string
  :group 'eb-chat)

(defcustom eb-chat-irc-bouncer-password
  (if-let ((fun (plist-get (car (auth-source-search :host "chat.sr.ht"))
                           :secret)))
      (funcall fun)
    "")
  "The password to use for the IRC bouncer connection."
  :type 'string
  :group 'eb-chat)

(defcustom eb-chat-irc-oftc-nick
  (or (plist-get (car (auth-source-search :host "oftc.net"))
                 :user)
      "")
  "The nick to use for the IRC connection to the OFTC network."
  :type 'string
  :group 'eb-chat)

(defcustom eb-chat-irc-libera-nick
  (or (plist-get (car (auth-source-search :host "libera.chat"))
                 :user)
      "")
  "The nick to use for the IRC connection to the Libera.chat network."
  :type 'string
  :group 'eb-chat)

(defcustom eb-chat-irc-libera-password
  (if-let ((fun (plist-get (car (auth-source-search :host "libera.chat"))
                           :secret)))
      (funcall fun)
    "")
  "The password to use for the IRC connection to the Libera.chat network."
  :type 'string
  :group 'eb-chat)

(defcustom eb-chat-ement-username nil
  "The username to use to connect to the Matrix homeserver."
  :type 'string
  :group 'eb-chat)

(defcustom eb-chat-ement-password nil
  "The password to use to connect to the Matrix homeserver."
  :type 'string
  :group 'eb-chat)

(autoload #'erc-buffer-list "erc")

;;;###autoload
(defvar eb-chat-erc-buffer-source
  `(:name "ERC"
          :narrow ?e
          :category buffer
          :preview-key ,(kbd "M-.")
          :state ,#'consult--buffer-state
          :items ,(lambda () (mapcar #'buffer-name (erc-buffer-list))))
  "Source for ERC buffers to be set in `consult-buffer-sources'.")

(autoload #'telega-root--buffer "telega")

;;;###autoload
(defvar eb-chat-telega-buffer-source
  `(:name "Telega"
          :narrow ?t
          :category buffer
          :preview-key ,(kbd "M-.")
          :state ,#'consult--buffer-state
          :items ,(lambda () (mapcar #'buffer-name
                                     (append (list (telega-root--buffer)) (telega-chat-buffers)))))
  "Source for Telega buffers to be set in `consult-buffer-sources'.")

;;;###autoload
;; TODO: `ement-session-rooms' with `ement-sessions' is too slow and blocks Emacs
(defvar eb-chat-ement-buffer-source
  `(:name "Ement"
          :narrow ?n
          :category buffer
          :preview-key ,(kbd "M-.")
          :state ,#'consult--buffer-state
          :items ,(lambda ()
                    (mapcar #'buffer-name (eb-util--mode-buffers 'ement-room-mode 'ement-room-list-mode)))))

;;;###autoload
(defun eb-chat-ement-connect ()
  "Connect to Ement with personal credentials."
  (interactive)
  (ement-connect
   :user-id eb-chat-ement-username
   :password eb-chat-ement-password
   :uri-prefix "http://localhost:8009"))

;;;###autoload
(defun eb-chat-erc-connect ()
  "Connects to IRC networks via TLS."
  (interactive)
  (erc-tls
   :server "irc.libera.chat"
   :port 6697
   :nick eb-chat-irc-libera-nick
   :password eb-chat-irc-libera-password)
  (erc-tls
   :server "irc.oftc.net"
   :port 6697
   :nick eb-chat-irc-oftc-nick))

;;;###autoload
(defun eb-chat-erc-bouncer-connect-libera ()
  "Connects to Libera via an IRC bouncer."
  (interactive)
  (setq erc-email-userid (format "%s/irc.libera.chat" eb-chat-irc-libera-nick))
  (erc-tls
   :server "chat.sr.ht"
   :port 6697
   :nick eb-chat-irc-bouncer-nick
   :password eb-chat-irc-bouncer-password))

(defun eb-chat-erc-status-sidebar-toggle ()
  "Toggles the status sidebar killing its buffer when closed."
  (interactive)
  (if (get-buffer-window erc-status-sidebar-buffer-name nil)
      (progn
        (erc-status-sidebar-close)
        (kill-buffer erc-status-sidebar-buffer-name))
    (erc-status-sidebar-open)))

(defun eb-chat-erc-toggle-timestamps ()
  "Refreshes the current ERC buffer after toggling the timestamps."
  (interactive)
  (erc-toggle-timestamps)
  (force-window-update (selected-window)))

(defun eb-chat-erc-window-reuse-condition (buf-name action)
  "Sets up a condition for ERC buffers to be reused."
  (with-current-buffer buf-name
    (when (eq major-mode 'erc-mode)
      (not action))))

(defun eb-chat-erc-status-add-padding (fun channame &optional num-messages erc-face)
  "Adds left padding on the sidebar formatted channels list."
  (concat " " (funcall fun channame num-messages erc-face)))

;;;###autoload
(defun eb-chat-erc-bouncer-connect-oftc ()
  "Connects to the OFTC network via an IRC bouncer."
  (interactive)
  (setq erc-email-userid (format "%s/irc.oftc.net" eb-chat-irc-oftc-nick))
  (erc-tls
   :server "chat.sr.ht"
   :port 6697
   :nick eb-chat-irc-bouncer-nick
   :password eb-chat-irc-bouncer-password))

;;;###autoload
(defun eb-chat-erc-close-buffers ()
  "Closes all ERC buffers when ERC server is closed by killing a buffer."
  (interactive)
  (mapc #'kill-buffer (erc-buffer-list nil erc-server-process)))

(provide 'eb-chat)
