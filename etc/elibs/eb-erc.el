;; -*- lexical-binding: t; -*-
(require 'erc)
(require 'erc-status-sidebar)

(defgroup eb-erc nil
  "Personal settings for ERC."
  :group 'eb)

(defcustom eb-erc-bouncer-nick ""
  "The nick to use for the ERC bouncer connection."
  :type 'string
  :group 'eb-erc)

(defcustom eb-erc-bouncer-password ""
  "The password to use for the ERC bouncer connection."
  :type 'string
  :group 'eb-erc)

(defcustom eb-erc-oftc-nick ""
  "The nick to use for the ERC connection to the OFTC network."
  :type 'string
  :group 'eb-erc)

(defcustom eb-erc-libera-nick ""
  "The nick to use for the ERC connection to the Libera.chat network."
  :type 'string
  :group 'eb-erc)

(defcustom eb-erc-libera-password ""
  "The password to use for the ERC connection to the Libera.chat network."
  :type 'string
  :group 'eb-erc)

(defcustom eb-erc-oftc-password ""
  "The password to use for the ERC connection to the OFTC.net network."
  :type 'string
  :group 'eb-erc)

;;;###autoload
(defun eb-erc-connect ()
  "Connect to IRC networks via TLS."
  (interactive)
  (erc-tls
   :server "irc.libera.chat"
   :port 6697
   :nick eb-erc-libera-nick
   :password eb-erc-libera-password)
  (erc-tls
   :server "irc.oftc.net"
   :port 6697
   :nick eb-erc-oftc-nick
   :password eb-erc-oftc-password))

;;;###autoload
(defun eb-erc-bouncer-connect-libera ()
  "Connect to Libera via an IRC bouncer."
  (interactive)
  (setq erc-email-userid (format "%s/irc.libera.chat" eb-erc-libera-nick))
  (erc-tls
   :server "chat.sr.ht"
   :port 6697
   :nick eb-erc-bouncer-nick
   :password eb-erc-bouncer-password))

(defun eb-erc-status-sidebar-toggle ()
  "Toggle the status sidebar killing its buffer when closed."
  (interactive)
  (if (get-buffer-window erc-status-sidebar-buffer-name nil)
      (progn
        (erc-status-sidebar-close)
        (kill-buffer erc-status-sidebar-buffer-name))
    (erc-status-sidebar-open)))

(defun eb-erc-toggle-timestamps ()
  "Refresh the current ERC buffer after toggling the timestamps."
  (interactive)
  (erc-toggle-timestamps)
  (force-window-update (selected-window)))

(defun eb-erc-window-reuse-condition (buf-name action)
  "Set up a condition for ERC buffers to be reused."
  (with-current-buffer buf-name
    (when (eq major-mode 'erc-mode)
      (not action))))

(defun eb-erc-status-add-padding (fun channame &optional num-messages erc-face)
  "Add left padding on the sidebar formatted channels list."
  (concat " " (funcall fun channame num-messages erc-face)))

;;;###autoload
(defun eb-erc-bouncer-connect-oftc ()
  "Connect to the OFTC network via an IRC bouncer."
  (interactive)
  (setq erc-email-userid (format "%s/irc.oftc.net" eb-erc-oftc-nick))
  (erc-tls
   :server "chat.sr.ht"
   :port 6697
   :nick eb-erc-bouncer-nick
   :password eb-erc-bouncer-password))

;;;###autoload
(defun eb-erc-close-buffers ()
  "Close all ERC buffers when ERC server is closed by killing a buffer."
  (interactive)
  (mapc #'kill-buffer (erc-buffer-list nil erc-server-process)))

(provide 'eb-erc)
