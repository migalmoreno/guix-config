;; -*- lexical-binding: t; -*-
(defgroup eb-slack nil
  "Personal utilities for Slack, the Emacs Slack client."
  :group 'eb)

(defcustom eb-slack-workspace ""
  "The username to use to connect to the Slack workspace."
  :type 'string
  :group 'eb-slack)

(defcustom eb-slack-token ""
  "The password to use to connect to the Slack workspace."
  :type 'string
  :group 'eb-slack)

(defcustom eb-slack-cookie ""
  "The password to use to connect to the Slack workspace."
  :type 'string
  :group 'eb-slack)

;;;###autoload
(defun eb-slack-connect ()
  "Connect to Slack workspace with personal credentials."
  (interactive)
  (slack-register-team
   :name eb-slack-workspace
   :token eb-slack-token
   :cookie eb-slack-cookie)
  (slack-change-current-team))

(provide 'eb-ement)
