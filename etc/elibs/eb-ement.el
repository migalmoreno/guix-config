;; -*- lexical-binding: t; -*-
(require 'ement)

(defgroup eb-ement nil
  "Personal utilities for Ement, the Emacs Matrix client."
  :group 'eb)

(defcustom eb-ement-username ""
  "The username to use to connect to the Matrix homeserver."
  :type 'string
  :group 'eb-ement)

(defcustom eb-ement-password ""
  "The password to use to connect to the Matrix homeserver."
  :type 'string
  :group 'eb-ement)

;;;###autoload
(defun eb-ement-connect ()
  "Connect to Ement with personal credentials."
  (interactive)
  (ement-connect
   :user-id eb-ement-username
   :password eb-ement-password
   :uri-prefix "http://localhost:8009"))

(provide 'eb-ement)
