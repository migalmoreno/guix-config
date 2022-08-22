;; -*- lexical-binding: t; -*-
(require 'geiser)
(require 'geiser-guile)
(require 'geiser-repl)

(defgroup eb-scheme nil
  "Personal tweaks to work with the Scheme programming language."
  :group 'eb)

;;;###autoload
(defun eb-scheme-geiser-autoconnect ()
  "Start a Geiser REPL unless an active connection is already present."
  (unless (geiser-repl--connection*)
    (save-window-excursion
      (run-guile))))

(provide 'eb-scheme)
