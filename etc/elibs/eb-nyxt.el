;; -*- lexical-binding: t; -*-
(require 'sly)
(require 'ol)

(defgroup eb-nyxt nil
  "Nyxt browser integrations and tweaks."
  :group 'eb)

(defcustom eb-nyxt-port 4006
  "Default port to use for the Slynk connection to Nyxt."
  :type 'integer
  :group 'eb-nyxt)

(defcustom eb-nyxt-development-p t
  "Indicate whether `nyxt' is to be used in its development version."
  :type 'boolean
  :type 'eb-nyxt)

(defcustom eb-nyxt-startup-threshold 3
  "Number of seconds to wait for Nyxt to start up."
  :type 'integer
  :group 'eb-nyxt)

(defcustom eb-nyxt-workspace 2
  "The EXWM workspace to assign Nyxt windows to."
  :type 'integer
  :group 'eb-nyxt)

(defvar eb-nyxt-development-flags nil
  "Nyxt flags to use to start the Nyxt process when
`eb-nyxt-development-p' is non-`nil'.")

(defvar eb-nyxt-process nil
  "Hold the current Nyxt process.")

;;;###autoload
(defun eb-nyxt-connect-to-slynk ()
  "Connect to the Slynk server to interact with the Nyxt browser."
  (interactive)
  (sly-connect "localhost" eb-nyxt-port))

;;;###autoload
(defun eb-nyxt--slynk-connected-p ()
  "Indicate whether there's currently a connection to `eb-nyxt-port'."
  (cl-find-if (lambda (p)
                (= (sly-connection-port p) eb-nyxt-port))
              sly-net-processes))

;;;###autoload
(cl-defun eb-nyxt-sly-eval (sexp &rest args &key &allow-other-keys)
  "Evaluate SEXP and ARGS with Slynk, automatically attaching a Slynk
process if needed."
  (let ((sly-default-connection (or (eb-nyxt--slynk-connected-p)
                                    (eb-nyxt-connect-to-slynk))))
    (apply #'sly-eval sexp args)))

(defun eb-nyxt--slynk-eval-sexps (&rest sexps)
  "Transform SEXPS to string form to send to `slynk'."
  (cl-letf (((symbol-function 'sly-display-eval-result) #'ignore))
    (let ((string (mapconcat #'prin1-to-string sexps "")))
      (sly-interactive-eval string))))

(defun eb-nyxt--system-process-p ()
  "Return non-`nil' if the Nyxt system process is currently running."
  (cl-some (lambda (pid)
             (string-match (rx (: (* any) "nyxt" (* any)))
                           (assoc-default 'comm (process-attributes pid))))
           (list-system-processes)))

;;;###autoload
(cl-defun eb-nyxt-exwm-focus-window (&key (focus nil))
  "Handle Nyxt's EXWM window, focusing on Nyxt's Emacs buffer if FOCUS,
and if exwm is enabled, it switches to its corresponding workspace."
  (interactive
   (when current-prefix-arg
     (list :focus t)))
  (let* ((nyxt-buffer (car (cl-remove-if-not (lambda (buffer)
                                               (string-match (rx (: "Nyxt:")) (buffer-name buffer)))
                                             (buffer-list))))
         (exwm-workspace (when (require 'exwm nil t)
                           (window-frame (or (get-buffer-window nyxt-buffer t)
                                             (when focus
                                               (frame-first-window
                                                (exwm-workspace--workspace-from-frame-or-index
                                                 eb-nyxt-workspace))))))))
    (when (and (require 'exwm nil t) focus)
      (exwm-workspace-switch (exwm-workspace--position exwm-workspace))
      (if (and (= (exwm-workspace--position exwm-workspace--current)
                  (exwm-workspace--position exwm-workspace))
               (> (length (window-list exwm-workspace)) 1))
          (if (get-buffer-window nyxt-buffer)
              (select-window (get-buffer-window nyxt-buffer))
            (switch-to-buffer-other-window nyxt-buffer))
        (switch-to-buffer nyxt-buffer)))))

(cl-defun eb-nyxt-run-with-nyxt (sexps &key (focus nil) (autostart nil))
  "Evaluate SEXPS in the context of the current Nyxt connection and if FOCUS,
change focus to the Nyxt exwm workspace. If AUTOSTART is non-`nil' and a Nyxt system
process is not found, it will automatically create one and connect Slynk to it."
  (let* ((sly-log-events nil)
         (sly-default-connection (or (eb-nyxt--slynk-connected-p)
                                     (when (or eb-nyxt-process
                                               (eb-nyxt--system-process-p))
                                       (eb-nyxt-connect-to-slynk)))))
    (cond
     ((and (not (eb-nyxt--system-process-p))
           (not eb-nyxt-process)
           (not (eb-nyxt--slynk-connected-p))
           autostart)
      (message "Launching Nyxt...")
      (setq eb-nyxt-process
            (if eb-nyxt-development-p
                (apply #'start-process "nyxt"
                       nil (executable-find "guix")
                       eb-nyxt-development-flags)
              (start-process "nyxt" nil (executable-find "nyxt"))))
      (set-process-sentinel eb-nyxt-process
                            (lambda (p _e)
                              (when (= (process-exit-status p) 0)
                                (setq eb-nyxt-process nil))))
      (run-at-time
       eb-nyxt-startup-threshold nil
       (lambda ()
         (let ((sly-default-connection (eb-nyxt-connect-to-slynk)))
           (eb-nyxt-exwm-focus-window :focus focus)
           (eb-nyxt--slynk-eval-sexps sexps)))))
     ((or (eb-nyxt--system-process-p)
          eb-nyxt-process)
      (eb-nyxt-exwm-focus-window :focus focus)
      (eb-nyxt--slynk-eval-sexps sexps)))))

(org-link-set-parameters
 "nyxt"
 :store #'eb-nyxt-store-link)

(defun eb-nyxt-store-link ()
  "Store the current page link via Org mode."
  (when (and (or eb-nyxt-process
                 (eb-nyxt--system-process-p))
             (when (require 'exwm nil t)
               (string-match (rx (: "Nyxt:")) (buffer-name (current-buffer)))))
    (org-link-store-props
     :type "nyxt"
     :link (eb-nyxt-sly-eval '(nyxt:render-url (nyxt:url (nyxt:current-buffer))))
     :description (eb-nyxt-sly-eval '(nyxt:title (nyxt:current-buffer))))))

(cl-defun eb-nyxt-capture (template &key (roam-p nil))
  "Store and capture the current Nyxt page link in the corresponding
 Org capture TEMPLATE, or if ROAM-P, in the corresponding Org Roam capture
template."
  (interactive)
  (with-current-buffer
      (car (cl-remove-if-not (lambda (buffer)
                               (string-match (rx (: "Nyxt:"))
                                             (buffer-name buffer)))
                             (buffer-list)))
    (org-store-link t t)
    (if roam-p
        (org-roam-capture nil template)
      (org-capture nil template))))

;;;###autoload
(defun eb-nyxt-search (query)
  "Search for QUERY in Nyxt."
  (interactive "sSearch for: ")
  (eb-nyxt-run-with-nyxt
   `(simple-search ,query)
   :focus t :autostart t))

;;;###autoload
(defun eb-nyxt-change-theme ()
  "Switch theme in Nyxt."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-vivendi)
      (eb-nyxt-run-with-nyxt
       '(nx-tailor:select-theme "modus-vivendi"))
    (eb-nyxt-run-with-nyxt
     '(nx-tailor:select-theme "modus-operandi"))))

;;;###autoload
(defun eb-nyxt-copy-url ()
  "Kill current page URL in Nyxt."
  (interactive)
  (eb-nyxt-run-with-nyxt '(copy-url)))

;;;###autoload
(defun eb-nyxt-delete-current-buffer ()
  "Delete the currently-selected buffer in Nyxt."
  (interactive)
  (eb-nyxt-run-with-nyxt '(delete-current-buffer)))

;;;###autoload
(defun eb-nyxt-scroll-other-window ()
  "Scroll the Nyxt window."
  (interactive)
  (eb-nyxt-run-with-nyxt
   '(nyxt/document-mode::scroll-down)))

;;;###autoload
(defun eb-nyxt-scroll-other-window-down ()
  "Scroll the Nyxt window upward."
  (interactive)
  (eb-nyxt-run-with-nyxt
   '(nyxt/document-mode::scroll-up)))

(defun eb-nyxt-set-transient-map ()
  "Set a transient map for transient `eb-nyxt' commands."
  (interactive)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map "v" 'eb-nyxt-scroll-other-window)
     (define-key map "V" 'eb-nyxt-scroll-other-window-down)
     map)
   t))

(provide 'eb-nyxt)
