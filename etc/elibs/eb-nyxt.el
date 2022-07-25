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
  "Indicates whether `nyxt' is to be used in its development version."
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
  "Holds the current Nyxt process.")

;;;###autoload
(defun eb-nyxt-connect-to-slynk ()
  "Connects to the Slynk server to interact with the Nyxt browser."
  (interactive)
  (sly-connect "localhost" eb-nyxt-port))

;;;###autoload
(cl-defun eb-nyxt-sly-eval (sexp &rest args &key &allow-other-keys)
  "Evaluate SEXP and ARGS with Slynk, automatically attaching a Slynk
process if needed."
  (let ((sly-default-connection (or (eb-nyxt--slynk-connected-p)
                                    (eb-nyxt-connect-to-slynk))))
    (apply #'sly-eval sexp args)))

;;;###autoload
(defun eb-nyxt--slynk-connected-p ()
  "Indicates whether there's currently a connection to `eb-nyxt-port'."
  (cl-find-if (lambda (p)
                (= (sly-connection-port p) eb-nyxt-port))
              sly-net-processes))

(defun eb-nyxt--slynk-eval-sexps (&rest sexps)
  "Transform SEXPS to string form to send to `slynk'."
  (cl-letf (((symbol-function 'sly-display-eval-result) #'ignore))
    (let ((string (mapconcat #'prin1-to-string sexps "")))
      (sly-interactive-eval string))))

(defun eb-nyxt--system-process-p ()
  "Returns non-`nil' if the Nyxt system process is currently running."
  (cl-some (lambda (pid)
             (string-match (rx (: (* any) "nyxt" (* any)))
                           (assoc-default 'comm (process-attributes pid))))
           (list-system-processes)))

;;;###autoload
(cl-defun eb-nyxt-set-up-window (&key (focus nil))
  "Handles Nyxt's window, focusing on Nyxt's Emacs buffer if FOCUS,
and if exwm is enabled, it switches to the corresponding workspace."
  (interactive
   (when current-prefix-arg
     (list :focus t)))
  (let* ((nyxt-buffer (car (cl-remove-if-not (lambda (buffer)
                                               (string-match (rx (: "Nyxt:")) (buffer-name buffer)))
                                             (buffer-list))))
         (exwm-workspace (when (require 'exwm nil t)
                           (exwm-workspace--position
                            (window-frame (or (get-buffer-window nyxt-buffer t)
                                              (when focus
                                                (frame-first-window
                                                 (exwm-workspace--workspace-from-frame-or-index
                                                  eb-nyxt-workspace)))))))))
    (when (require 'exwm nil t)
      (when (equal (current-buffer) nyxt-buffer)
        ;; (exwm-input-set-local-simulation-keys nil)
        )
      (when focus
        (exwm-workspace-switch exwm-workspace)
        (switch-to-buffer nyxt-buffer)))))

(cl-defun eb-nyxt-run-with-nyxt (sexps &key (focus nil) (autostart nil))
  "Evaluates SEXPS in the context of the current Nyxt connection and if FOCUS,
changes focus to the Nyxt frame. If AUTOSTART is non-`nil' and a Nyxt system
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
           (eb-nyxt-set-up-window :focus focus)
           (eb-nyxt--slynk-eval-sexps sexps)))))
     ((or (eb-nyxt--system-process-p)
          eb-nyxt-process)
      (eb-nyxt-set-up-window :focus focus)
      (eb-nyxt--slynk-eval-sexps sexps)))))

(org-link-set-parameters
 "nyxt"
 :store #'eb-nyxt-store-link)

(defun eb-nyxt-store-link ()
  "Stores the current page link via Org mode."
  (when (and (or eb-nyxt-process
                 (eb-nyxt--system-process-p))
             (string-match (rx (: "Nyxt:")) (buffer-name (current-buffer))))
    (org-link-store-props
     :type "nyxt"
     :link (eb-nyxt-sly-eval '(nyxt:render-url (nyxt:url (nyxt:current-buffer))))
     :description (eb-nyxt-sly-eval '(nyxt:title (nyxt:current-buffer))))))

(cl-defun eb-nyxt-capture (template &key (roam-p nil))
  "Stores and captures the current Nyxt page link in the corresponding
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
  "Searches for QUERY in Nyxt."
  (interactive "sSearch for: ")
  (eb-nyxt-run-with-nyxt
   `(simple-search ,query)
   :focus t :autostart t))

(defun eb-nyxt-change-theme (theme)
  "Switches current browser THEME in Nyxt."
  (interactive
   (list (completing-read "Theme: " custom-known-themes)))
  (eb-nyxt-run-with-nyxt
   `(nx-tailor:select-theme ,theme)))

;;;###autoload
(defun eb-nyxt-copy-url ()
  "Kills current page URL in Nyxt."
  (interactive)
  (eb-nyxt-run-with-nyxt '(copy-url)))

;;;###autoload
(defun eb-nyxt-scroll-other-window ()
  "Scrolls the Nyxt window."
  (interactive)
  (eb-nyxt-run-with-nyxt
   '(nyxt/document-mode::scroll-down)))

;;;###autoload
(defun eb-nyxt-scroll-other-window-down ()
  "Scrolls the Nyxt window upward."
  (interactive)
  (eb-nyxt-run-with-nyxt
   '(nyxt/document-mode::scroll-up)))

(defun eb-nyxt-set-transient-map ()
  "Sets a transient map for transient `eb-nyxt' commands."
  (interactive)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map "v" 'eb-nyxt-scroll-other-window)
     (define-key map "V" 'eb-nyxt-scroll-other-window-down)
     map)
   t))

(provide 'eb-nyxt)
