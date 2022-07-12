;; -*- lexical-binding: t; -*-
(require 'xdg)
(require 'cl-lib)
(require 'dash)
(require 'consult)
(require 'webpaste)
(require 'sly)

(defgroup eb-web nil
  "Web browser integrations and tweaks."
  :group 'eb)

(defcustom eb-web-nyxt-port 4006
  "Default port to use for the Slynk connection to Nyxt."
  :type 'integer
  :group 'eb-web)

(defcustom eb-web-nyxt-development-p t
  "Indicates whether `nyxt' is to be used in its development version."
  :type 'boolean
  :type 'eb-web)

(defcustom eb-web-nyxt-startup-threshold 3
  "Number of seconds to wait for Nyxt to start up."
  :type 'string
  :group 'eb-web)

(defcustom eb-web-nyxt-workspace 2
  "The EXWM workspace to assign Nyxt windows to."
  :type 'integer
  :group 'eb-web)

(defvar eb-web-nyxt-process nil
  "Holds the current Nyxt process.")

(defun eb-web-srht-repo-id (name)
  "Return the ID associated with the sourcehut repository NAME."
    (interactive "sRepo name: ")
    (let* ((srht-token (password-store-get-field "vc/sourcehut" "token"))
           (oauth2-token (concat "Bearer " srht-token))
           (id (assoc-default
                'id
                (assoc-default
                 'repository
                 (assoc-default
                  'me
                  (assoc-default
                   'data
                   (request-response-data
                    (request
                      "https://git.sr.ht/query"
                      :params `(("query" . ,(concat "query { me { repository(name:\"" name "\") { id } } }")))
                      :type "GET"
                      :headers `(("Authorization" . ,oauth2-token))
                      :parser 'json-read
                      :sync t
                      :timeout 2
                      :error (cl-function
                              (lambda (&key error-thrown &allow-other-keys) (message "Error %S" error-thrown)))))))))))
      id))

(defun eb-web-srht-set-readme (id)
  "Export the current file to html and set the result as readme for the
  sourcehut repo identified by ID."
  (interactive
   (list (if-let* ((project (project-current t))
                   (dir (project-root project))
                   (name (string-match (rx (: "/" (group (+ (not "/"))) "/" eol)) dir)))
             (eb-web-srht-repo-id (match-string 1 dir))
             (call-interactively #'eb-web-srht-repo-id))))
  (let* ((srht-token (password-store-get-field "vc/sourcehut" "token"))
         (oauth2-token (concat "Bearer " srht-token))
         (readme (if (derived-mode-p 'html-mode)
                     (buffer-substring-no-properties (point-min) (point-max))
                     (org-export-as (org-export-get-backend 'html) nil nil t)))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (query (make-hash-table))
         (variables (make-hash-table)))
    (puthash "id" id variables)
    (puthash "readme" readme variables)
    (puthash
     "query"
     "mutation UpdateRepo($id: Int!, $readme: String!) {
      updateRepository(id: $id, input: { readme: $readme }) { id }
    }"
     query)
    (puthash "variables" variables query)
    (request
      "https://git.sr.ht/query"
      :type "POST"
      :data (json-serialize query)
      :headers `(("Content-Type" . "application/json") ("Authorization" . ,oauth2-token))
      :parser 'json-read
      :complete (cl-function (lambda (&key symbol-status &allow-other-keys)
                               (message "Set: %S" symbol-status))))))

;;;###autoload
(defun eb-web--bookmark-make-record (url title)
  "Create a bookmark record from a Nyxt web buffer."
  (let* ((defaults (delq nil (list title url)))
         (bookmark
          `(,title
            ,@(bookmark-make-record-default 'no-file)
            (nyxt-url . ,url)
            (filename . ,url)
            (handler . eb-web--jump-to-bookmark)
            (defaults . ,defaults))))
    bookmark))

;;;###autoload
(defun eb-web--jump-to-bookmark (bookmark)
  "Jump to BOOKMARK in Nyxt."
  (let ((location (bookmark-prop-get bookmark 'nyxt-url)))
    (browse-url-default-browser location)))

(defun eb-web--jump-to-bookmark-alt (bookmark)
  "Jump to BOOKMARK in alternative browser."
  (cl-letf (((symbol-function 'browse-url-can-use-xdg-open) #'ignore))
    (eb-web--jump-to-bookmark bookmark)))

;;;###autoload
(defun eb-web--slynk-connected-p ()
  "Indicates whether there's a connection to `eb-web-nyxt-port' currently."
  (require 'sly)
  (cl-find-if (lambda (p)
                (= (sly-connection-port p) eb-web-nyxt-port))
              sly-net-processes))

(defun eb-web--slynk-eval-sexps (&rest sexps)
  "Transform STRING to S-expression form to send to `slynk'."
  (cl-letf (((symbol-function 'sly-display-eval-result) #'ignore))
    (let ((string (mapconcat #'prin1-to-string sexps "")))
      (sly-interactive-eval string))))

(defun eb-web-open-with-cookies (cookies &optional url)
  "Fetches and opens URL with corresponding external application and COOKIES."
  (interactive "\nsURL: ")
  (let ((url-request-extra-headers
         `(("Cookie"
            . ,(cl-loop for (field cookie) in cookies
                        collect (format " %s=%s;" field cookie) into headers
                        finally (return (string-join headers))))))
        (filename (concat temporary-file-directory
                          (car (last (split-string url "/"))))))
    (unless (file-exists-p filename)
      (with-current-buffer
          (url-retrieve-synchronously url t)
        (goto-char (point-min))
        (re-search-forward "^$")
        (forward-line 1)
        (delete-region (point) (point-min))
        (write-region (point-min) (point-max) filename)))
    (consult-file-externally filename)))

(defun eb-web-shr-browse-url-new-window ()
  "Opens links in a new window."
  (interactive)
  (shr-browse-url nil nil t))

(defun eb-web--system-nyxt-p ()
  "Returns non-`nil' if the Nyxt system process is currently running."
  (cl-some (lambda (pid)
             (string-match (rx (: (* any) "nyxt" (* any)))
                           (assoc-default 'comm (process-attributes pid))))
           (list-system-processes)))

;;;###autoload
(cl-defun eb-web-nyxt-set-up-window (&key (focus nil))
  "Handles Nyxt's window, focusing on Nyxt's Emacs buffer if FOCUS,
and if exwm is enabled, it switches to the corresponding workspace."
  (interactive
   (when current-prefix-arg
     (list :focus t)))
  (let* ((nyxt-buffer (car (seq-filter (lambda (buf)
                                         (string-prefix-p
                                          "Nyxt:" (string-trim (buffer-name buf)) t))
                                       (buffer-list))))
         (exwm-workspace (when (require 'exwm nil t)
                           (exwm-workspace--position
                            (window-frame (or (get-buffer-window nyxt-buffer t)
                                              (when focus
                                                (frame-first-window
                                                 (exwm-workspace--workspace-from-frame-or-index
                                                  eb-web-nyxt-workspace)))))))))
    (when (require 'exwm nil t)
      (when (equal (current-buffer) nyxt-buffer)
        ;; (exwm-input-set-local-simulation-keys nil)
        )
      (when focus
        (exwm-workspace-switch exwm-workspace)
        (switch-to-buffer nyxt-buffer)))))

(cl-defmacro eb-web--with-nyxt ((&key (focus t)) &body body)
  "Evaluates BODY in the context of the current Nyxt connection and if FOCUS,
changes focus to the Nyxt frame."
  `(let* ((sly-log-events nil)
          (sly-default-connection (or (eb-web--slynk-connected-p)
                                      (when eb-web-nyxt-process
                                        (eb-web-connect-to-slynk))))
          (nyxt-executable (if eb-web-nyxt-development-p
                               (executable-find "guix")
                             (executable-find "nyxt")))
          (nyxt-development-flags
           (when (and (file-exists-p
                       (expand-file-name
                        "nyxt-dev.desktop"
                        (concat (xdg-data-home) "/applications/")))
                      eb-web-nyxt-development-p)
             (thread-last
               (xdg-data-home)
               (format "%s/applications/nyxt-dev.desktop")
               (xdg-desktop-read-file)
               (gethash "Exec")
               (split-string)
               (cdr)))))
     (cond
      ((and (not eb-web-nyxt-process) (not (eb-web--slynk-connected-p)))
       (setq eb-web-nyxt-process (apply #'start-process "nyxt"
                                        nil nyxt-executable nyxt-development-flags))
       (set-process-sentinel eb-web-nyxt-process
                             (lambda (p _e)
                               (when (= (process-exit-status p) 0)
                                 (setq eb-web-nyxt-process nil))))
       (run-at-time
        eb-web-nyxt-startup-threshold nil
        (lambda ()
          (let ((sly-default-connection (eb-web-connect-to-slynk)))
            (eb-web-nyxt-set-up-window :focus ,focus)
            (eb-web--slynk-eval-sexps ,@body)))))
      (t
       (eb-web-nyxt-set-up-window :focus ,focus)
       (eb-web--slynk-eval-sexps ,@body)))))

;;;###autoload
(defun eb-web-connect-to-slynk ()
  "Connects to the Slynk server to interact with the Nyxt browser."
  (interactive)
  (sly-connect "localhost" eb-web-nyxt-port))

;;;###autoload
(defun eb-web-search (query)
  "Searches for QUERY in Nyxt."
  (interactive "sSearch for: ")
  (eb-web--with-nyxt
   (:focus t)
   `(simple-search ,query)))

;;;###autoload
(defun eb-web-change-theme (theme)
  "Switches current browser THEME in Nyxt."
  (interactive
   (list (completing-read "Theme: " custom-known-themes)))
  (eb-web--with-nyxt
   (:focus nil)
   `(nx-tailor:select-theme ,theme)))

;;;###autoload
(defun eb-web-copy-url ()
  "Kills current page URL in Nyxt."
  (interactive)
  (eb-web--with-nyxt
   (:focus nil)
   `(copy-url)))

;;;###autoload
(defun eb-web-scroll-other-window ()
  "Scrolls the Nyxt window."
  (interactive)
  (eb-web--with-nyxt
   (:focus nil)
   `(nyxt/document-mode::scroll-down)))

;;;###autoload
(defun eb-web-scroll-other-window-down ()
  "Scrolls the Nyxt window upward."
  (interactive)
  (eb-web--with-nyxt
   (:focus nil)
   `(nyxt/document-mode::scroll-up)))

(defun eb-web-set-nyxt-transient-map ()
  "Sets a transient map for Nyxt transient `eb-web' commands."
  (interactive)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map "v" 'eb-web-scroll-other-window)
     (define-key map "V" 'eb-web-scroll-other-window-down)
     map)
   t))

;;;###autoload
(defun eb-web-webpaste-text (text)
  "Creates a new paste with TEXT."
  (interactive "sText: ")
  (webpaste--paste-text text))

;;;###autoload
(define-minor-mode eb-web-eww-mode
  "Custom mode for EWW buffers."
  :global t :group 'eb-web
  (setq-local shr-inhibit-images t))

(defun eb-web-add-url-scheme (fun url &rest args)
  "Adds an HTTPS scheme to URL if it's missing and invoke FUN and ARGS with it."
  (let ((link (if (string-match (rx (: bol (+ (in (?A . ?Z))) ":")) url)
                  url
                (concat "https:" url))))
    (apply fun link args)))

(provide 'eb-web)
