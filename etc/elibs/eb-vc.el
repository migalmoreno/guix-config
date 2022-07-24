;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'json)
(require 'ox)
(require 'request)

(defgroup eb-vc nil
  "Version control enhancements."
  :group 'eb)

(defcustom eb-web-srht-token nil
  "The OAuth2 token for the Sourcehut account."
  :type 'string
  :group 'eb-web)

(defun eb-vc-srht-repo-id (name)
  "Return the ID associated with the sourcehut repository NAME."
    (interactive "sRepo name: ")
    (let* ((srht-token eb-web-srht-token)
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

;;;###autoload
(defun eb-vc-srht-set-readme (id)
  "Export the current file to html and set the result as readme for the
  sourcehut repo identified by ID."
  (interactive
   (list (if-let* ((project (project-current t))
                   (dir (project-root project))
                   (name (string-match (rx (: "/" (group (+ (not "/"))) "/" eol)) dir)))
             (eb-vc-srht-repo-id (match-string 1 dir))
             (call-interactively #'eb-vc-srht-repo-id))))
  (let* ((srht-token eb-web-srht-token)
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

(provide 'eb-vc)
