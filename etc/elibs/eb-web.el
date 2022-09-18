;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'consult)
(require 'webpaste)

(defgroup eb-web nil
  "Generic browser utilities and tweaks."
  :group 'eb)

(defcustom eb-web-privacy-alts '()
  "Alist of the form SERVICE . PRIVATE-MAPPING where SERVICE is the hostname of
a media service and PRIVATE-MAPPING is a cons pair of REGEX . PRIVATE-HOST
where REGEX is what should match the alternative front-end and PRIVATE-HOST is
the preferred instance to rewrite URLs to."
  :type 'list
  :group 'eb-web)

(cl-defun eb-web--transform-host (url &key (alt-p t))
  "Transform URL to its currently set proxy in `eb-web-privacy-alts'.
If ALT-P is non-`nil', URL is a proxy URL, so try to find the
original service url."
  (string-match (rx (: (+ any) "//" (group (+ (not "/"))))) url)
  (if-let* ((service-url (match-string 1 url))
            (mapping (if alt-p
                         (cl-rassoc service-url eb-web-privacy-alts :test (lambda (url privacy-map)
                                                                          (string-match-p (car privacy-map) url)))
                       (assoc-string service-url eb-web-privacy-alts))))
      (if alt-p
          (replace-regexp-in-string
         service-url
         (car mapping)
         url)
        (replace-regexp-in-string
           service-url
           (cddr mapping)
           url))
    url))

;;;###autoload
(defun eb-web--get-geolocation ()
  "Fetch the current location's coordinates through Mozilla's Geolocation API."
  (with-current-buffer
      (url-retrieve-synchronously
       "https://location.services.mozilla.com/v1/geolocate?key=geoclue" t)
    (goto-char (point-min))
    (re-search-forward (rx (: bol "\n")) nil t)
    (delete-region (point) (point-min))
    (let* ((location (car (cdr (json-parse-string (buffer-string) :object-type 'plist))))
           (latitude (plist-get location :lat))
           (longitude (plist-get location :lng)))
      (list :longitude longitude :latitude latitude))))

;;;###autoload
(defun eb-web--bookmark-make-record (url title)
  "Create a bookmark record from a browser buffer."
  (let* ((defaults (delq nil (list title url)))
         (bookmark
          `(,title
            ,@(bookmark-make-record-default 'no-file)
            (browser-url . ,url)
            (filename . ,url)
            (handler . eb-web--jump-to-bookmark)
            (defaults . ,defaults))))
    bookmark))

;;;###autoload
(defun eb-web--jump-to-bookmark (bookmark)
  "Jump to BOOKMARK in main browser."
  (let ((location (bookmark-prop-get bookmark 'browser-url)))
    (browse-url-default-browser location)))

(defun eb-web--jump-to-bookmark-alt (bookmark)
  "Jump to BOOKMARK in alternative browser."
  (cl-letf (((symbol-function 'browse-url-can-use-xdg-open) #'ignore))
    (eb-web--jump-to-bookmark bookmark)))

(defun eb-web-open-with-cookies (cookies &optional url)
  "Fetch and open URL with corresponding external application and COOKIES."
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
  "Open links in a new window."
  (interactive)
  (shr-browse-url nil nil t))

(defun eb-web-add-url-scheme (fun url &rest args)
  "Add an HTTPS scheme to URL if it's missing and invoke FUN and ARGS with it."
  (let ((link (if (string-match (rx (: bol (+ (in (?A . ?Z))) ":")) url)
                  url
                (concat "https:" url))))
    (apply fun link args)))

(defun eb-web-trace-url (fun url &rest args)
  "Transform alternative front-end URL to its original service host and
invoke FUN and ARGS with it."
  (let ((link (eb-web--transform-host url)))
    (apply fun link args)))

;;;###autoload
(defun eb-web-webpaste-text (text)
  "Create a new paste with TEXT."
  (interactive "sText: ")
  (webpaste--paste-text text))

;;;###autoload
(define-minor-mode eb-web-eww-mode
  "Custom mode for EWW buffers."
  :global t :group 'eb-web
  (setq-local shr-inhibit-images t))

(provide 'eb-web)
