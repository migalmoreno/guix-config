;; -*- lexical-binding: t; -*-
(require 'ytdl)
(require 'cl-lib)

(defgroup eb-ytdl nil
  "Personal `ytdl' customizations."
  :group 'eb)

(defun eb-ytdl--list-formats (url)
  "List all available formats for the stream with URL."
  (with-temp-buffer
    (call-process (executable-find "yt-dlp") nil t nil "--list-formats" url)
    (goto-char (point-min))
    (let ((formats
           (cl-loop while (not (eobp))
                    do (forward-line 1)
                    when (re-search-forward
                          (rx (: bol (+ digit) (+ blank)
                                 (group (+ alphanumeric)) (+ blank)
                                 (group (: (+ alphanumeric) (? blank) (+ alphanumeric)))
                                 (+ blank) (group (+ alphanumeric))))
                          (point-at-eol) t)
                    collect (list (match-string 2)
                                  (match-string 1))))
          result)
      (dolist (fmt formats result)
        (unless (member fmt result)
          (push fmt result)))
      result)))

;;;###autoload
(defun eb-ytdl--download-async (url filename extra-ytdl-args
                                                 &optional finish-function dl-type)
  "Asynchronously download video at URL into FILENAME.

Extra arguments to ytdl can be provided with EXTRA-YTDL-ARGS.

FINISH-FUNCTION is a function that is executed once the file is
downloaded.  It takes a single argument (file-path).

DL-TYPE is the download type, see `ytdl-download-types'."
  (ytdl--eval-mode-line-string 1)
  (let ((process-id)
        (uuid (ytdl--uuid url)))
    (setq process-id
          (async-start
           (lambda ()
             (with-temp-buffer
               (apply #'call-process "yt-dlp" nil t nil
                      "-o" (concat filename
                                   ".%(ext)s")
                      (append extra-ytdl-args
                              (list "--" url)))
               (goto-char (point-min))
               (if (search-forward-regexp "^ERROR:" nil t nil)
                   (progn
                     (beginning-of-line)
                     (buffer-substring-no-properties (line-beginning-position)
                                                     (line-end-position)))
                 (let ((file-path nil)
                       (ytdl-extensions
                        '("3gp" "aac" "flv" "m4a" "mp3" "mp4" "ogg" "wav" "webm" "mkv")))
                   (while (and (not (when file-path
                                      (file-exists-p file-path)))
                               ytdl-extensions)
                     (setq file-path (concat filename
                                             "."
                                             (car ytdl-extensions))
                           ytdl-extensions (cdr ytdl-extensions)))
                   file-path))))
           (lambda (response)
             (if (string-match "^ERROR" response)
                 (progn
                   (setq (ytdl--list-entry-status (gethash uuid
                                                           ytdl--download-list))
                         "error")
                   (setq (ytdl--list-entry-error (gethash uuid
                                                          ytdl--download-list))
                         response)
                   (ytdl--eval-mode-line-string -1)
                   (ytdl--message response))
               (ytdl--async-download-finished response uuid)
               (when finish-function
                 (funcall finish-function response)))
             (ytdl--refresh-list))))
    (puthash uuid (make-ytdl--list-entry :title (file-name-nondirectory filename)
                                         :status "downloading"
                                         :type (or dl-type "Unknown")
                                         :path nil
                                         :size "?"
                                         :error nil
                                         :url url
                                         :process-id process-id)
             ytdl--download-list))
  (ytdl--refresh-list)
  (ytdl-show-list))

(provide 'eb-ytdl)
