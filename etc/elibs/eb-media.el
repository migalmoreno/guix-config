;; -*- lexical-binding: t; -*-
(require 'emms)
(require 'ytdl)
(require 'mpv)
(require 'notifications)

(defgroup eb-media nil
  "Personal media customizations."
  :group 'eb)

(defcustom eb-media-privacy-alts '()
  "Alist of the form SERVICE . PRIVATE-HOST where SERVICE is the hostname of
a media service and PRIVATE-HOST is the privacy friendly alternative
from which to play content."
  :type 'list
  :group 'eb-media)

;;;###autoload
(defcustom eb-media-music-dir (expand-file-name "~/music")
  "Specifies the preferred music directory file system path."
  :type 'directory
  :group 'eb-media)

;;;###autoload
(defcustom eb-media-video-dir (expand-file-name "~/videos")
  "Specifies the preferred videos directory file system path."
  :type 'directory
  :group 'eb-media)

(defvar eb-media-mpv-mode-line-string nil)
(defvar eb-media-mpv-toggle-button nil)
(defvar eb-media-mpv-prev-button nil)
(defvar eb-media-mpv-next-button nil)

(cl-defun eb-media--transform-url (url &key (original nil))
  "Transform URL to its currently set proxy in `eb-media-privacy-alts'.
If ORIGINAL, tries to find the original service URL for the
currently-proxied URL."
  (string-match (rx (: "//" (group (+ any)) "/" (group (+ any)))) url)
  (when-let* ((service-url (match-string 1 url))
              (mapping (if original
                           (cl-rassoc service-url eb-media-privacy-alts :test #'string=)
                         (assoc-string service-url eb-media-privacy-alts))))
    (if original
        (or (replace-regexp-in-string
             service-url
             (car mapping)
             url)
            url)
      (replace-regexp-in-string
       service-url
       (cdr mapping)
       url))))

;;;###autoload
(defun eb-media-emms-download-track ()
  "Download EMMS track at point using `ytdl'."
  (interactive)
  (emms-playlist-ensure-playlist-buffer)
  (with-current-emms-playlist
    (let* ((track (emms-playlist-track-at))
           (title (emms-track-get track 'info-title))
           (source (emms-track-get track 'name)))
      (if (equal (emms-track-get track 'type) 'url)
        (ytdl--download-async
         source
         (expand-file-name title eb-media-music-dir)
         ytdl-music-extra-args
         (lambda (_res)
           (notifications-notify
            :title "emms"
            :body (format "Successfully downloaded \"%s\"." title)
            :timeout 2000))
         "Music")
        (error "Track `%s' is not a remote track to download." title)))))

;;;###autoload
(defun eb-media-emms-library-load ()
  "Loads an EMMS playlist with local music files and moves to it."
  (interactive)
  (ignore-errors
    (if (not emms-playlist-buffer)
        (progn
          (emms-cache-reset)
          (emms-add-directory-tree eb-media-music-dir)
          (emms-playlist-mode-go))
      (with-current-emms-playlist
        (unless (emms-playlist-selected-track)
          (emms-add-directory-tree eb-media-music-dir))
        (emms-playlist-mode-go)))))

(define-emms-source eb-media-track (url title &optional length)
  (let ((emms-track (emms-track 'url url)))
    (emms-track-set emms-track 'info-title title)
    (when length
      (emms-track-set emms-track 'info-playing-time length))
    (emms-playlist-insert-track emms-track)))

;;;###autoload
(defun eb-media-emms-source-track (url title &optional length play)
  "Append track with URL and TITLE to the current EMMS playlist.
Optionally, provide a LENGTH for the mode line and whether to PLAY the track."
  (interactive "sURL: \nsTitle: \nP")
  (if length
      (emms-add-eb-media-track url title length)
    (emms-add-eb-media-track url title nil))
  (when play
    (emms-stop)
    (emms-playlist-current-select-last)
    (emms-start)))

;;;###autoload
(defun eb-media-emms-seek-to-beginning ()
  "Seek to beginning of current EMMS track."
  (interactive)
  (emms-seek-to 0))

;;;###autoload
(defun eb-media-emms-next ()
  "Advance to the next track depending on the current playlist state."
  (interactive)
  (if emms-random-playlist
      (emms-random)
    (emms-next)))

;;;###autoload
(defun eb-media-emms-previous ()
  "Advance to the previous track depending on the current playlist state."
  (interactive)
  (if emms-random-playlist
      (emms-random)
    (emms-next)))

;;;###autoload
(defun eb-media-mpv-download ()
  "Downloads current mpv playback via `ytdl'."
  (interactive)
  (require 'ytdl)
  (if-let ((download-type (completing-read "Download type: " '("Music" "Video")))
           (track (mpv-get-property "path"))
           (title (mpv-get-property "media-title")))
      (ytdl--download-async
       track
       (expand-file-name title (if (string= download-type "Music")
                                   ytdl-music-folder
                                 ytdl-video-folder))
       (if (string= download-type "Music")
           ytdl-music-extra-args
         ytdl-video-extra-args)
       #'ignore
       download-type)
    (error "`mpv' is not currently active.")))

;;;###autoload
(cl-defun eb-media-mpv-start (url &key (private nil) (audio-only nil) (repeat nil))
  "Prompts for video quality before calling `empv--play-or-enqueue' on URL.
If PRIVATE, use a privacy-friendly alternative of URL as defined per
`eb-media-privacy-alts'. Also, one can specify whether to play the file
 as AUDIO-ONLY and if to REPEAT it by default."
  (interactive "sURI: ")
  (let* ((formats
          (mapcar (lambda (format)
                    (let* ((format-res (string-match (rx (: (+ num) "x" (group (+ num))))
                                                     (car format)))
                           (res-height (and format-res (match-string 1 (car format)))))
                      (if res-height
                          (list
                           (format "%s | %s" res-height (cl-second format))
                           (format "best[height<=%s]"
                                   res-height))
                        (list
                         (format "%s | %s" (car format) (cl-second format))
                         (format "best[ext=%s]" (cl-second format))))))
                  (eb-media--ytdl-list-formats url)))
         (selected-format (and formats
                               (alist-get
                                (consult--read
                                 formats
                                 :prompt "Resolution: "
                                 :sort nil)
                                formats nil nil 'equal)))
         (extra-args (split-string
                      (concat
                       (when formats (format "--ytdl-format=%s" selected-format))
                       (when audio-only " --video=no")
                       (when repeat " --loop-file=inf")))))
    (when private
      (setq url (eb-media--transform-url url)))
    (if (mpv-get-property "playlist")
        (pcase (completing-read "Play or Enqueue: " '("Play" "Enqueue"))
          ("Play" (apply #'mpv-start url extra-args))
          ("Enqueue" (apply #'mpv-enqueue url extra-args)))
      (apply #'mpv-start url extra-args))))

(defun eb-media-mpv-mode-line-clear ()
  "Clears the mode line after the mpv process exits."
  (interactive)
  (setq eb-media-mpv-mode-line-string nil
        eb-media-mpv-toggle-button nil
        eb-media-mpv-prev-button nil
        eb-media-mpv-next-button nil))

(defun eb-media-mpv-mode-line-update (&optional result)
  "Updates the mode line information with current mpv playback information."
  (interactive)
  (require 'mpv)
  (if (mpv-live-p)
      (cl-flet ((playlist-p ()
                            (let ((playlist (ignore-errors (mpv--with-json
                                                            (mpv-get-property "playlist")))))
                              (if (and (not (equal playlist 'false))
                                       (consp playlist)
                                       (> (length playlist) 1))
                                  (setq eb-media-mpv-prev-button ""
                                        eb-media-mpv-next-button "")
                                (setq eb-media-mpv-prev-button nil
                                      eb-media-mpv-next-button nil))))
                (paused-p ()
                          (setq eb-media-mpv-toggle-button
                                (if (equal (mpv--with-json (mpv-get-property "pause")) 'false)
                                    ""
                                  ""))))
        (prog1
            (if-let* ((title (ignore-errors
                               (mpv-get-property "media-title")))
                      (embellished-title (if (stringp title)
                                             (if (<= (length title) 30)
                                                 (concat title " ")
                                               (let ((shortened-title (substring title 0 29)))
                                                 (if (= (aref shortened-title (- (length shortened-title) 1))
                                                        ?.)
                                                     (concat shortened-title ".. ")
                                                   (concat shortened-title "... "))))
                                           (mpv-get-property "path"))))
                (progn
                  (pcase (alist-get 'event result)
                    ("file-loaded"
                     (paused-p)
                     (playlist-p)
                     (setq eb-media-mpv-mode-line-string embellished-title))
                    ((or "pause" "unpause" "tracks-changed")
                     (paused-p))
                    ((or "end-file" "shutdown")
                     (eb-media-mpv-mode-line-clear)))
                  (playlist-p)
                  (unless result
                    (paused-p)
                    (concat
                     embellished-title
                     eb-media-mpv-prev-button
                     eb-media-mpv-next-button)))
              (eb-media-mpv-mode-line-clear))
          (force-mode-line-update t)))
    (eb-media-mpv-mode-line-clear)))

;;;###autoload
(defun eb-media-mpv-seek-start ()
  "Seek to the start of the current MPV stream."
  (interactive)
  (mpv-seek 0))

(defun eb-media-mpv-playlist-shuffle ()
  "Toggles the shuffle state for the current playlist."
  (interactive)
  (mpv-run-command "playlist-shuffle"))

;;;###autoload
(defun eb-media-mpv-set-colors (background foreground)
  "Sets BACKGROUND and FOREGROUND colors in current mpv process."
  (mpv-set-property "background" background)
  (mpv-set-property "osd-color" foreground))

(defun eb-media-mpv-current-time ()
  (interactive)
  (cl-flet ((transform-time (time)
                            (cond
                             ((and time (> time 3600))
                              (format-time-string "%T" time t))
                             (time
                              (format-time-string "%M:%S" time)))))
    (let* ((time (mpv-get-property "playback-time"))
           (duration (mpv-get-property "duration"))
           (formatted-time (transform-time time))
           (formatted-duration (transform-time duration)))
      (message "%s/%s" formatted-time formatted-duration))))

;;;###autoload
(defun eb-media-mpv-kill-url (original)
  "Copies the URL in the current mpv stream to the system clibpoard."
  (interactive
   (list
    (yes-or-no-p "Copy original URL?")))
  (when-let* ((title (mpv-get-property "media-title"))
              (url (mpv-get-property "path"))
              (original-url (if original
                                url
                              ;; (eb-media--transform-url :original t)
                              (eb-media--transform-url url))))
    (kill-new original-url)
    (message (format "Copied \"%s\" URL to the system clipboard." title))))

(defun eb-media-mpv-set-transient-map ()
  "Sets a transient map for transient `eb-media' commands."
  (interactive)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map "b" 'mpv-seek-backward)
     (define-key map "f" 'mpv-seek-forward)
     (define-key map "P" 'mpv-playlist-prev)
     (define-key map "N" 'mpv-playlist-next)
     (define-key map "t" 'eb-media-mpv-current-time)
     (define-key map "p" 'mpv-pause)
     (define-key map "C" 'mpv-chapter-prev)
     (define-key map "c" 'mpv-chapter-next)
     map)
   t))

(defun eb-media--ytdl-list-formats (url)
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
(defun eb-media-ytdl--download-async (url filename extra-ytdl-args
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

;;;###autoload
(define-minor-mode eb-media-mpv-mode-line-mode
  "Displays the current mpv playback information in the mode line."
  :global t :group 'eb-media
  (setq eb-media-mpv-mode-line-string nil)
  (if eb-media-mpv-mode-line-mode
      (progn
        (setq eb-media-mpv-mode-line-string
              (eb-media-mpv-mode-line-update))
        (add-hook 'mpv-on-event-hook #'eb-media-mpv-mode-line-update))
    (progn
      (setq eb-media-mpv-mode-line-string nil
            eb-media-mpv-prev-button nil
            eb-media-mpv-toggle-button nil
            eb-media-mpv-next-button nil)
      (remove-hook 'mpv-on-event-hook #'eb-media-mpv-mode-line-update))))

(provide 'eb-media)
