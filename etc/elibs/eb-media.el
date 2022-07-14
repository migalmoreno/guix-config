;; -*- lexical-binding: t; -*-
(require 'emms)
(require 'emms-player-mpv)
(require 'ytdl)
(require 'mpv)
(require 'tq)
(require 'ol)
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

(defcustom eb-media-music-dir (expand-file-name "~/music")
  "Specifies the preferred music directory file system path."
  :type 'directory
  :group 'eb-media)

(defcustom eb-media-video-dir (expand-file-name "~/videos")
  "Specifies the preferred videos directory file system path."
  :type 'directory
  :group 'eb-media)

(defcustom eb-media-mpv-started-hook nil
  "Hook run when an MPV process starts."
  :group 'eb-media
  :type 'hook)

(defcustom eb-media-mpv-paused-hook nil
  "Hook run when an MPV process is paused or resumed."
  :group 'eb-media
  :type 'hook)

(defcustom eb-media-mpv-finished-hook nil
  "Hook run when an MPV process is stopped by the user."
  :group 'eb-media
  :type 'hook)

(defcustom eb-media-mpv-seeked-hook nil
  "Hook run when an MPV process is seeked forward or backward."
  :group 'eb-media
  :type 'hook)

(defvar eb-media-mpv-mode-line-string nil)
(defvar eb-media-mpv-toggle-button nil)
(defvar eb-media-mpv-prev-button nil)
(defvar eb-media-mpv-next-button nil)

(defvar eb-media-mpv-playing-time-string "")
(defvar eb-media-mpv-total-duration nil
  "The current mpv playback total duration.")
(defvar eb-media-mpv-playing-time 0
  "Time elapsed for the current MPV playback.")
(defvar eb-media-mpv-playing-time-display-timer nil)
(defvar eb-media-mpv-paused-p nil
  "Whether the current MPV playback is paused or not.")
(defvar eb-media-mpv-stopped-p nil
  "Whether the current MPV playback was stopped by the user.")

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
            :app-name "emms"
            :title (format "Successfully downloaded \"%s\"." title)
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
    (error "`mpv' is not currently active")))

;;;###autoload
(defun eb-media-mpv-capture-link ()
  "Stores the current mpv playback link and captures it."
  (let ((url (mpv-get-property "path"))
        (title (mpv-get-property "media-title")))
    (org-link-set-parameters
     "mpv"
     :store (lambda ()
              (org-link-store-props
               :type "mpv"
               :link url
               :description title)))
    (org-capture nil "tv")))

;;;###autoload
(cl-defun eb-media-mpv-start (url &key (private nil) (audio-only nil) (repeat nil))
  "Prompts for video quality before calling `mpv-start' on URL.
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

(defun eb-media-mpv-playing-time-start ()
  "Sets up the display of mpv playback time."
  (setq eb-media-mpv-total-duration nil)
  (setq eb-media-mpv-playing-time 0)
  (unless eb-media-mpv-playing-time-display-timer
    (setq eb-media-mpv-playing-time-display-timer
          (run-at-time t 1 #'eb-media-mpv-playing-time-display))))

(defun eb-media-mpv-playing-time-pause ()
  "Pause displaying the current mpv playback time."
  (if eb-media-mpv-paused-p
      (eb-media-mpv-playing-time-stop)
    (unless eb-media-mpv-playing-time-display-timer
      (setq eb-media-mpv-playing-time-display-timer
            (run-at-time t 1 #'eb-media-mpv-playing-time-display)))))

(defun eb-media-mpv-playing-time-stop ()
  "Removes the playing time of mpv playback."
  (if (or (not eb-media-mpv-paused-p)
          eb-media-mpv-stopped-p)
      (progn
        (setq eb-media-mpv-playing-time-string "")
        (force-mode-line-update t)))
  (emms-cancel-timer eb-media-mpv-playing-time-display-timer)
  (setq eb-media-mpv-playing-time-display-timer nil)
  (setq eb-media-mpv-total-duration nil))

(defun eb-media-mpv-playing-time-seek ()
  "Seeks forward or backward in the displayed playing time."
  (setq eb-media-mpv-playing-time (mpv-get-property "playback-time"))
  (when (< eb-media-mpv-playing-time 0)
    (setq eb-media-mpv-playing-time 0)))

(defun eb-media-mpv-playing-time-display ()
  "Displays the current MPV playing time."
  (unless eb-media-mpv-paused-p
    (setq eb-media-mpv-playing-time (round (1+ eb-media-mpv-playing-time))))
  (unless eb-media-mpv-total-duration
    (setq eb-media-mpv-total-duration (or (ignore-errors (mpv-get-property "duration")) 0)))
  (cl-flet ((transform-time (time)
                            (cond
                             ((and time (numberp time) (> time 3600))
                              (format-time-string "%T" time t))
                             ((and (numberp time) time)
                              (format-time-string "%M:%S" time))
                             (t 0))))
    (if-let* ((formatted-playing-time (transform-time eb-media-mpv-playing-time))
              (formatted-total (transform-time eb-media-mpv-total-duration)))
        (setq eb-media-mpv-playing-time-string
              (if (null eb-media-mpv-playing-time-mode)
                  ""
                (format " %s/%s " formatted-playing-time formatted-total)))
      (setq eb-media-mpv-playing-time-string ""))
    (force-mode-line-update t)))

(defun eb-media-mpv-mode-line-clear ()
  "Clears the mode line after the mpv process exits."
  (interactive)
  (setq eb-media-mpv-mode-line-string nil)
  (setq eb-media-mpv-toggle-button nil)
  (setq eb-media-mpv-prev-button nil)
  (setq eb-media-mpv-next-button nil))

(defun eb-media-mpv-set-playlist ()
  "Sets appropriate information if current MPV process involves a playlist."
  (let ((playlist (ignore-errors (mpv--with-json
                                  (mpv-get-property "playlist")))))
    (if (and (not (equal playlist 'false))
             (consp playlist)
             (> (length playlist) 1))
        (setq eb-media-mpv-prev-button ""
              eb-media-mpv-next-button "")
      (setq eb-media-mpv-prev-button nil
            eb-media-mpv-next-button nil))))

(defun eb-media-mpv-set-paused ()
  "Sets appropriate information if the current MPV process is stopped."
  (mpv-get-property "pause")
  (prog1
      (if (equal (mpv--with-json (mpv-get-property "pause"))
                 'false)
          (setq eb-media-mpv-toggle-button "")
        (setq eb-media-mpv-toggle-button ""))
    (force-mode-line-update t)))

(defun eb-media-mpv-run-command (command &rest arguments)
  "Send a COMMAND to mpv, passing the remaining ARGUMENTS.
Block while waiting for the response."
  (when (mpv-live-p)
    (let* ((response
            (cl-block mpv-run-command-wait-for-response
              (mpv--enqueue
               (cons command arguments)
               (lambda (response)
                 (cl-return-from mpv-run-command-wait-for-response
                   response)))
              (while (mpv-live-p)
                (sleep-for 0.05))))
           (status (alist-get 'error response))
           (data (alist-get 'data response)))
    (unless (string-equal status "success")
      (error "`%s' failed: %s" command status))
    data)))

(advice-add #'mpv-run-command :override #'eb-media-mpv-run-command)

(defun eb-media-mpv-compute-title ()
  "Computes and sets the current MPV process media title."
  (when-let* ((title (mpv--with-json (mpv-get-property "media-title")))
              (embellished-title
               (and (not (equal title 'false))
                    (if (stringp title)
                        (if (<= (length title) 30)
                            (concat title " ")
                          (let ((shortened-title (substring title 0 29)))
                            (if (= (aref shortened-title (- (length shortened-title) 1))
                                   ?.)
                                (concat shortened-title ".. ")
                              (concat shortened-title "... "))))
                      (concat (mpv-get-property "path") " ")))))
    (setq eb-media-mpv-mode-line-string embellished-title))
  (force-mode-line-update t))

(defun eb-media-mpv-event-handler (result)
  "Handles the MPV events from RESULT."
  (pcase (alist-get 'event result)
    ((or "file-loaded" "start-file")
     (eb-media-mpv-change-theme)
     (setq eb-media-mpv-stopped-p nil)
     (eb-media-mpv-set-playlist)
     (eb-media-mpv-set-paused)
     (if (equal emms-player-mpv-proc
                mpv--process)
         (eb-media-mpv-compute-title)
       (run-at-time 2 nil #'eb-media-mpv-compute-title))
     (if eb-media-mpv-paused-p
         (progn
           (setq eb-media-mpv-playing-time 0)
           (eb-media-mpv-playing-time-display)
           (run-hooks 'eb-media-mpv-paused-hook))
       (run-hooks 'eb-media-mpv-started-hook)))
    ("pause"
     (eb-media-mpv-set-paused)
     (unless eb-media-mpv-paused-p
       (setq eb-media-mpv-paused-p t)
       (run-hooks 'eb-media-mpv-paused-hook)))
    ("unpause"
     (eb-media-mpv-set-paused)
     (when eb-media-mpv-paused-p
       (setq eb-media-mpv-paused-p nil)
       (run-hooks 'eb-media-mpv-paused-hook)))
    ("seek"
     (run-hooks 'eb-media-mpv-seeked-hook))
    ((or "end-file" "shutdown")
     (setq eb-media-mpv-stopped-p t)
     (run-hooks 'eb-media-mpv-finished-hook)
     (eb-media-mpv-mode-line-clear))))

(defun eb-media-mpv-update-playlist (&rest _args)
  "Invokes `eb-media-mpv-set-playlist' after calling `mpv-enque'."
  (eb-media-mpv-set-playlist))

(defun eb-media-mpv-display-mode-line (&optional result)
  "Updates and displays the necessary MPV metadata in the modeline."
  (interactive)
  (if result
      (eb-media-mpv-event-handler result)
    (eb-media-mpv-set-paused)
    (eb-media-mpv-set-playlist)
    (eb-media-mpv-compute-title)))

;;;###autoload
(defun eb-media-mpv-seek-start ()
  "Seek to the start of the current MPV stream."
  (interactive)
  (mpv-seek 0))

(defun eb-media-mpv-kill ()
  "Kills the mpv process unless this is not currently `emms-player-mpv-proc'."
  (interactive)
  (when (equal mpv--process
               emms-player-mpv-proc)
    (emms-stop))
  (when mpv--queue
    (tq-close mpv--queue))
  (when (and (mpv-live-p)
             (not (equal mpv--process
                         emms-player-mpv-proc)))
    (kill-process mpv--process))
  (with-timeout
      (0.5 (error "Failed to kill mpv"))
    (while (and (mpv-live-p)
                (not (equal mpv--process
                            emms-player-mpv-proc)))
      (sleep-for 0.05)))
  (setq mpv--process nil)
  (setq mpv--queue nil)
  (run-hooks 'eb-media-mpv-finished-hook))

(advice-add #'mpv-kill :override #'eb-media-mpv-kill)

(defun eb-media-mpv-connect-to-emms-on-startup (data)
  (interactive)
  (when (string= (alist-get 'event data) "start-file")
    (eb-media-mpv-connect-to-emms-proc)))

(add-hook 'emms-player-mpv-event-functions #'eb-media-mpv-connect-to-emms-on-startup)

;;;###autoload
(defun eb-media-mpv-connect-to-emms-proc ()
  "Connect to a running EMMS MPV process."
  (interactive)
  (setq eb-media-mpv-playing-time-string "")
  (when (not (equal mpv--process
                    emms-player-mpv-proc))
    (mpv-kill))
  (setq mpv--process emms-player-mpv-proc)
  (set-process-query-on-exit-flag mpv--process nil)
  (set-process-sentinel
   mpv--process
   (lambda (p _e)
     (when (memq (process-status p) '(exit signal))
       (when (not (equal mpv--process
                         emms-player-mpv-proc))
         (mpv-kill))
       (run-hooks 'mpv-on-exit-hook))))
  (unless mpv--queue
    (setq mpv--queue (tq-create (make-network-process
                                 :name "emms-mpv-socket"
                                 :family 'local
                                 :service emms-player-mpv-ipc-socket
                                 :coding '(utf-8 . utf-8)
                                 :noquery t
                                 :filter #'emms-player-mpv-ipc-filter
                                 :sentinel #'emms-player-mpv-ipc-sentinel)))
    (set-process-filter
     (tq-process mpv--queue)
     (lambda (_proc string)
       (ignore-errors
         (mpv--tq-filter mpv--queue string)))))
  (run-hooks 'mpv-on-start-hook)
  (run-hooks 'eb-media-mpv-started-hook)
  (eb-media-mpv-display-mode-line)
  t)

(defun eb-media-mpv--filter-processes ()
  "Filter processes to those that have a currently-running MPV process."
  (cl-remove-if-not (lambda (proc)
                      (string-match (rx (: (* anything) "mpv" (* anything))) (process-name proc)))
                    (process-list)))

(defun eb-media-mpv-playlist-shuffle ()
  "Toggles the shuffle state for the current playlist."
  (interactive)
  (mpv-run-command "playlist-shuffle"))

;;;###autoload
(defun eb-media-mpv-change-theme ()
  "Sets theme in current mpv process according to current system theme."
  (interactive)
  (if (string-match (getenv "GTK_THEME") ":dark")
      (progn
        (mpv-set-property "background" "#000000")
        (mpv-set-property "osd-color" "#ffffff"))
    (mpv-set-property "background" "#ffffff")
    (mpv-set-property "osd-color" "#323232")))

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
     (define-key map "p" 'mpv-pause)
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

(define-minor-mode eb-media-mpv-playing-time-mode
  "Displays the current MPV playing time."
  :global t :group 'eb-media
  (if eb-media-mpv-playing-time-mode
      (progn
        (add-hook 'eb-media-mpv-started-hook #'eb-media-mpv-playing-time-start)
        (add-hook 'eb-media-mpv-paused-hook #'eb-media-mpv-playing-time-pause)
        (add-hook 'eb-media-mpv-finished-hook #'eb-media-mpv-playing-time-stop)
        (add-hook 'eb-media-mpv-seeked-hook #'eb-media-mpv-playing-time-seek))
    (eb-media-mpv-playing-time-stop)
    (remove-hook 'eb-media-mpv-started-hook #'eb-media-mpv-playing-time-start)
    (remove-hook 'eb-media-mpv-paused-hook #'eb-media-mpv-playing-time-pause)
    (remove-hook 'eb-media-mpv-finished-hook #'eb-media-mpv-playing-time-stop)
    (remove-hook 'eb-media-mpv-seeked-hook #'eb-media-mpv-playing-time-seek)))

;;;###autoload
(define-minor-mode eb-media-mpv-mode-line-mode
  "Displays the current mpv playback information in the mode line."
  :global t :group 'eb-media
  (setq eb-media-mpv-mode-line-string nil)
  (if eb-media-mpv-mode-line-mode
      (add-hook 'mpv-on-event-hook #'eb-media-mpv-display-mode-line)
    (setq eb-media-mpv-mode-line-string nil)
    (setq eb-media-mpv-toggle-button nil)
    (setq eb-media-mpv-next-button nil)
    (setq eb-media-mpv-prev-button nil)
    (remove-hook 'mpv-on-event-hook #'eb-media-mpv-display-mode-line)))

(provide 'eb-media)
