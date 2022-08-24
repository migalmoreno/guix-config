;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'ytdl)
(require 'mpv)
(require 'emms-player-mpv)
(require 'tq)
(require 'ol)
(require 'all-the-icons)
(require 'eb-web)
(require 'eb-ytdl)

(defgroup eb-mpv nil
  "Personal `mpv.el' enhancements."
  :group 'eb)

(defcustom eb-mpv-started-hook nil
  "Hook run when an MPV process starts."
  :group 'eb-mpv
  :type 'hook)

(defcustom eb-mpv-paused-hook nil
  "Hook run when an MPV process is paused or resumed."
  :group 'eb-mpv
  :type 'hook)

(defcustom eb-mpv-finished-hook nil
  "Hook run when an MPV process is stopped by the user."
  :group 'eb-mpv
  :type 'hook)

(defcustom eb-mpv-seeked-hook nil
  "Hook run when an MPV process is seeked forward or backward."
  :group 'eb-mpv
  :type 'hook)

(defvar eb-mpv-mode-line-string nil)
(defvar eb-mpv-toggle-button nil)
(defvar eb-mpv-prev-button nil)
(defvar eb-mpv-next-button nil)
(defvar eb-mpv-playing-time-string "")
(defvar eb-mpv-playing-time-display-timer nil)
(defvar eb-mpv-playing-time 0
  "Time elapsed for the current MPV playback.")
(defvar eb-mpv-total-duration nil
  "The current mpv playback total duration.")
(defvar eb-mpv-playlist-p nil
  "Indicates if the current MPV process contains a playlist.")
(defvar eb-mpv-paused-p nil
  "Whether the current MPV playback is paused or not.")
(defvar eb-mpv-stopped-p nil
  "Whether the current MPV playback was stopped by the user.")

;;;###autoload
(defun eb-mpv-download ()
  "Download current mpv playback via `ytdl'."
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

(org-link-set-parameters
 "mpv"
 :store #'eb-mpv-store-link)

(defun eb-mpv-store-link ()
  "Store a link to an mpv track."
  (when (and (mpv-live-p)
             (not (equal (mpv--with-json
                          (mpv-get-property "video"))
                         'false))
             (string-match (rx (: "mpv:")) (buffer-name (current-buffer))))
    (let ((url (mpv-get-property "path"))
          (title (mpv-get-property "media-title")))
      (org-link-store-props
       :type "mpv"
       :link url
       :description title))))

(defun eb-mpv-capture ()
  "Store and capture the current mpv playback link with the corresponding
Org capture template."
  (interactive)
  (with-current-buffer (car (cl-remove-if-not (lambda (buffer)
                                                (string-match (rx (: "mpv:"))
                                                              (buffer-name buffer)))
                                              (buffer-list)))
    (org-store-link t t)
    (org-capture nil "tv")))

;;;###autoload
(cl-defun eb-mpv-start (url &key (force-private-p nil) (audio-only nil) (repeat nil))
  "Prompt for video quality before calling `mpv-start' on URL.
If FORCE-PRIVATE-P, ensure to use a privacy-friendly alternative of URL
 as defined in `eb-web-privacy-alts'. You can additionally specify whether
 to play the file as AUDIO-ONLY and if to REPEAT it by default."
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
                  (eb-ytdl--list-formats url)))
         (selected-format (and formats
                               (alist-get
                                (completing-read "Resolution: "
                                                 (lambda (string pred action)
                                                   (if (eq action 'metadata)
                                                       `(metadata
                                                         (category . mpv-file)
                                                         (display-sort-function . ,#'identity))
                                                     (complete-with-action action formats string pred))))
                                formats nil nil 'equal)))
         (extra-args (split-string
                      (concat
                       (when formats (format "--ytdl-format=%s" selected-format))
                       (when audio-only " --video=no")
                       (when repeat " --loop-file=inf")))))
    (when force-private-p
      (setq url (eb-web--transform-host url)))
    (if (mpv-get-property "playlist")
        (pcase (completing-read "Play or Enqueue: " '("Play" "Enqueue"))
          ("Play" (apply #'mpv-start url extra-args))
          ("Enqueue" (apply #'mpv-enqueue url extra-args)))
      (apply #'mpv-start url extra-args))))

;;;###autoload
(cl-defun eb-mpv-start-other-window (url &rest args &key &allow-other-keys)
  "Launch mpv in another window."
  (interactive "sURI: ")
  (apply #'eb-mpv-start url args)
  (switch-to-buffer-other-window (current-buffer)))

(defun eb-mpv-playing-time-start ()
  "Set up the display of mpv playback time."
  (setq eb-mpv-total-duration nil)
  (setq eb-mpv-playing-time 0)
  (unless eb-mpv-playing-time-display-timer
    (setq eb-mpv-playing-time-display-timer
          (run-at-time t 1 #'eb-mpv-playing-time-display))))

(defun eb-mpv-playing-time-pause ()
  "Pause displaying the current mpv playback time."
  (if eb-mpv-paused-p
      (eb-mpv-playing-time-stop)
    (unless eb-mpv-playing-time-display-timer
      (setq eb-mpv-playing-time-display-timer
            (run-at-time t 1 #'eb-mpv-playing-time-display)))))

(defun eb-mpv-playing-time-stop ()
  "Remove the playing time of mpv playback."
  (if (or (not eb-mpv-paused-p)
          eb-mpv-stopped-p)
      (progn
        (setq eb-mpv-playing-time-string "")
        (force-mode-line-update t)))
  (when eb-mpv-playing-time-display-timer
    (cancel-timer eb-mpv-playing-time-display-timer))
  (setq eb-mpv-playing-time-display-timer nil)
  (setq eb-mpv-total-duration nil))

(defun eb-mpv-playing-time-seek ()
  "Seek forward or backward in the displayed playing time."
  (setq eb-mpv-playing-time (mpv-get-property "playback-time"))
  (when (< eb-mpv-playing-time 0)
    (setq eb-mpv-playing-time 0)))

(defun eb-mpv-playing-time-display ()
  "Display the current MPV playing time."
  (unless (and eb-mpv-paused-p
               eb-mpv-playlist-p)
    (setq eb-mpv-playing-time (round (1+ eb-mpv-playing-time))))
  (unless eb-mpv-total-duration
    (setq eb-mpv-total-duration (or (ignore-errors (mpv-get-property "duration")) 0)))
  (cl-flet ((transform-time (time)
                            (cond
                             ((and time (numberp time) (> time 3600))
                              (format-time-string "%T" time t))
                             ((and (numberp time) time)
                              (format-time-string "%M:%S" time))
                             (t 0))))
    (if-let* ((formatted-playing-time (transform-time eb-mpv-playing-time))
              (formatted-total (transform-time eb-mpv-total-duration)))
        (setq eb-mpv-playing-time-string
              (if (null eb-mpv-playing-time-mode)
                  ""
                (format " %s/%s " formatted-playing-time formatted-total)))
      (setq eb-mpv-playing-time-string ""))
    (force-mode-line-update t)))

(defun eb-mpv-mode-line-clear ()
  "Clear the mode line after the mpv process exits."
  (interactive)
  (setq eb-mpv-mode-line-string nil)
  (setq eb-mpv-toggle-button nil)
  (setq eb-mpv-prev-button nil)
  (setq eb-mpv-next-button nil)
  (setq eb-mpv-playing-time-string nil))

(defun eb-mpv-set-playlist ()
  "Set appropriate information if current MPV process involves a playlist."
  (let ((playlist (ignore-errors (mpv--with-json
                                  (mpv-get-property "playlist")))))
    (if (and (not (equal playlist 'false))
             (consp playlist)
             (> (length playlist) 1))
        (progn
          (setq eb-mpv-playlist-p t)
          (setq eb-mpv-prev-button (all-the-icons-material "skip_previous"
                                                           :v-adjust -0.1
                                                           :height 1)
                eb-mpv-next-button (all-the-icons-material "skip_next"
                                                           :v-adjust -0.1
                                                           :height 1)))
      (setq eb-mpv-playlist-p nil)
      (setq eb-mpv-prev-button nil)
      (setq eb-mpv-next-button nil))))

(defun eb-mpv-set-paused ()
  "Set appropriate information if the current MPV process is stopped."
  (mpv-get-property "pause")
  (prog1
      (if (equal (mpv--with-json (mpv-get-property "pause"))
                 'false)
          (setq eb-mpv-toggle-button (all-the-icons-material "pause"
                                                             :v-adjust -0.14
                                                             :height 1))
        (setq eb-mpv-toggle-button (all-the-icons-material "play_arrow"
                                                           :v-adjust -0.14
                                                           :height 1)))
    (force-mode-line-update t)))

(defun eb-mpv-run-command (command &rest arguments)
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

(advice-add #'mpv-run-command :override #'eb-mpv-run-command)

(defun eb-mpv-compute-title ()
  "Compute and set the current MPV process media title."
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
    (setq eb-mpv-mode-line-string embellished-title))
  (force-mode-line-update t))

(defun eb-mpv-event-handler (result)
  "Handle the MPV events from RESULT."
  (pcase (alist-get 'event result)
    ((or "file-loaded" "start-file")
     (eb-mpv-change-theme)
     (setq eb-mpv-stopped-p nil)
     (eb-mpv-set-playlist)
     (eb-mpv-set-paused)
     (if (equal emms-player-mpv-proc
                mpv--process)
         (eb-mpv-compute-title)
       (run-at-time 2 nil #'eb-mpv-compute-title))
     (if (and eb-mpv-paused-p eb-mpv-playlist-p)
         (progn
           (setq eb-mpv-playing-time 0)
           (eb-mpv-playing-time-display)
           (run-hooks 'eb-mpv-paused-hook))
       (run-hooks 'eb-mpv-started-hook)))
    ("pause"
     (eb-mpv-set-paused)
     (unless eb-mpv-paused-p
       (setq eb-mpv-paused-p t)
       (run-hooks 'eb-mpv-paused-hook)))
    ("unpause"
     (eb-mpv-set-paused)
     (when eb-mpv-paused-p
       (setq eb-mpv-paused-p nil)
       (run-hooks 'eb-mpv-paused-hook)))
    ("seek"
     (run-hooks 'eb-mpv-seeked-hook))
    ((or "end-file" "shutdown")
     (setq eb-mpv-stopped-p t)
     (run-hooks 'eb-mpv-finished-hook)
     (eb-mpv-mode-line-clear))))

(defun eb-mpv-update-playlist (&rest _args)
  "Invoke `eb-mpv-set-playlist' after calling `mpv-enqueue'."
  (eb-mpv-set-playlist))

(defun eb-mpv-display-mode-line (&optional result)
  "Update and display the necessary MPV metadata in the modeline."
  (interactive)
  (if result
      (eb-mpv-event-handler result)
    (eb-mpv-set-paused)
    (eb-mpv-set-playlist)
    (eb-mpv-compute-title)))

;;;###autoload
(defun eb-mpv-seek-start ()
  "Seek to the start of the current MPV stream."
  (interactive)
  (mpv-seek 0))

(defun eb-mpv-kill ()
  "Kill the mpv process unless this is not currently `emms-player-mpv-proc'."
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
  (run-hooks 'eb-mpv-finished-hook))

(advice-add #'mpv-kill :override #'eb-mpv-kill)

(defun eb-mpv-connect-to-emms-on-startup (data)
  (interactive)
  (when (string= (alist-get 'event data) "start-file")
    (eb-mpv-connect-to-emms-proc)))

(add-hook 'emms-player-mpv-event-functions #'eb-mpv-connect-to-emms-on-startup)

;;;###autoload
(defun eb-mpv-connect-to-emms-proc ()
  "Connect to a running EMMS MPV process."
  (interactive)
  (setq eb-mpv-playing-time-string "")
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
  (run-hooks 'eb-mpv-started-hook)
  (when (equal mpv--process
               emms-player-mpv-proc)
    (eb-mpv-display-mode-line))
  t)

(defun eb-mpv--filter-processes ()
  "Filter processes to those that have a currently-running MPV process."
  (cl-remove-if-not (lambda (proc)
                      (string-match (rx (: (* anything) "mpv" (* anything))) (process-name proc)))
                    (process-list)))

(defun eb-mpv-playlist-shuffle ()
  "Toggle the shuffle state for the current playlist."
  (interactive)
  (mpv-run-command "playlist-shuffle"))

;;;###autoload
(defun eb-mpv-change-theme ()
  "Set theme in current mpv process according to current system theme."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-vivendi)
      (progn
        (mpv-set-property "background" "#000000")
        (mpv-set-property "osd-color" "#ffffff"))
    (mpv-set-property "background" "#ffffff")
    (mpv-set-property "osd-color" "#323232")))

;;;###autoload
(defun eb-mpv-kill-url (original-p)
  "Copy the URL in the current mpv stream to the system clibpoard. If
ORIGINAL-P, ensure the original service URL is killed rather than a
proxy url as per `eb-web-privacy-alts'."
  (interactive
   (list
    (yes-or-no-p "Copy original URL?")))
  (when-let* ((title (mpv-get-property "media-title"))
              (url (mpv-get-property "path"))
              (original-url (if original-p
                                (eb-web--transform-host url :original-p nil)
                              (eb-web--transform-url url))))
    (kill-new original-url)
    (message (format "Copied \"%s\" to the system clipboard" title))))

(defun eb-mpv-set-transient-map ()
  "Set a transient map for transient MPV commands."
  (interactive)
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map "b" 'mpv-seek-backward)
     (define-key map "f" 'mpv-seek-forward)
     (define-key map "p" 'mpv-pause)
     map)
   t))

;;;###autoload
(define-minor-mode eb-mpv-playing-time-mode
  "Display the current MPV playing time."
  :global t :group 'eb-mpv
  (if eb-mpv-playing-time-mode
      (progn
        (add-hook 'eb-mpv-started-hook #'eb-mpv-playing-time-start)
        (add-hook 'eb-mpv-paused-hook #'eb-mpv-playing-time-pause)
        (add-hook 'eb-mpv-finished-hook #'eb-mpv-playing-time-stop)
        (add-hook 'eb-mpv-seeked-hook #'eb-mpv-playing-time-seek))
    (eb-mpv-playing-time-stop)
    (remove-hook 'eb-mpv-started-hook #'eb-mpv-playing-time-start)
    (remove-hook 'eb-mpv-paused-hook #'eb-mpv-playing-time-pause)
    (remove-hook 'eb-mpv-finished-hook #'eb-mpv-playing-time-stop)
    (remove-hook 'eb-mpv-seeked-hook #'eb-mpv-playing-time-seek)))

;;;###autoload
(define-minor-mode eb-mpv-mode-line-mode
  "Display the current mpv playback information in the mode line."
  :global t :group 'eb-mpv
  (setq eb-mpv-mode-line-string nil)
  (if eb-mpv-mode-line-mode
      (progn
        (add-hook 'mpv-on-event-hook #'eb-mpv-display-mode-line)
        (add-hook 'mpv-on-exit-hook #'eb-mpv-mode-line-clear))
    (setq eb-mpv-mode-line-string nil)
    (setq eb-mpv-toggle-button nil)
    (setq eb-mpv-next-button nil)
    (setq eb-mpv-prev-button nil)
    (remove-hook 'mpv-on-event-hook #'eb-mpv-display-mode-line)
    (remove-hook 'mpv-on-exit-hook #'eb-mpv-mode-line-clear)))

(provide 'eb-mpv)
