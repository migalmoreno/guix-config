;; -*- lexical-binding: t; -*-
(require 'emms)
(require 'emms-player-mpv)
(require 'ytdl)

(defgroup eb-emms nil
  "Personal EMMS enhancements."
  :group 'eb)

(defcustom eb-emms-music-dir (expand-file-name "~/music")
  "Specifies the preferred music directory file system path."
  :type 'directory
  :group 'eb-emms)

;;;###autoload
(defun eb-emms-download-track ()
  "Download EMMS track at point using `ytdl'."
  (interactive)
  (emms-playlist-ensure-playlist-buffer)
  (with-current-emms-playlist
    (let* ((download-type (completing-read "Download type: " '("Music" "Video")))
           (track (emms-playlist-track-at))
           (title (emms-track-get track 'info-title))
           (source (emms-track-get track 'name)))
      (if (equal (emms-track-get track 'type) 'url)
          (ytdl--download-async
           source
           (expand-file-name title (if (string= download-type "Music")
                                       ytdl-music-folder
                                     ytdl-video-folder))
           (if (string= download-type "Music")
               ytdl-music-extra-args
             ytdl-video-extra-args)
           #'ignore
           download-type)
        (error "Track `%s' is not a remote track to download." title)))))

;;;###autoload
(defun eb-emms-library-load ()
  "Loads an EMMS playlist with local music files and moves to it."
  (interactive)
  (ignore-errors
    (if (not emms-playlist-buffer)
        (progn
          (emms-add-directory-tree eb-emms-music-dir)
          (emms-playlist-mode-go))
      (with-current-emms-playlist
        (unless (emms-playlist-selected-track)
          (emms-add-directory-tree eb-emms-music-dir))
        (emms-playlist-mode-go)))))

(define-emms-source eb-emms-track (url title &optional length)
  (let ((emms-track (emms-track 'url url)))
    (emms-track-set emms-track 'info-title title)
    (when length
      (emms-track-set emms-track 'info-playing-time length))
    (emms-playlist-insert-track emms-track)))

;;;###autoload
(defun eb-emms-source-track (url title &optional length play)
  "Append track with URL and TITLE to the current EMMS playlist.
Optionally, provide a LENGTH for the mode line and whether to PLAY the track."
  (interactive "sURL: \nsTitle: \nP")
  (if length
      (emms-add-eb-emms-track url title length)
    (emms-add-eb-emms-track url title nil))
  (when play
    (emms-stop)
    (emms-playlist-current-select-last)
    (emms-start)))

;;;###autoload
(defun eb-emms-seek-to-beginning ()
  "Seek to beginning of current EMMS track."
  (interactive)
  (emms-seek-to 0))

;;;###autoload
(defun eb-emms-next ()
  "Advance to the next track depending on the current playlist state."
  (interactive)
  (if emms-random-playlist
      (emms-random)
    (emms-next)))

;;;###autoload
(defun eb-emms-previous ()
  "Advance to the previous track depending on the current playlist state."
  (interactive)
  (if emms-random-playlist
      (emms-random)
    (emms-next)))

(provide 'eb-emms)
