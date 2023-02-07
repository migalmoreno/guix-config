(define-module (conses features video)
  #:use-module (conses utils)
  #:use-module (conses packages emacs-xyz)
  #:use-module (conses features web-browsers)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home-services video)
  #:use-module (gnu packages video)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:export (feature-mpv
            feature-youtube-dl))

(define* (feature-mpv
          #:key
          (mpv mpv)
          (emacs-mpv emacs-mpv)
          (extra-mpv-conf '())
          (extra-bindings '()))
  "Configure mpv, the command-line player."
  (ensure-pred file-like? mpv)
  (ensure-pred file-like? emacs-mpv)
  (ensure-pred alist? extra-mpv-conf)
  (ensure-pred alist? extra-bindings)

  (define f-name 'mpv)

  (define (get-home-services config)
    "Return home services related to mpv."
    (require-value 'fonts config)

    (define (run-with-emacs command)
      (format #f (string-append
                  "run \"/bin/sh\" \"-c\" \"emacsclient -e"
                  (if (string? command)
                      "~a\""
                      "'~s'\""))
              command))
    (define gtk-light-theme? (get-value 'gtk-dark-theme? config))
    (define font-sans-serif (get-value 'font-sans-serif config))

    (append
     (list
      (simple-service
       'add-mpv-mime-entries
       home-xdg-mime-applications-service-type
       (home-xdg-mime-applications-configuration
        (default
         '((video/mp4 . mpv.desktop)
           (video/mkv . mpv.desktop)
           (audio/mpeg . mpv.desktop)))))
      (service home-mpv-service-type
               (home-mpv-configuration
                (package mpv)
                (default-options
                 `((script . ,(file-append mpv-mpris "/lib/mpris.so"))
                   (osd-font . ,font-sans-serif)
                   (sub-font . ,font-sans-serif)
                   ,@extra-mpv-conf))
                (bindings
                 `(("ctrl+a" . "seek 0 absolute-percent")
                   ("ctrl+e" . "seek 100 absolute-percent")
                   ("ctrl+f" . "seek 5 relative")
                   ("ctrl+b" . "seek -5 relative")
                   ("Shift+n" . "add chapter 1")
                   ("Shift+p" . "add chapter -1")
                   ("D" . ,(run-with-emacs '(rde-mpv-download)))
                   ("Alt+c" . ,(run-with-emacs '(rde-mpv-capture)))
                   ,@extra-bindings))))
      (when (get-value 'emacs config)
        (let ((emacs-embark (get-value 'emacs-embark config))
              (emacs-all-the-icons (get-value 'emacs-all-the-icons config)))
          (rde-elisp-configuration-service
           f-name
           config
           `((eval-when-compile
               (require 'mpv)
               (require 'cl-lib))
             (cl-defun rde-mpv-play-url (url &optional format &key
                                             audio repeat
                                             (formats t) (select t) playlist)
               "Play URL with `mpv-start'.
You can specify whether to PLAY the file as AUDIO, if you want to be
prompted for FORMATS or use FORMAT, to REPEAT the file, manually SELECT what to
do with the file, and whether to add the file to the current PLAYLIST."
               (interactive "sURI: ")
               (let* ((sel-format (or format
                                      (and formats (ytdl-select-format url))))
                      (extra-args
                       (split-string
                        (concat
                         (format "--ytdl-format=%s" (or sel-format "best"))
                         (and audio " --video=no")
                         (and repeat " --loop-file=inf")))))
                 (if (and select (mpv-get-property "playlist"))
                     (pcase (completing-read "Play or Enqueue: "
                                             '("Play" "Enqueue"))
                       ("Play" (apply 'mpv-start url extra-args))
                       ("Enqueue" (apply 'mpv-playlist-append-url url extra-args)))
                   (if (and playlist (mpv-get-property "playlist"))
                       (apply 'mpv-playlist-append-url url extra-args)
                     (apply 'mpv-start url extra-args)))))

             (defun rde-mpv-kill ()
               "Kill the mpv process unless this is not currently
`emms-player-mpv-proc'."
               (interactive)
               (require 'emms-player-mpv)
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
               (run-hooks 'mpv-finished-hook))

             (defun rde-mpv-download ()
               "Download current mpv playback via `ytdl'."
               (interactive)
               (if-let* ((dl-type (ytdl--get-download-type))
                         (track (mpv-get-property "path"))
                         (title (mpv-get-property "media-title")))
                   (ytdl--download-async
                    track
                    (expand-file-name title (ytdl--eval-field (nth 1 dl-type)))
                    (ytdl--eval-list (ytdl--eval-field (nth 2 dl-type)))
                    'ignore
                    (car dl-type))
                 (error "mpv is not currently active")))

             (defun rde-mpv-store-link ()
               "Store a link to an mpv track."
               (when (and (mpv-live-p)
                          (not (equal (mpv--with-json
                                       (mpv-get-property "video"))
                                      'false))
                          (string-match "mpv:"
                                        (buffer-name (current-buffer))))
                 (let ((url (mpv-get-property "path"))
                       (title (mpv-get-property "media-title")))
                   (org-link-store-props
                    :type "mpv"
                    :link url
                    :description title))))

             (defun rde-mpv-capture ()
               "Store and capture the current mpv playback link."
               (interactive)
               (with-current-buffer
                   (car (cl-remove-if-not
                         (lambda (buffer)
                           (string-match "mpv:" (buffer-name buffer)))
                                          (buffer-list)))
                 (org-store-link t t)
                 (org-capture nil "tv")))

             (cl-defun rde-mpv-play-url-other-window (url
                                                      &rest args
                                                      &key &allow-other-keys)
               "Launch an mpv process for URL and ARGS in another window."
               (interactive "sURI: ")
               (apply 'rde-mpv-play-url url args)
               (switch-to-buffer-other-window (current-buffer)))

             (defun rde-mpv-seek-start ()
               "Seek to the start of the current MPV stream."
               (interactive)
               (mpv-seek 0))

             (defun rde-mpv-connect-to-emms-on-startup (data)
               "Connect to the EMMS process when mpv is started with DATA."
               (interactive)
               (when (string= (alist-get 'event data) "start-file")
                 (rde-mpv-connect-to-emms-proc)))

             (defun rde-mpv-connect-to-emms-proc ()
               "Connect to a running EMMS MPV process."
               (interactive)
               (setq mpv-playing-time-string "")
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
                 (setq mpv--queue
                       (tq-create (make-network-process
                                   :name "emms-mpv-socket"
                                   :family 'local
                                   :service emms-player-mpv-ipc-socket
                                   :coding '(utf-8 . utf-8)
                                   :noquery t
                                   :filter 'emms-player-mpv-ipc-filter
                                   :sentinel 'emms-player-mpv-ipc-sentinel)))
                 (set-process-filter
                  (tq-process mpv--queue)
                  (lambda (_proc string)
                    (ignore-errors
                      (mpv--tq-filter mpv--queue string)))))
               (run-hooks 'mpv-on-start-hook)
               (run-hooks 'mpv-started-hook)
               (when (equal mpv--process
                            emms-player-mpv-proc)
                 (mpv-display-mode-line))
               t)

             (defun rde-mpv-playlist-shuffle ()
               "Toggle the shuffle state for the current playlist."
               (interactive)
               (mpv-run-command "playlist-shuffle"))

             (defun rde-mpv-kill-url (original-p)
               "Copy the URL in the current mpv stream to the system clibpoard.
If ORIGINAL-P, ensure the original service URL is killed rather than a
proxy url as per `rde-browse-url-mappings'."
               (interactive
                (list
                 (yes-or-no-p "Copy original URL?")))
               (when-let* ((title (mpv-get-property "media-title"))
                           (url (mpv-get-property "path"))
                           (original-url
                            (if original-p
                                (rde-browse-url--transform-host url)
                              (rde-browse-url--transform-host
                               url :alt nil))))
                 (kill-new original-url)
                 (message (format "Copied \"%s\" to the system clipboard"
                                  title))))

             (defun rde-mpv-set-transient-map ()
               "Set a transient map for transient MPV commands."
               (interactive)
               (set-transient-map
                (let ((map (make-sparse-keymap)))
                  (define-key map "b" 'mpv-seek-backward)
                  (define-key map "f" 'mpv-seek-forward)
                  (define-key map "p" 'mpv-pause)
                  map)
                t))

             ,@(if (get-value 'emacs-modus-themes config)
                   '((defun rde-mpv-change-theme (&optional theme)
                       "Set mpv theme according to system theme or THEME."
                       (interactive)
                       (if (or (and theme
                                    (rde-modus-themes--dark-theme-p theme))
                               (rde-modus-themes--dark-theme-p))
                           (progn
                             (mpv-set-property "background" "#000000")
                             (mpv-set-property "osd-color" "#ffffff"))
                         (mpv-set-property "background" "#ffffff")
                         (mpv-set-property "osd-color" "#323232")))
                     (add-hook 'mpv-started-hook 'rde-mpv-change-theme)
                     (add-hook 'rde-modus-themes-after-enable-theme-hook
                               'rde-mpv-change-theme))
                   '())

             (advice-add 'mpv-kill :override 'rde-mpv-kill)
             (with-eval-after-load 'org
               (org-link-set-parameters
                "mpv"
                :store 'rde-mpv-store-link))
             (let ((map mode-specific-map))
               (define-key map "mc" 'mpv-jump-to-chapter)
               (define-key map "ml" 'mpv-jump-to-playlist-entry)
               (define-key map "mn" 'mpv-playlist-next)
               (define-key map "mp" 'mpv-playlist-prev)
               (define-key map "mN" 'mpv-chapter-next)
               (define-key map "mP" 'mpv-chapter-prev)
               (define-key map "mq" 'mpv-quit)
               (define-key map "mR" 'mpv-set-ab-loop)
               (define-key map (kbd "m SPC") 'mpv-pause)
               (define-key map "mr" 'mpv-toggle-loop)
               (define-key map "mv" 'mpv-toggle-video)
               (define-key map "m\r" 'rde-mpv-play-url)
               (define-key map "ms" 'rde-mpv-download)
               (define-key map "ma" 'rde-mpv-seek-start)
               (define-key map "mw" 'rde-mpv-kill-url))
             (define-key mode-specific-map "M" 'rde-mpv-set-transient-map)
             (autoload 'mpv-mode-line-mode "mpv")
             (mpv-mode-line-mode)
             (autoload 'mpv-playing-time-mode "mpv")
             (mpv-playing-time-mode)
             (advice-add 'mpv-start :around 'rde-browse-url-add-scheme)
             (with-eval-after-load 'mpv
               (setq mpv-seek-step 3)
               (setq mpv-display-title-truncate-threshold 25)
               ,@(if (get-value 'emacs-all-the-icons config)
                     '((eval-when-compile
                         (require 'all-the-icons))
                       (with-eval-after-load 'all-the-icons
                         (setq mpv-display-prev-file-indicator
                               (all-the-icons-material
                                "skip_previous" :v-adjust -0.1 :height 1))
                         (setq mpv-display-next-file-indicator
                               (all-the-icons-material
                                "skip_next" :v-adjust -0.1 :height 1))
                         (setq mpv-display-pause-indicator
                               (all-the-icons-material
                                "pause" :v-adjust -0.14 :height 1))
                         (setq mpv-display-resume-indicator
                               (all-the-icons-material
                                "play_arrow" :v-adjust -0.14 :height 1))))
                   '()))
             ,@(if emacs-embark
                   `((with-eval-after-load 'embark
                       (define-key embark-url-map "v" 'rde-mpv-play-url)
                       (defvar rde-mpv-chapter-embark-actions
                         (let ((map (make-sparse-keymap)))
                           (define-key map "r" 'mpv-set-chapter-ab-loop)))
                       (add-to-list 'embark-keymap-alist
                                    (cons 'mpv-chapter
                                          'rde-mpv-chapter-embark-actions))
                       (defvar rde-mpv-file-embark-actions
                         (let ((map (make-sparse-keymap)))
                           (define-key map "d" 'mpv-remove-playlist-entry)))
                       (add-to-list 'embark-keymap-alist
                                    (cons 'mpv-file
                                          'rde-mpv-file-embark-actions))))
                 '()))
           #:elisp-packages (append
                             (list emacs-mpv)
                             (or (and=> emacs-embark list) '())
                             (or (and=> emacs-all-the-icons list) '()))))))
     (if (get-value 'nyxt config)
         (list
          (rde-nyxt-configuration-service
           f-name
           config
           '((defun play-video (url &rest extra-args &key &allow-other-keys)
               "Play stream from URL with EXTRA-ARGS in an Emacs mpv process."
               (let* ((nyxt::*interactive-p* t)
                      (url (render-url (quri:uri url)))
                      (playlist (null (member (eval-in-emacs
                                               '(mpv-get-property "playlist"))
                                              '("nil" "[]") :test 'string=)))
                      (play-or-enqueue
                       (when playlist
                         (nyxt:prompt1
                          :prompt "Select"
                          :sources (make-instance
                                    'prompter:yes-no-source
                                    :constructor '("Play" "Enqueue")))))
                      (formats (delete-duplicates
                                (remove-if-not
                                 (lambda (f)
                                   (ppcre:scan "\\d+x\\d+" f))
                                 (mapcar (lambda (f) (getf f :resolution))
                                         (with-input-from-string
                                             (s (eval-in-emacs
                                                 `(ytdl--list-formats ,url)))
                                           (read s))))
                                :test 'equal))
                      (format (when formats
                                (ppcre:register-groups-bind (height)
                                    ("\\d+x(\\d+)"
                                     (nyxt:prompt1
                                      :prompt "Format"
                                      :sources (make-instance
                                                'prompter:source
                                                :name "Formats"
                                                :constructor formats)))
                                  (format nil "best[height<=~a]" height))))
                      (res (and play-or-enqueue
                                (string= play-or-enqueue "Enqueue"))))
                 (eval-in-emacs
                  `(apply 'rde-mpv-play-url ,url ,format
                          :select nil :playlist ,res ',extra-args))))

             (define-command play-video-current (&optional
                                                 (buffer (current-buffer)))
               "Play contents of BUFFER in an Emacs-controlled mpv process."
               (play-video (render-url (url buffer))))

             (define-key *rde-keymap* "C-c v" 'play-video-current))))
         '())))

  (feature
   (name f-name)
   (values `((,f-name . ,mpv)
             (emacs-mpv . ,emacs-mpv)))
   (home-services-getter get-home-services)))

(define* (feature-youtube-dl
          #:key
          (youtube-dl yt-dlp)
          (emacs-ytdl emacs-ytdl)
          (download-dir "~/downloads")
          (music-dir "~/music")
          (video-dir "~/videos")
          (music-dl-args '())
          (video-dl-args '())
          (youtube-dl-key "y"))
  "Configure youtube-dl, a command-line program to download videos
from YouTube and various other sites."
  (ensure-pred any-package? youtube-dl)
  (ensure-pred file-like? ffmpeg)
  (ensure-pred file-like? emacs-ytdl)
  (ensure-pred path? download-dir)
  (ensure-pred path? music-dir)
  (ensure-pred path? video-dir)
  (ensure-pred list? music-dl-args)
  (ensure-pred list? video-dl-args)
  (ensure-pred string? youtube-dl-key)

  (define f-name 'youtube-dl)

  (define (get-home-services config)
    "Return home services related to youtube-dl."
    (define yt-dlp (file-append youtube-dl "/bin/yt-dlp"))
    (define ffmpeg-bin (file-append ffmpeg "/bin/ffmpeg"))

    (list
     (rde-elisp-configuration-service
      f-name
      config
      `((with-eval-after-load 'rde-keymaps
          (define-key rde-app-map (kbd ,youtube-dl-key) 'ytdl-show-list))
        (with-eval-after-load 'ytdl
          (define-key ytdl--dl-list-mode-map "a" 'ytdl-download)
          (setq ytdl-command ,yt-dlp)
          (setq ytdl-download-folder ,download-dir)
          (setq ytdl-music-folder ,music-dir)
          (setq ytdl-video-folder ,video-dir)
          (setq ytdl-mode-line nil)
          (setq ytdl-music-extra-args
                (list ,@music-dl-args "--ffmpeg-location" ,ffmpeg-bin))
          (setq ytdl-video-extra-args
                (list ,@video-dl-args "--ffmpeg-location" ,ffmpeg-bin))))
      #:elisp-packages (list emacs-ytdl))))

  (feature
   (name f-name)
   (values `((,f-name . ,youtube-dl)
             (emacs-ytdl . ,emacs-ytdl)))
   (home-services-getter get-home-services)))
