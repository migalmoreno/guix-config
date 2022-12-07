(define-module (conses features video)
  #:use-module (conses utils)
  #:use-module (conses packages emacs-xyz)
  #:use-module (conses features web-browsers)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
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
      (format #f (match command
                   ((? string? e) "run \"/bin/sh\" \"-c\" \"emacsclient -e ~a\"'")
                   (_ "run \"/bin/sh\" \"-c\" \"emacsclient -e '~s'\"")) command))
    (define gtk-light-theme? (get-value 'gtk-dark-theme? config))
    (define font-sans-serif (get-value 'font-sans-serif config))

    (append
     (list
      (simple-service
       'home-mpv-mime-applications-service
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
                   ("D" . ,(run-with-emacs '(configure-mpv-download)))
                   ("Alt+c" . ,(run-with-emacs '(configure--mpv-capture)))
                   ,@extra-bindings))))
      (rde-elisp-configuration-service
       f-name
       config
       `((eval-when-compile
           (require 'mpv)
           (require 'cl-lib))
         (cl-defun configure-mpv-play-url (url &key (audio nil) (repeat nil) (formats t) (select t) (playlist nil))
           "Play URL with `mpv-start'.
You can specify whether to PLAY the file as AUDIO, if you want to be
prompted for FORMATS, to REPEAT the file, manually SELECT what to do with the file,
and whether to add the file to the current PLAYLIST."
           (interactive "sURI: ")
           (let* ((format (and formats (ytdl-select-format url)))
                  (extra-args (split-string
                               (concat
                                (format "--ytdl-format=%s" (or format "best"))
                                (and audio " --video=no")
                                (and repeat " --loop-file=inf")))))
             (if (and select (mpv-get-property "playlist"))
                 (pcase (completing-read "Play or Enqueue: " '("Play" "Enqueue"))
                   ("Play" (apply 'mpv-start url extra-args))
                   ("Enqueue" (apply 'mpv-playlist-append-url url extra-args)))
               (if (and playlist (mpv-get-property "playlist"))
                   (apply 'mpv-playlist-append-url url extra-args)
                 (apply 'mpv-start url extra-args)))))

         (defun configure-mpv-kill ()
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
           (run-hooks 'mpv-finished-hook))

         (defun configure-mpv-download ()
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

         (defun configure-mpv-store-link ()
           "Store a link to an mpv track."
           (when (and (mpv-live-p)
                      (not (equal (mpv--with-json
                                   (mpv-get-property "video"))
                                  'false))
                      (string-match "mpv:" (buffer-name (current-buffer))))
             (let ((url (mpv-get-property "path"))
                   (title (mpv-get-property "media-title")))
               (org-link-store-props
                :type "mpv"
                :link url
                :description title))))

         (defun configure-mpv-capture ()
           "Store and capture the current mpv playback link."
           (interactive)
           (with-current-buffer
               (car (cl-remove-if-not (lambda (buffer)
                                        (string-match "mpv:" (buffer-name buffer)))
                                      (buffer-list)))
             (org-store-link t t)
             (org-capture nil "tv")))

         (cl-defun configure-mpv-play-url-other-window (url &rest args &key &allow-other-keys)
           "Launch an mpv process for URL and ARGS in another window."
           (interactive "sURI: ")
           (apply 'configure-mpv-play-url url args)
           (switch-to-buffer-other-window (current-buffer)))

         (defun configure-mpv-seek-start ()
           "Seek to the start of the current MPV stream."
           (interactive)
           (mpv-seek 0))

         (defun configure-mpv-connect-to-emms-on-startup (data)
           "Connect to the EMMS process when mpv is started with DATA."
           (interactive)
           (when (string= (alist-get 'event data) "start-file")
             (configure-mpv-connect-to-emms-proc)))

         (defun configure-mpv-connect-to-emms-proc ()
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
             (setq mpv--queue (tq-create (make-network-process
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

         (defun configure-mpv-playlist-shuffle ()
           "Toggle the shuffle state for the current playlist."
           (interactive)
           (mpv-run-command "playlist-shuffle"))

         (defun configure-mpv-kill-url (original-p)
           "Copy the URL in the current mpv stream to the system clibpoard.
If ORIGINAL-P, ensure the original service URL is killed rather than a
proxy url as per `configure-browse-url-mappings'."
           (interactive
            (list
             (yes-or-no-p "Copy original URL?")))
           (when-let* ((title (mpv-get-property "media-title"))
                       (url (mpv-get-property "path"))
                       (original-url (if original-p
                                         (configure-browse-url--transform-host url)
                                       (configure-browse-url--transform-host url :alt-p nil))))
             (kill-new original-url)
             (message (format "Copied \"%s\" to the system clipboard" title))))

         (defun configure-mpv-set-transient-map ()
           "Set a transient map for transient MPV commands."
           (interactive)
           (set-transient-map
            (let ((map (make-sparse-keymap)))
              (define-key map "b" 'mpv-seek-backward)
              (define-key map "f" 'mpv-seek-forward)
              (define-key map "p" 'mpv-pause)
              map)
            t))

         (advice-add 'mpv-kill :override 'configure-mpv-kill)
         (with-eval-after-load 'org
           (org-link-set-parameters
            "mpv"
            :store 'configure-mpv-store-link))
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
           (define-key map "m\r" 'configure-mpv-play-url)
           (define-key map "ms" 'configure-mpv-download)
           (define-key map "ma" 'configure-mpv-seek-start)
           (define-key map "mw" 'configure-mpv-kill-url))
         (define-key mode-specific-map "M" 'configure-mpv-set-transient-map)
         (autoload 'mpv-mode-line-mode "mpv")
         (mpv-mode-line-mode)
         (autoload 'mpv-playing-time-mode "mpv")
         (mpv-playing-time-mode)
         (advice-add 'mpv-start :around 'configure-browse-url-add-scheme)
         (with-eval-after-load 'mpv
           (setq mpv-seek-step 3)
           (setq mpv-display-title-truncate-threshold 25)
           ,@(if (get-value 'emacs-all-the-icons config)
                 '((eval-when-compile
                     (require 'all-the-icons))
                   (with-eval-after-load 'all-the-icons
                     (setq mpv-display-prev-file-indicator
                           (all-the-icons-material "skip_previous" :v-adjust -0.1 :height 1))
                     (setq mpv-display-next-file-indicator
                           (all-the-icons-material "skip_next" :v-adjust -0.1 :height 1))
                     (setq mpv-display-pause-indicator
                           (all-the-icons-material "pause" :v-adjust -0.14 :height 1))
                     (setq mpv-display-resume-indicator
                           (all-the-icons-material "play_arrow" :v-adjust -0.14 :height 1))))
               '()))
         ,@(if (get-value 'emacs-embark config)
               `((eval-when-compile
                   (require 'embark))
                 (with-eval-after-load 'embark
                   (define-key embark-url-map "v" 'configure-mpv-play-url)
                   (embark-define-keymap embark-mpv-chapter-actions
                     "Keymap for actions on mpv chapters."
                     ("r" mpv-set-chapter-ab-loop))
                   (add-to-list 'embark-keymap-alist '(mpv-chapter . embark-mpv-chapter-actions))
                   (embark-define-keymap
                       embark-mpv-file-actions
                     "Keymap for actions on mpv playlist entries."
                     ("d" mpv-remove-playlist-entry))
                   (add-to-list 'embark-keymap-alist '(mpv-file . embark-mpv-file-actions))))
             '()))
       #:elisp-packages (append
                         (list emacs-mpv)
                         (if (get-value 'emacs-embark config)
                             (list (get-value 'emacs-embark config))
                             '())
                         (if (get-value 'emacs-all-the-icons config)
                             (list (get-value 'emacs-all-the-icons config))
                             '()))))
     (if (get-value 'nyxt config)
         (list
          (rde-nyxt-configuration-service
           f-name
           config
           '((defun play-video-mpv (url &rest extra-args &key &allow-other-keys)
               "Play stream from URL with EXTRA-ARGS in an Emacs-controlled mpv process."
               (let* ((nyxt::*interactive-p* t)
                      (url (render-url (quri:uri url)))
                      (playlist (null (member (eval-in-emacs '(mpv-get-property "playlist"))
                                              '("nil" "[]") :test 'string=)))
                      (play-or-enqueue (when playlist
                                         (nyxt:prompt1
                                          :prompt "Select"
                                          :sources (make-instance 'prompter:yes-no-source
                                                                  :constructor '("Play" "Enqueue")))))
                      (res (and play-or-enqueue (string= play-or-enqueue "Enqueue"))))
                 (eval-in-emacs
                  `(apply 'configure-mpv-play-url ,url :select nil :playlist ,res ',extra-args))))
             (define-command play-video-mpv-current-buffer (&optional (buffer (current-buffer)))
               "Play contents of BUFFER in an Emacs-controlled mpv process."
               (play-video-mpv (render-url (url buffer))))
             (define-key *rde-keymap* "C-c v" 'play-video-mpv-current-buffer))))
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
      `((require 'configure-rde-keymaps)
        (define-key rde-app-map (kbd ,youtube-dl-key) 'ytdl-show-list)
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
      #:elisp-packages (list emacs-ytdl
                             (get-value 'emacs-configure-rde-keymaps config)))))

  (feature
   (name f-name)
   (values `((,f-name . ,youtube-dl)
             (emacs-ytdl . ,emacs-ytdl)))
   (home-services-getter get-home-services)))
