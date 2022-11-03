(define-module (conses features video)
  #:use-module (conses utils)
  #:use-module (conses packages emacs-xyz)
  #:use-module (conses features web-browsers)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
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
           (audio/mp3 . mpv.desktop)))))
      (service home-mpv-service-type
               (home-mpv-configuration
                (package mpv)
                (default-options
                 `((script . ,(file-append mpv-mpris "/lib/mpris.so"))
                   (background . ,(if gtk-light-theme? "#ffffff" "#000000"))
                   (osd-font . ,font-sans-serif)
                   (osd-color . ,(if gtk-light-theme? "#323232" "#ffffff"))
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
         (cl-defun configure-mpv-play-url (url &key (force-private-p nil) (audio-only nil) (repeat nil))
           "Prompt for video quality before calling `mpv-start' on URL.
If FORCE-PRIVATE-P, ensure to use a privacy-friendly alternative of URL
as defined in `configure-browse-url-mappings'.  You can additionally specify whether
to play the file as AUDIO-ONLY and if to REPEAT it by default."
           (interactive "sURI: ")
           (let* ((formats
                   (mapcar (lambda (format)
                             (let* ((format-res (string-match (rx (+ num) "x" (group (+ num)))
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
                           (configure-ytdl--list-stream-formats url)))
                  (selected-format (and formats
                                        (alist-get
                                         (completing-read "Resolution: "
                                                          (lambda (string pred action)
                                                            (if (eq action 'metadata)
                                                                `(metadata
                                                                  ,(cons 'display-sort-function 'identity))
                                                              (complete-with-action action formats string pred))))
                                         formats nil nil 'equal)))
                  (extra-args (split-string
                               (concat
                                (when formats (format "--ytdl-format=%s" selected-format))
                                (when audio-only " --video=no")
                                (when repeat " --loop-file=inf")))))
             (when force-private-p
               (setq url (configure-browse-url--transform-host url)))
             (if (mpv-get-property "playlist")
                 (pcase (completing-read "Play or Enqueue: " '("Play" "Enqueue"))
                   ("Play" (apply 'mpv-start url extra-args))
                   ("Enqueue" (apply 'mpv-playlist-append-url url extra-args)))
               (apply 'mpv-start url extra-args))))

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
                'ignore
                download-type)
             (error "`mpv' is not currently active")))

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
         (add-hook 'emms-player-mpv-event-functions 'configure-mpv-connect-to-emms-on-startup)
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
           ,@(if (get-value 'emacs-all-the-icons config)
                 '((eval-when-compile
                     (require 'all-the-icons))
                   (with-eval-after-load 'all-the-icons
                     (setq mpv-prev-entry-indicator
                           (all-the-icons-material "skip_previous" :v-adjust -0.1 :height 1))
                     (setq mpv-next-entry-indicator
                           (all-the-icons-material "skip_next" :v-adjust -0.1 :height 1))
                     (setq mpv-pause-indicator
                           (all-the-icons-material "pause" :v-adjust -0.14 :height 1))
                     (setq mpv-resume-indicator
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
           '((define-command play-video-in-current-page (&optional (buffer (current-buffer)))
               "Play video in the currently open buffer."
               (let ((url (render-url (url buffer))))
                 (eval-in-emacs
                  `(configure-mpv-play-url ,url))))
             (define-key *rde-keymap* "C-c v" 'play-video-in-current-page))))
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
          (video-dl-args '()))
  "Configure youtube-dl, a command-line program to download videos
from YouTube and various other sites."
  (ensure-pred any-package? youtube-dl)
  (ensure-pred any-package? emacs-ytdl)
  (ensure-pred path? download-dir)
  (ensure-pred path? music-dir)
  (ensure-pred path? video-dir)
  (ensure-pred list? music-dl-args)
  (ensure-pred list? video-dl-args)

  (define f-name 'youtube-dl)

  (define (get-home-services config)
    "Return home services related to youtube-dl."
    (list
     (simple-service
      'home-youtube-dl-profile-service
      home-profile-service-type
      (list yt-dlp ffmpeg))
     (rde-elisp-configuration-service
      f-name
      config
      `((require 'configure-rde-keymaps)
        (defun configure-ytdl--list-stream-formats (url)
          "List all available formats for the stream with URL with yt-dlp."
          (with-temp-buffer
            (call-process (executable-find "yt-dlp") nil t nil "--list-formats" url)
            (goto-char (point-min))
            (let ((formats
                   (cl-loop while (not (eobp))
                            do (forward-line 1)
                            when (re-search-forward
                                  (rx bol (+ digit) (+ blank) (group (+ alphanumeric)) (+ blank)
                                      (group (+ alphanumeric) (? blank) (+ alphanumeric))
                                      (+ blank) (group (+ alphanumeric)))
                                  (point-at-eol) t)
                            collect (list (match-string 2)
                                          (match-string 1))))
                  result)
              (dolist (fmt formats result)
                (unless (member fmt result)
                  (push fmt result)))
              result)))

        (define-key rde-app-map "y" 'ytdl-show-list)
        (with-eval-after-load 'ytdl
          (define-key ytdl--dl-list-mode-map "a" 'ytdl-download)
          (setq ytdl-command ,(file-append yt-dlp "/bin/yt-dlp"))
          (setq ytdl-download-folder ,download-dir)
          (setq ytdl-music-folder ,music-dir)
          (setq ytdl-video-folder ,video-dir)
          (setq ytdl-mode-line nil)
          (setq ytdl-music-extra-args ',music-dl-args)
          (setq ytdl-video-extra-args ',video-dl-args)))
      #:elisp-packages (list emacs-ytdl
                             (get-value 'emacs-configure-rde-keymaps config)))))

  (feature
   (name f-name)
   (values `((,f-name . ,youtube-dl)
             (emacs-ytdl . ,emacs-ytdl)))
   (home-services-getter get-home-services)))
