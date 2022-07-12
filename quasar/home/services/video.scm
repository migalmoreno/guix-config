(define-module (quasar home services video)
  #:use-module (quasar home packages emacs-xyz)
  #:use-module (conses home services web-browsers)
  #:use-module (conses home services emacs)
  #:use-module (conses home services gtk)
  #:use-module (conses packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home-services video)
  #:use-module (gnu home-services base)
  #:use-module (gnu home services xdg)
  #:use-module (gnu packages video)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages kodi)
  #:use-module (gnu packages graphics)
  #:use-module (guix gexp)
  #:export (mpv-service
            youtube-dl-service))

(define (mpv-service)
  (define (run-with-emacs command)
    (format #f "run \"/bin/sh\" \"-c\" \"emacsclient -e '~s'\"" command))

  (list
   (home-generic-service
    'home-mpv-packages
    #:packages (list obs kodi blender))
   (simple-service 'add-kodi-envs
                   home-environment-variables-service-type
                   '(("KODI_DATA" . "$XDG_DATA_HOME/kodi")))
   (simple-service 'home-mpv-xdg
                   home-xdg-mime-applications-service-type
                   (home-xdg-mime-applications-configuration
                    (default
                     '((video/mp4 . mpv.desktop)
                       (video/mkv . mpv.desktop)
                       (audio/mp3 . mpv.desktop)))))
   (service home-mpv-service-type
            (home-mpv-configuration
             (default-options
               `((border . no)
                 (volume . 100)
                 (screenshot-directory . ,(string-append (or (getenv "XDG_DATA_HOME") "~/.local/share") "/mpv/screenshots"))
                 (autofit . 800x800)
                 (background . ,(if (gtk-theme-light-p) "\"#ffffff\"" "\"#000000\""))
                 (osd-font . "IBM Plex Sans")
                 (osd-border-size . 2)
                 (osd-bar . yes)
                 (osd-color . ,(if (gtk-theme-light-p) "\"#323232\"" "\"#ffffff\""))
                 (osd-level . 0)
                 (slang . en)
                 (sub-font . "IBM Plex Sans")
                 (ytdl-raw-options . "ignore-config=,sub-lang=en,write-auto-sub=")
                 (script-opts-add=osc-visibility . never)
                 (script-opts-add=osc-windowcontrols . no)))
             (bindings
              `(("ctrl+a" . "seek 0 absolute-percent")
                ("ctrl+e" . "seek 100 absolute-percent")
                ("ctrl+f" . "seek 5 relative")
                ("ctrl+b" . "seek -5 relative")
                ("Shift+n" . "add chapter 1")
                ("Shift+p" . "add chapter -1")
                ("F" . "cycle fullscreen")
                ("M" . "cycle mute")
                ("+" . "add volume 2")
                ("-" . "add volume -2")
                (":" . "script-binding console/enable")
                ("s" . "screenshot video")
                ("Q" . "quit-watch-later")
                ("D" . ,(run-with-emacs '(eb-media-mpv-download)))
                ("Alt+c" . ,(run-with-emacs '(eb-media-mpv-capture-link)))
                ("O" . "no-osd cycle-values osd-level 3 0")
                ("o" . "osd-bar show-progress")
                ("v" . "cycle sub-visibility")
                ("b" . "cycle sub")
                ("n" . "script-message osc-visibility always")
                ("N" . "script-message osc-visibility never")
                ("L" . "cycle-values loop-file \"inf\" \"no\"")))))
   (elisp-configuration-service
    `((let ((map mode-specific-map))
        (define-key map "Mc" 'mpv-jump-to-chapter)
        (define-key map "Ml" 'mpv-jump-to-file)
        (define-key map "MN" 'mpv-chapter-next)
        (define-key map "MP" 'mpv-chapter-prev)
        (define-key map "Mq" 'mpv-quit)
        (define-key map "MR" 'mpv-set-ab-loop)
        (define-key map "Mr" 'mpv-toggle-loop)
        (define-key map "Mv" 'mpv-toggle-video)
        (define-key map "M\r" 'eb-media-mpv-start)
        (define-key map "Ms" 'eb-media-mpv-download)
        (define-key map "Ma" 'eb-media-mpv-seek-start)
        (define-key map "Mw" 'eb-media-mpv-kill-url))
       (define-key mode-specific-map "m" 'eb-media-mpv-set-transient-map)
       (eb-media-mpv-mode-line-mode)
       (eb-media-mpv-playing-time-mode)
       (add-hook 'mpv-on-exit-hook 'eb-media-mpv-mode-line-clear)
       (advice-add 'mpv-start :around 'eb-web-add-url-scheme)
       (with-eval-after-load 'mpv
         (custom-set-variables
          '(mpv-seek-step 3)))
      ,#~""
      (with-eval-after-load 'embark
        (let ((map embark-url-map))
          (define-key map "v" 'eb-media-mpv-start)
          (define-key map "c" 'browse-url-chromium))
        (embark-define-keymap embark-mpv-chapter-actions
                              "Keymap for actions on mpv chapters."
                              ("r" mpv-set-chapter-ab-loop))
        (add-to-list 'embark-keymap-alist '(mpv-chapter . embark-mpv-chapter-actions))
        (embark-define-keymap embark-mpv-file-actions
                              "Keymap for actions on mpv playlist entries."
                              ("d" mpv-remove-file))
        (add-to-list 'embark-keymap-alist '(mpv-file . embark-mpv-file-actions)))
      ,#~""
      (advice-add 'ytdl--download-async :override 'eb-media-ytdl--download-async))
    #:elisp-packages (list emacs-mpv-next))
   (nyxt-configuration-service
    '((define-command play-video-in-current-page (&optional (buffer (current-buffer)))
        "Plays video in the currently open buffer."
        (let ((video-url (render-url (url buffer))))
          (echo "Opening ~s with mpv." video-url)
          (eval-in-emacs
           `(eb-media-empv-start ,video-url))))
      (define-key *custom-keymap*
        "M-c v" 'play-video-in-current-page)))))

(define (youtube-dl-service)
  (list
   (home-generic-service 'youtube-dl-packages #:packages (list yt-dlp ffmpeg))
   (elisp-configuration-service
    `((let ((map mode-specific-map))
        (define-key map "Yd" 'ytdl-download)
        (define-key map "Yl" 'ytdl-show-list))
      (with-eval-after-load 'ytdl
        (custom-set-variables
         '(ytdl-download-folder (xdg-user-dir "DOWNLOAD"))
         '(ytdl-music-folder (expand-file-name "~/music"))
         '(ytdl-video-folder (expand-file-name "~/videos"))
         '(ytdl-mode-line nil)
         '(ytdl-music-extra-args '("-q" "-x" "--add-metadata" "--audio-format" "mp3"))
         '(ytdl-video-extra-args '("-q" "-f" "bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best"
                                   "--add-metadata" "--compat-options" "all")))))
    #:elisp-packages (list emacs-ytdl))))
