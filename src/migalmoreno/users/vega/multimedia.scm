(define-module (migalmoreno users vega multimedia)
  #:use-module (gnu services)
  #:use-module (rde features bittorrent)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features video)
  #:use-module (rde home services video))

(define (mpv-run-with-emacs cmd)
  (format #f (string-append
              "run \"/bin/sh\" \"-c\" \"emacsclient -e "
              (if (string? cmd)
                  "~a\""
                  "'~s'\""))
          cmd))

(define extra-mpv-config
  `((global ((border . no)
             (volume . 100)
             ,(cons 'screenshot-directory
                    (string-append
                     (or (getenv "XDG_DATA_HOME") "~/.local/share")
                     "/mpv/screenshots"))
             (autofit . 800x800)
             (osd-border-size . 2)
             (osd-bar . yes)
             (osd-level . 0)
             (slang . en)
             (ytdl-raw-options . "ignore-config=,sub-lang=en,write-auto-sub=")
             (script-opts-add=osc-visibility . never)
             (script-opts-add=osc-windowcontrols . no)
             (save-position-on-quit . #t)))))

(define extra-mpv-bindings
  `("ctrl+a seek 0 absolute-percent"
    "ctrl+e seek 100 absolute-percent"
    "ctrl+f seek 5 relative"
    "ctrl+b seek -5 relative"
    "Shift+n add chapter 1"
    "Shift+p add chapter -1"
    "F cycle fullscreen"
    (format #f "D ~a" ,(mpv-run-with-emacs '(rde-mpv-download)))
    (format #f "Alt+c ~a" ,(mpv-run-with-emacs '(rde-mpv-capture)))
    "M cycle mute"
    "+ add volume 2"
    "- add volume -2"
    ": script-binding console/enable"
    "s screenshot video"
    "Q quit-watch-later"
    "O no-osd cycle-values osd-level 3 0"
    "o osd-bar show-progress"
    "v cycle sub-visibility"
    "b cycle sub"
    "n script-message osc-visibility always"
    "N script-message osc-visibility never"
    "L cycle-values loop-file \"inf\" \"no\""))

(define-public extra-mpv-settings-service
  (simple-service
   'add-mpv-extra-settings
   home-mpv-service-type
   (home-mpv-extension
    (mpv-conf extra-mpv-config)
    (input-conf extra-mpv-bindings))))

(define base-ytdl-args '("-q" "--add-metadata" "--compat-options" "all"))

(define-public multimedia-features
  (list
   (feature-transmission
    #:download-dir (format #f "~a/videos" (getenv "HOME")))
   (feature-yt-dlp
    #:music-dl-args
    `("-x" "-f" "bestaudio" "--audio-format" "mp3" ,@base-ytdl-args)
    #:video-dl-args
    `("-f" "bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best"
      ,@base-ytdl-args))
   (feature-mpv)
   (feature-emacs-emms)))
