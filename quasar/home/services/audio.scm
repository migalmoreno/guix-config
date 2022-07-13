(define-module (quasar home services audio)
  #:use-module (conses home services emacs)
  #:use-module (conses home services web-browsers)
  #:use-module (conses home services linux)
  #:use-module (gnu home-services base)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages music)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages video)
  #:use-module (guix gexp)
  #:export (audio-service))

(define (audio-service)
  (list
   (service home-pipewire-service-type)
   (home-generic-service
    'home-audio-packages
    #:packages (list
                atomicparsley
                python-mutagen
                perl-image-exiftool))
   (elisp-configuration-service
    `((let ((map mode-specific-map))
       (define-key map "ew" 'emms-smart-browse)
       (define-key map "eb" 'emms-seek-backward)
       (define-key map "ef" 'emms-seek-forward)
       (define-key map "eh" 'emms-history-save)
       (define-key map "eq" 'emms-stop)
       (define-key map "es" 'emms-toggle-random-playlist)
       (define-key map "eS" 'emms-seek-to)
       (define-key map (kbd "e SPC") 'emms-pause)
       (define-key map "er" 'emms-toggle-repeat-track)
       (define-key map "el" 'eb-media-emms-library-load)
       (define-key map "en" 'eb-media-emms-next)
       (define-key map "ep" 'eb-media-emms-previous)
       (define-key map "ea" 'eb-media-emms-seek-to-beginning))
      (with-eval-after-load 'emms
        (require 'emms-setup)
        (require 'emms-info-libtag)
        (emms-all)
        (emms-default-players)
        (emms-toggle-random-playlist)
        (define-key emms-playlist-mode-map "m" 'eb-media-emms-download-track)
        (emms-browser-make-filter
         "all-files" (emms-browser-filter-only-type 'file))
        (emms-browser-make-filter
         "last-week" (emms-browser-filter-only-recent 7))
        (let ((mp3-function (assoc "mp3" emms-tag-editor-tagfile-functions)))
          (add-to-list 'emms-tag-editor-tagfile-functions `("aac" ,(cadr mp3-function) ,(caddr mp3-function)))
          (add-to-list 'emms-tag-editor-tagfile-functions `("m4a" ,(executable-find "AtomicParsley")
                                                            ((info-artist . "--artist")
                                                             (info-title . "--title")
                                                             (info-album . "--album")
                                                             (info-tracknumber . "--tracknum")
                                                             (info-year . "--year")
                                                             (info-genre . "--genre")
                                                             (info-note . "--comment")
                                                             (info-albumartist . "--albumArtist")
                                                             (info-composer . "--composer")))))
        (custom-set-variables
         '(emms-playlist-buffer-name "*Music*")
         '(emms-seek-seconds 15)
         '(emms-source-file-default-directory (expand-file-name "~/music"))
         '(emms-playlist-mode-center-when-go t)
         '(emms-repeat-playlist t)
         '(emms-info-functions '(emms-info-libtag))
         '(emms-browser-covers 'emms-browser-cache-thumbnail-async)
         '(emms-mode-line-format "%s"))
        (append emms-player-mpv-parameters '("--no-video" "--ytdl-format=best"))
        (setq emms-browser-thumbnail-small-size 64
              emms-mode-line-icon-enabled-p nil
              emms-browser-thumbnail-medium-size 128)))
    #:elisp-packages (list emacs-emms))
   (nyxt-configuration-service
    `((define-command emms-source-track ()
        "Sources the current site as an EMMS track."
        (let ((url (quri:render-uri (url (current-buffer)))))
          (echo "Starting to play ~a in EMMS" url)
          (eval-in-emacs
           `(eb-media-emms-source-track
             ,url
             ,(title (current-buffer))
             t))))
      (define-key *custom-keymap*
        "M-c e" 'emms-source-track)))))
