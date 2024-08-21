(define-module (migalmoreno users vega wayland)
  #:use-module ((migalmoreno users vega multimedia) #:prefix multimedia:)
  #:use-module (migalmoreno utils)
  #:use-module (dtao-guile home-service)
  #:use-module (dwl-guile home-service)
  #:use-module (dwl-guile patches)
  #:use-module (farg colors)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services xdg)
  #:use-module (guix gexp)
  #:use-module (rde features base)
  #:use-module (rde features bluetooth)
  #:use-module (rde features gnupg)
  #:use-module (rde features keyboard)
  #:use-module (rde features networking)
  #:use-module (rde features linux)
  #:use-module (rde features wm)
  #:use-module (rde features xdg)
  #:use-module (rde features xdisorg)
  #:use-module (rde home services desktop)
  #:use-module (rde home services shells)
  #:use-module (rde packages))

(define (bemenu-options palette)
  (define (%palette v) (format #f "~s" (palette v)))
  (list
   "-H" "28" "--fn" "\"Iosevka 11\"" "--ignorecase"
   ;; "--hp" "10" "--cw" "1" "--ch" "20"
   "--tf" (%palette 'fg)
   "--tb" (format #f "~s" (farg:offset (palette 'bg) '20))
   "--ff" (%palette 'fg) "--fb" (%palette 'bg-alt)
   "--nf" (%palette 'fg) "--nb" (%palette 'bg-alt)
   "--af" (%palette 'fg) "--ab" (%palette 'bg-alt)
   "--cf" (%palette 'fg) "--cb" (%palette 'bg-alt)
   "--hf" (%palette 'fg) "--hb" (%palette 'accent-0)))

(define extra-home-packages-service
  (simple-service
   'add-extra-home-packages
   home-profile-service-type
   (strings->packages
    "ddcutil" "light" "v4l-utils" "binutils" "wireguard-tools" "texinfo"
    "pass-otp" "docker-cli" "docker-compose" "b4"
    "gst-plugins-good" "gst-plugins-bad" "gst-plugins-ugly"
    "gst-plugins-base" "gst-libav" "wl-clipboard" "emacs-arei" "guile-next"
    "guile-ares-rs")))

(define extra-home-environment-variables-service
  (simple-service
   'add-extra-home-environment-variables
   home-environment-variables-service-type
   `(("GPG_TTY" . "$(tty)")
     ("LESSHISTFILE" . "-")
     ("npm_config_userconfig" . "$XDG_CONFIG_HOME/npm-config")
     ("npm_config_cache" . "$XDG_CACHE_HOME/npm")
     ("npm_config_prefix" . "$XDG_DATA_HOME/npm"))))

(define extra-xdg-desktop-entries-service
  (simple-service
   'add-extra-desktop-entries
   home-xdg-mime-applications-service-type
   (home-xdg-mime-applications-configuration
    (desktop-entries
     (list
      (xdg-desktop-entry
       (file "emulator")
       (name "Emulator")
       (type 'application)
       (config
        `((exec . ,#~(string-join
                      (list
                       #$(file-append
                          (@ (gnu packages package-management) guix)
                          "/bin/guix")
                       "time-machine" "-C"
                       #$(project-file "rde/channels-lock.scm") "--"
                       "shell" "-C" "-N" "--emulate-fhs"
                       "--share=/tmp/.X11-unix" "--share=/dev/shm"
                       "--expose=/etc/machine-id" "--share=$HOME"
                       "--preserve='^ANDROID'" "--preserve='^DISPLAY$'"
                       "--preserve='^XAUTHORITY$'" "--share=/dev/kvm"
                       "--share=/dev/video0" "--share=/dev/dri"
                       "-m" #$(project-file
                               "src/migalmoreno/manifests/android.scm")
                       "--" "env" "DISPLAY=:0"
                       #$(string-append "LD_LIBRARY_PATH="
                                        "/lib:/lib/nss:"
                                        "~/.android/emulator/lib64/qt/lib:"
                                        "~/.android/emulator/lib64")
                       "~/.android/emulator/emulator" "-show-kernel" "-gpu"
                       "swiftshader_indirect" "-camera-back" "webcam0"
                       "-avd" "whatsapp_bridge" "-no-snapshot")))
          (icon . "android")
          (comment . "Run an Android emulator")))))))))

(define* (grim-script #:key select? clipboard?)
  `(begin
     (use-modules (srfi srfi-19))
     (dwl:shcmd
      ,(file-append (@ (gnu packages image) grim) "/bin/grim")
      ,@(if select?
            `("-g" "\"$("
              ,(file-append (@ (gnu packages image) slurp) "/bin/slurp")
              ")\"")
            '())
      ,@(if clipboard?
            `("-" "|" ,(file-append (@ (gnu packages xdisorg) wl-clipboard)
                                    "/bin/wl-copy"))
            '("-t" "jpeg"
              (format #f "~a/pictures/~a.jpg"
                      (getenv "HOME")
                      (date->string (current-date) "~Y~m~d-~H~M~S")))))))

(define (extra-home-wm-services _ palette)
  (let* ((dwl-guile-package (patch-dwl-guile-package
                             dwl-guile
                             #:patches (list %patch-xwayland
                                             %patch-focusmonpointer
                                             %patch-monitor-config)))
         (dtao-guile-package (@ (dtao-guile packages) dtao-guile))
         (pamixer (file-append (@ (gnu packages pulseaudio) pamixer)
                               "/bin/pamixer"))
         (shepherd-configuration (home-shepherd-configuration
                                  (auto-start? #f)
                                  (daemonize? #f)))
         (shepherd (home-shepherd-configuration-shepherd
                    shepherd-configuration)))
    (list
     (service home-shepherd-service-type shepherd-configuration)
     (service home-dwl-guile-service-type
              (home-dwl-guile-configuration
               (package dwl-guile-package)
               (auto-start? #f)
               (config
                (list
                 `((setq inhibit-defaults? #t
                         border-px 2
                         border-color ,(palette 'bg)
                         focus-color ,(farg:offset (palette 'accent-0) 10)
                         root-color ,(palette 'bg-alt)
                         tags (map number->string (iota 5 1))
                         smart-gaps? #t
                         smart-borders? #t
                         gaps-oh 12
                         gaps-ov 12
                         gaps-ih 12
                         gaps-iv 12)
                   (dwl:set-tag-keys "s" "s-S")
                   (set-keys "s-d" '(dwl:shcmd
                                     ,(file-append
                                       (@ (gnu packages xdisorg)
                                          j4-dmenu-desktop)
                                       "/bin/j4-dmenu-desktop")
                                     (format #f "--dmenu='~a ~a'"
                                             ,(file-append
                                               (@ (gnu packages xdisorg)
                                                  bemenu)
                                               "/bin/bemenu")
                                             ,(string-join
                                               (bemenu-options palette))))
                             "s-x" '(dwl:shcmd
                                     ,(file-append
                                       (@ (gnu packages wm) swaylock-effects)
                                       "/bin/swaylock"))
                             "s-j" '(dwl:focus-stack 1)
                             "s-k" '(dwl:focus-stack -1)
                             "s-l" '(dwl:change-master-factor 0.05)
                             "s-h" '(dwl:change-master-factor -0.05)
                             "s-q" 'dwl:kill-client
                             "S-s-<escape>" 'dwl:quit
                             "<XF86PowerOff>" 'dwl:quit
                             "s-f" 'dwl:toggle-fullscreen
                             "s-c" '(dwl:cycle-layout 1)
                             "s-<page-up>" '(dwl:change-masters 1)
                             "s-<page-down>" '(dwl:change-masters -1)
                             "s-<space>" 'dwl:zoom
                             "s-S-0" '(dwl:view 0)
                             "s-\\" 'dwl:toggle-gaps
                             "s-[" '(dwl:change-gaps -6)
                             "s-]" '(dwl:change-gaps 6)
                             "s-S-<space>" 'dwl:toggle-floating
                             "s-<mouse-left>" 'dwl:move
                             "s-<mouse-right>" 'dwl:resize
                             "s-<mouse-middle>" 'dwl:toggle-floating
                             "s-<prior>" '(dwl:shcmd ,pamixer "--unmute"
                                                     "--increase" "5")
                             "s-<next>" '(dwl:shcmd ,pamixer "--unmute"
                                                    "--decrease" "5")
                             "s-<insert>" ',(grim-script #:select? #t)
                             "s-<delete>" ',(grim-script))
                   (set-layouts 'default "[]=" 'dwl:tile
                                'monocle "|M|" 'dwl:monocle)
                   (set-monitor-rules '((name . "eDP-1")
                                        (masters . 1)
                                        (master-factor . 0.55)
                                        (width . 1920)
                                        (height . 1200)
                                        (layout . default))
                                      '((name . "DP-2")
                                        (masters . 1)
                                        (master-factor . 0.55)
                                        (width . 1920)
                                        (height . 1080)
                                        (layout . default))
                                      '((name . "HDMI-A-1")
                                        (masters . 1)
                                        (master-factor . 0.55)
                                        (x . 1920)
                                        (y . 0)
                                        (width . 1920)
                                        (height . 1080)
                                        (layout . default)))
                   (set-rules '((id . "nyxt")
                                (tags . 2))
                              '((title . "Android Emulator")
                                (floating? . #t)))
                   (add-hook!
                    dwl:hook-startup
                    (lambda ()
                      (dwl:start-repl-server)
                      (dwl:shcmd
                       ,(file-append
                         (@ (gnu packages wm) swaybg)
                         "/bin/swaybg")
                       "-i" ,(palette 'wallpaper) "-m" "stretch")
                      (dwl:shcmd
                       ,(file-append shepherd "/bin/shepherd")
                       "--logfile"
                       (format #f "~a/log/shepherd.log"
                               (or (getenv "XDG_STATE_HOME")
                                   (format #f "~a/.local/state"
                                           (getenv "HOME"))))))))))))
     (simple-service
      'add-rde-dtao-guile-service
      home-shepherd-service-type
      (list
       (shepherd-service
        (documentation "Run dtao-guile in RDE.")
        (provision '(rde-dtao-guile))
        (respawn? #t)
        (start
         #~(make-forkexec-constructor
            (list
             #$(file-append dtao-guile-package "/bin/dtao-guile")
             "-c" #$(string-append (getenv "HOME")
                                   "/.config/dtao-guile/config.scm"))
            #:user (getenv "USER")
            #:log-file
            #$(format #f "~a/log/dtao-guile.log"
                      (or (getenv "XDG_STATE_HOME")
                          (format #f "~a/.local/state" (getenv "HOME"))))))
        (stop #~(make-kill-destructor)))))
     (simple-service
      'restart-wm-services-on-change
      home-run-on-change-service-type
      `(("files/.config/dwl-guile/config.scm"
         ,#~(begin
              (system* #$(file-append dwl-guile-package "/bin/dwl-guile")
                       "-e" "(dwl:reload-config)")
              (system*
               #$(file-append dwl-guile-package "/bin/dwl-guile") "-e"
               (format #f "(dwl:shcmd ~s \"-i\" ~s \"-m\" \"stretch\")"
                       #$(file-append
                          (@ (gnu packages wm) swaybg)
                          "/bin/swaybg")
                       #$(palette 'wallpaper)))))
        ("files/.config/dtao-guile/config.scm"
         ,#~(system* #$(file-append shepherd "/bin/herd") "restart"
                     "rde-dtao-guile"))))
     (simple-service
      'run-dwl-guile-on-login-tty
      home-shell-profile-service-type
      (list
       #~(format #f "[ $(tty) = /dev/tty1 ] && exec ~a"
                 #$(program-file
                    "dwl-guile-start"
                    #~(system*
                       #$(file-append dwl-guile-package "/bin/dwl-guile")
                       "-c"
                       (string-append (getenv "HOME")
                                      "/.config/dwl-guile/config.scm"))))))
     (service home-dtao-guile-service-type
              (home-dtao-guile-configuration
               (package dtao-guile-package)
               (auto-start? #f)
               (config
                (dtao-config
                 (font "Iosevka:style=Regular:size=13")
                 (background-color (palette 'bg-alt))
                 (foreground-color (palette 'fg))
                 (padding-left 0)
                 (padding-top 0)
                 (padding-bottom 20)
                 (border-px 0)
                 (modules '((ice-9 match)
                            (ice-9 popen)
                            (ice-9 regex)
                            (ice-9 rdelim)
                            (ice-9 textual-ports)))
                 (block-spacing 0)
                 (height 28)
                 (delimiter-right " ")
                 (left-blocks
                  (append
                   (map
                    (lambda (tag)
                      (let ((str (string-append
                                  "^p(8)" (number->string tag) "^p(8)"))
                            (index (- tag 1)))
                        (dtao-block
                         (events? #t)
                         (click `(match button
                                   (0 (dtao:view ,index))))
                         (render
                          `(cond
                            ((dtao:selected-tag? ,index)
                             ,(format #f "^bg(~a)^fg(~a)~a^fg()^bg()"
                                      (palette 'accent-0)
                                      (if (palette 'light?)
                                          (palette 'fg)
                                          (palette 'bg-alt))
                                      str))
                            ((dtao:urgent-tag? ,index)
                             ,(format #f "^bg(~a)^fg(~a)~a^fg()^bg()"
                                      (palette 'red) (palette 'accent-0)
                                      str))
                            ((dtao:active-tag? ,index)
                             ,(format #f "^bg(~a)^fg(~a)~a^fg()^bg()"
                                      (farg:offset (palette 'bg) 20)
                                      (palette 'fg) str))
                            (else ,str))))))
                    (iota 5 1))
                   (list
                    (dtao-block
                     (events? #t)
                     (click `(dtao:next-layout))
                     (render
                      `(format #f "^p(8)~a^p(8)" (dtao:get-layout)))))))
                 (center-blocks
                  (list
                   (dtao-block
                    (events? #t)
                    (render
                     `(let ((title (dtao:title)))
                        (if (> (string-length title) 80)
                            (string-append (substring title 0 80) "...")
                            title))))))
                 (right-blocks
                  (list
                   (dtao-block
                    (interval 60)
                    (render
                     `(let* ((port (open-input-pipe
                                    ,(file-append
                                      (@ (gnu packages linux) acpi)
                                      "/bin/acpi")))
                             (str (get-string-all port)))
                        (close-port port)
                        (string-append
                         "^p(8)BAT: "
                         (match:substring
                          (string-match ".*, ([0-9]+%)" str) 1)
                         "^p(4)"))))
                   (dtao-block
                    (interval 1)
                    (render
                     `(let* ((port (open-input-pipe
                                    (string-append
                                     ,pamixer " --get-volume-human")))
                             (str (read-line port)))
                        (close-pipe port)
                        (unless (eof-object? str)
                          (string-append "^p(4)VOL: " str "^p(8)"))))
                    (click
                     `(match button
                        (0 (system
                            (string-append ,pamixer " --toggle-mute"))))))
                   (dtao-block
                    (interval 1)
                    (render
                     `(strftime "%A, %d %b (w.%V) %T"
                                (localtime (current-time))))))))))))))

(define (extra-home-desktop-services source palette)
  (append
   (@@ (rde features base) %rde-desktop-home-services)
   (extra-home-wm-services source palette)
   (list
    multimedia:extra-mpv-settings-service
    extra-home-packages-service
    extra-xdg-desktop-entries-service
    extra-home-environment-variables-service
    (service home-udiskie-service-type
             (home-udiskie-configuration
              (config '((notify . #f))))))))

(define-public (features source palette)
  (list
   (feature-gnupg
    #:gpg-primary-key "5F23F458"
    #:ssh-keys `((,%default-ssh-keygrip))
    #:pinentry-flavor
    (program-file
     "pinentry-bemenu-wrapper"
     #~(system* #$(file-append
                   (@ (gnu packages gnupg) pinentry-bemenu)
                   "/bin/pinentry-bemenu")
                #$@(bemenu-options palette)))
    #:default-ttl 34560000)
   (feature-desktop-services
    #:default-desktop-home-services
    (extra-home-desktop-services source palette))
   (feature-networking)
   (feature-pipewire)
   (feature-swaylock
    #:swaylock (@ (gnu packages wm) swaylock-effects)
    #:extra-config
    `((screenshots)
      (clock)
      (indicator)
      (effect-blur . 7x5)
      (effect-vignette . "0.5:0.5")
      (hide-keyboard-layout)
      ,(cons 'indicator-color (substring (palette 'accent-0) 1))
      ,(cons 'inside-color (substring (palette 'fg) 1))
      ,(cons 'ring-color (substring (palette 'accent-0) 1))))
   (feature-swayidle)
   (feature-kanshi
    #:extra-config
    `((profile headless ((output eDP-1 enable)))
      (profile single ((output eDP-1 disable)
                       (output HDMI-A-1 enable)))
      (profile multi ((output eDP-1 disable)
                      (output DP-2 mode 1920x1080 position -1920,0)
                      (output HDMI-A-1 mode 1920x1080 position 0,0)))))
   (feature-bluetooth)
   (feature-keyboard
    #:keyboard-layout %default-keyboard-layout)
   (feature-xdg
    #:xdg-user-directories-configuration
    (home-xdg-user-directories-configuration
     (desktop "$HOME")
     (documents "$HOME/documents")
     (download "$HOME/downloads")
     (music "$HOME/music")
     (pictures "$HOME/pictures")
     (publicshare "$HOME/public")
     (videos "$HOME/videos")
     (templates "$HOME")))))
