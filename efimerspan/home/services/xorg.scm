(define-module (efimerspan home services xorg)
  #:use-module (efimerspan home services glib)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu home services)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system setuid)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (home-unclutter-service-type
            home-unclutter-configuration
            xinitrc
            xorg-start-command
            xorg-configuration))

(define-configuration/no-serialization home-unclutter-configuration
  (package
    (package unclutter)
    "The @code{unclutter} package to use.")
  (seconds
   (integer 5)
   "The number of idle seconds to wait for @code{unclutter} to remove the cursor."))

(define (unclutter-shepherd-service config)
  (list
   (shepherd-service
    (provision '(home-unclutter))
    (requirement '(home-dbus))
    (stop #~(make-kill-destructor))
    (start #~(make-forkexec-constructor
              (list
               #$(file-append (home-unclutter-configuration-package config) "/bin/unclutter")
               "-display" ":0" "-idle"
               (number->string #$(home-unclutter-configuration-seconds config)))
              #:log-file (string-append
                          (or (getenv "XDG_LOG_HOME")
                              (format #f "~a/.local/var/log"
                                      (getenv "HOME")))
                          "/unclutter.log"))))))

(define (unclutter-profile-service config)
  (list (home-unclutter-configuration-package config)))

(define home-unclutter-service-type
  (service-type
   (name 'home-unclutter)
   (extensions
    (list
     (service-extension home-profile-service-type
                        unclutter-profile-service)
     (service-extension home-shepherd-service-type
                        unclutter-shepherd-service)))
   (default-value (home-unclutter-configuration))
   (description "Sets up an unclutter daemon.")))

(define %default-xorg-modules
  ;; Default list of modules loaded by the server.  When multiple drivers
  ;; match, the first one in the list is loaded.
  (list xf86-input-libinput
        ;; Libinput is the new thing and is recommended over evdev/synaptics:
        ;; <http://who-t.blogspot.fr/2015/01/xf86-input-libinput-compatibility-with.html>.
        ))

(define %default-xorg-fonts
  ;; Default list of fonts available to the X server.
  (list (file-append font-alias "/share/fonts/X11/75dpi")
        (file-append font-alias "/share/fonts/X11/100dpi")
        (file-append font-alias "/share/fonts/X11/misc")
        (file-append font-alias "/share/fonts/X11/cyrillic")
        (file-append font-misc-misc               ;default fonts for xterm
                     "/share/fonts/X11/misc")
        (file-append font-adobe75dpi "/share/fonts/X11/75dpi")))

(define %default-xorg-server-arguments
  ;; Default command-line arguments for X.
  '("-nolisten" "tcp"))

(define-record-type* <xorg-configuration>
  xorg-configuration make-xorg-configuration
  xorg-configuration?
  (modules          xorg-configuration-modules    ;list of file-like
                    (thunked)
                    ;; filter out modules not supported on current system
                    (default (filter
                              (lambda (p)
                                (member (%current-system)
                                        (package-supported-systems p)))
                              %default-xorg-modules)))
  (fonts            xorg-configuration-fonts      ;list of packges
                    (default %default-xorg-fonts))
  (drivers          xorg-configuration-drivers    ;list of strings
                    (default '()))
  (resolutions      xorg-configuration-resolutions ;list of tuples
                    (default '()))
  (keyboard-layout  xorg-configuration-keyboard-layout ;#f | <keyboard-layout>
                    (default #f))
  (extra-config     xorg-configuration-extra-config ;list of strings
                    (default '()))
  (server           xorg-configuration-server     ;file-like
                    (default xorg-server))
  (server-arguments xorg-configuration-server-arguments ;list of strings
                    (default %default-xorg-server-arguments)))

(define (xorg-configuration->file config)
  "Compute an Xorg configuration file corresponding to CONFIG, an
<xorg-configuration> record."
  (let ((xorg-server (xorg-configuration-server config)))
    (define all-modules
      ;; 'xorg-server' provides 'fbdevhw.so' etc.
      (append (xorg-configuration-modules config)
              (list xorg-server)))

    (define build
      #~(begin
          (use-modules (ice-9 match)
                       (srfi srfi-1)
                       (srfi srfi-26))

          (call-with-output-file #$output
            (lambda (port)
              (define drivers
                '#$(xorg-configuration-drivers config))

              (define (device-section driver)
                (string-append "
Section \"Device\"
  Identifier \"device-" driver "\"
  Driver \"" driver "\"
EndSection"))

              (define (screen-section driver resolutions)
                (string-append "
Section \"Screen\"
  Identifier \"screen-" driver "\"
  Device \"device-" driver "\"
  SubSection \"Display\"
    Modes "
  (string-join (map (match-lambda
                      ((x y)
                       (string-append "\"" (number->string x)
                                      "x" (number->string y) "\"")))
                    resolutions)) "
  EndSubSection
EndSection"))

              (define (input-class-section layout variant model options)
                (string-append "
Section \"InputClass\"
  Identifier \"evdev keyboard catchall\"
  MatchIsKeyboard \"on\"
  Option \"XkbLayout\" " (object->string layout)
  (if variant
      (string-append "  Option \"XkbVariant\" \""
                     variant "\"")
      "")
  (if model
      (string-append "  Option \"XkbModel\" \""
                     model "\"")
      "")
  (match options
    (()
     "")
    (_
     (string-append "  Option \"XkbOptions\" \""
                    (string-join options ",") "\""))) "

  MatchDevicePath \"/dev/input/event*\"
  Driver \"evdev\"
EndSection\n"))

              (define (expand modules)
                ;; Append to MODULES the relevant /lib/xorg/modules
                ;; sub-directories.
                (append-map (lambda (module)
                              (filter-map (lambda (directory)
                                            (let ((full (string-append module
                                                                       directory)))
                                              (and (file-exists? full)
                                                   full)))
                                          '("/lib/xorg/modules/drivers"
                                            "/lib/xorg/modules/input"
                                            "/lib/xorg/modules/multimedia"
                                            "/lib/xorg/modules/extensions")))
                            modules))

              (display "Section \"Files\"\n" port)
              (for-each (lambda (font)
                          (format port "  FontPath \"~a\"~%" font))
                        '#$(xorg-configuration-fonts config))
              (for-each (lambda (module)
                          (format port
                                  "  ModulePath \"~a\"~%"
                                  module))
                        (append (expand '#$all-modules)

                                ;; For fbdevhw.so and so on.
                                (list #$(file-append xorg-server
                                                     "/lib/xorg/modules"))))
              (display "EndSection\n" port)
              (display "
Section \"ServerFlags\"
  Option \"AllowMouseOpenFail\" \"on\"
EndSection\n" port)

              (display (string-join (map device-section drivers) "\n")
                       port)
              (newline port)
              (display (string-join
                        (map (cut screen-section <>
                                  '#$(xorg-configuration-resolutions config))
                             drivers)
                        "\n")
                       port)
              (newline port)

              (let ((layout  #$(and=> (xorg-configuration-keyboard-layout config)
                                      keyboard-layout-name))
                    (variant #$(and=> (xorg-configuration-keyboard-layout config)
                                      keyboard-layout-variant))
                    (model   #$(and=> (xorg-configuration-keyboard-layout config)
                                      keyboard-layout-model))
                    (options '#$(and=> (xorg-configuration-keyboard-layout config)
                                       keyboard-layout-options)))
                (when layout
                  (display (input-class-section layout variant model options)
                           port)
                  (newline port)))

              (for-each (lambda (config)
                          (display config port))
                        '#$(xorg-configuration-extra-config config))))))

    (computed-file "xserver.conf" build)))

(define (xorg-configuration-directory modules)
  "Return a directory that contains the @code{.conf} files for X.org that
includes the @code{share/X11/xorg.conf.d} directories of each package listed
in @var{modules}."
  (with-imported-modules '((guix build utils))
    (computed-file "xorg.conf.d"
                   #~(begin
                       (use-modules (guix build utils)
                                    (srfi srfi-1))

                       (define files
                         (append-map (lambda (module)
                                       (find-files (string-append
                                                    module
                                                    "/share/X11/xorg.conf.d")
                                                   "\\.conf$"))
                                     (list #$@modules)))

                       (mkdir #$output)
                       (for-each (lambda (file)
                                   (symlink file
                                            (string-append #$output "/"
                                                           (basename file))))
                                 files)
                       #t))))

(define* (xorg-wrapper xinitrc #:optional (config (xorg-configuration)))
  "Return a derivation that builds a script to start the X server with the
given @var{config}.  The resulting script should be used in place of
@code{/usr/bin/X}."
  (define exp
    ;; Write a small wrapper around the X server.
    #~(begin
        (setenv "XORG_DRI_DRIVER_PATH" (string-append #$mesa "/lib/dri"))
        (setenv "XKB_BINDIR" (string-append #$xkbcomp "/bin"))

        (let ((xinit #$(file-append xinit "/bin/xinit"))
              (X (string-append #$(xorg-configuration-server config) "/bin/X")))
          (apply execl xinit xinit
                 #$xinitrc
                 "--"
                 X
                 ":0"
                 "vt1"
                 "-keeptty"
                 "-xkbdir" (string-append #$xkeyboard-config "/share/X11/xkb")
                 "-config" #$(xorg-configuration->file config)
                 "-configdir" #$(xorg-configuration-directory
                                 (xorg-configuration-modules config))
                 (cdr (command-line))))))

  (program-file "X-wrapper" exp))

(define* (xorg-start-command xinitrc #:optional (config (xorg-configuration)))
  "Return a @code{startx} script in which the modules, fonts, etc. specified
in @var{config}, are available.  The result should be used in place of
@code{startx}."
  (define X
    (xorg-wrapper xinitrc config))

  (define exp
    ;; Write a small wrapper around the X server.
    #~(apply execl #$X #$X ;; Second #$X is for argv[0].
             "-logverbose" "-verbose" "-terminate"
             #$@(xorg-configuration-server-arguments config)
             (cdr (command-line))))

  exp)

(define* (xinitrc #:key wm)
  "Returns a xinitrc script that starts X with the specified WM."
  (define builder
    (let ((name (package-name wm)))
      #~(system* #$(file-append wm "/bin/" (match name
                                             ("emacs-native-comp" "emacs")
                                             (_ name)))
                 "-mm" "--debug-init")))

  (program-file "xinitrc" builder))
