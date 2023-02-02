(define-module (conses packages video)
  #:use-module (gnu packages video)
  #:use-module (guix packages)
  #:use-module (guix git-download))

(define-public mpv-34
  (package
    (inherit mpv)
    (name "mpv")
    (version "0.34.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mpv-player/mpv")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "12qxwm1ww5vhjddl8yvj1xa0n1fi9z3lmzwhaiday2v59ca0qgsk"))))))
