(define-module (efimerspan packages web-browsers)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web-browsers)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public nyxt-next
  (let ((commit "8e6e2b4ad8b825e431e6ecb882000aa4fc04f7e4")
        (revision "2"))
    (package
      (inherit nyxt)
      (version (git-version "3" revision commit))
      (source
       (origin
         (inherit (package-source nyxt))
         (uri (git-reference
               (url "https://github.com/atlas-engineer/nyxt")
               (commit commit)))
         (file-name (git-file-name "nyxt" version))
         (sha256
          (base32
           "1r1w097a4f2x1yc3p5kf5rv7zzbclr7id4zdhm3v4zpf9zc9h6gr"))))
      (native-inputs
       (modify-inputs (package-native-inputs nyxt)
         (prepend pkg-config)))
      (propagated-inputs
       (list
        gst-libav
        gst-plugins-bad
        gst-plugins-base
        gst-plugins-good
        gst-plugins-ugly
        aspell
        aspell-dict-en))
      (inputs
       (modify-inputs (package-inputs nyxt)
         (prepend sbcl-cl-gopher
                  sbcl-nhooks
                  sbcl-phos
                  sbcl-cl-tld
                  sbcl-nfiles
                  sbcl-cl-webkit))))))
