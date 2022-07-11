(define-module (conses packages web-browsers)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web-browsers)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public nyxt-next
  (let ((commit "fb58a43b8ae7251db76823713aef6f1583544283")
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
           "1254kshv2zkchdz7kinmwjvykhdvigjrddgxs21i4sfni3xw23p6"))))
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
                  sbcl-nkeymaps
                  sbcl-phos
                  sbcl-cl-tld
                  sbcl-nfiles
                  sbcl-cl-webkit))))))
