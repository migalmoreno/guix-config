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
  (let ((commit "c7eaaa35cf5b1c2b776c0f7859fd4a9200d2fa23")
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
           "1mci0v3fxdgs5yk1619y27iy1kzm5rg2svabwva60dnid8hrl13b"))))
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
