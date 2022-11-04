(define-module (conses packages web-browsers)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages web-browsers)
  #:use-module (guix packages)
  #:use-module (guix git-download))

(define-public nyxt-next
  (let ((commit "cba29f9714e0f0914887c0db7a0363ae087f91e9")
        (revision "1"))
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
           "0ancmbqpkzlnwp4g2f7gfwdpcb3mk8wsfrwsm87i168h8kn6bnj4"))))
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
                  sbcl-dissect
                  sbcl-ndebug
                  sbcl-cl-tld
                  sbcl-nfiles
                  sbcl-ospm
                  sbcl-py-configparser
                  sbcl-cl-webkit
                  sbcl-slynk))))))
