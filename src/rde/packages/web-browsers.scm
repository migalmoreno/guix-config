(define-module (rde packages web-browsers)
  #:use-module (rde packages lisp-xyz)
  #:use-module (guix packages)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages web-browsers)
  #:use-module (guix git-download))

(define-public nyxt-next
  (let ((commit "d115fbec7874843aa88cebb6a83a46d778f36e83")
        (revision "0"))
    (package
      (inherit nyxt)
      (name "nyxt-next")
      (version (git-version "3.3" revision commit))
      (source
       (origin
         (inherit (package-source nyxt))
         (uri (git-reference
               (url "https://github.com/atlas-engineer/nyxt")
               (commit commit)))
         (file-name (git-file-name "nyxt" version))
         (sha256
          (base32
           "0cndm01ci8hdxfb02h9a6b4a5mad6j7ry79mnlr7bmwp7amhn83f"))))
      (native-inputs
       (modify-inputs (package-native-inputs nyxt)
         (delete sbcl-prove)
         (prepend sbcl-lisp-unit2)))
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
         (delete sbcl-cl-css)
         (prepend sbcl-cl-gopher
                  sbcl-nhooks
                  sbcl-nkeymaps
                  sbcl-phos
                  sbcl-dissect
                  sbcl-ndebug
                  sbcl-cl-tld
                  sbcl-history-tree
                  sbcl-montezuma
                  sbcl-nfiles
                  sbcl-ospm
                  sbcl-lass
                  sbcl-njson
                  sbcl-py-configparser
                  sbcl-cl-webkit
                  sbcl-nsymbols-next
                  sbcl-slynk
                  pkg-config
                  gcc-toolchain))))))

(define-public nyxt-next-sans-gst
  (package
   (inherit nyxt-next)
   (name "nyxt-next-sans-gst")
   (propagated-inputs
    (modify-inputs (package-propagated-inputs nyxt-next)
      (delete "gst-libav" "gst-plugins-bad" "gst-plugins-base"
              "gst-plugins-good" "gst-plugins-ugly")))))
