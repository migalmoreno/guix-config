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
  (let ((commit "df86d95a62d5a5f03b2744092945f47f7631a449")
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
           "1j6im0ybnkji67l3swgqamwqgf1az4j54nijpf4r8bh6r1b30hgn"))))
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
