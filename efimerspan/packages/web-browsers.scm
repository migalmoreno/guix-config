(define-module (efimerspan packages web-browsers)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web-browsers)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public nyxt-next
  (let ((commit "8ba300a668adc2d99aae743f3a3c0ace0a6d6b51")
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
           "12j8smr7zc17dg10885bm94an31fx82z0wwy7lrv668hnmdx9xv8"))))
      (inputs
       (modify-inputs (package-inputs nyxt)
         (prepend sbcl-cl-gopher sbcl-nhooks sbcl-phos sbcl-cl-tld sbcl-nfiles))))))
