(define-module (efimerspan packages web-browsers)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web-browsers)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public nyxt-next
  (let ((commit "363b407e7e4e3201c8346db9c94d8bb1f4d85fa5")
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
           "12z3s1jcdhnalxwkridhfhg4rayz0rs51cyagzk7f0ximkw7ji5s"))))
      (inputs
       (modify-inputs (package-inputs nyxt)
         (prepend sbcl-cl-gopher sbcl-nhooks sbcl-phos sbcl-cl-tld sbcl-nfiles))))))
