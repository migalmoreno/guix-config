(define-module (efimerspan packages web-browsers)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web-browsers)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public nyxt-next
  (let ((commit "b6020ef4c06d0dd40097c9dd530303ade334a00a")
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
           "0yinrzj2wjpf6yd5x5prm0yhx3d5rqk0lj08m7r5i65xrnnqyh64"))))
      (inputs
       (modify-inputs (package-inputs nyxt)
         (prepend sbcl-cl-gopher sbcl-nhooks sbcl-phos sbcl-cl-tld sbcl-nfiles))))))
