(define-module (efimerspan packages android)
  #:use-module (efimerspan packages python-xyz)
  #:use-module (gnu packages)
  #:use-module (gnu packages android)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages protobuf)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:))

(define-public adb-next
  (package
    (inherit adb)
    (version "11.0.0_r1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://android.googlesource.com/platform/system/core")
             (commit (string-append "android-" version))))
       (file-name (string-append "android-platform-system-core-"
                                 version "-checkout"))
       (sha256
        (base32
         "0a10rnn24fpkjzmfa7d6ymx0am0hqclgs7nhiaw4h7rw7k12iihb"))))))

(define-public payload-dumper
  (let ((commit "60224410cbe9e937cc158d5eb376b56d8b40e12b")
        (revision "0"))
    (package
      (name "payload-dumper")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/vm03/payload_dumper")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1lhl4qxqi7lnwaxh494s71sn1dldshapcbim9y13gqb96y0am43v"))))
      (inputs
       (list python-3 python-protobuf python-six python-bsdiff4))
      (build-system python-build-system)
      (arguments
       (list #:use-setuptools? #f
             #:tests? #f
             #:phases
             #~(modify-phases %standard-phases
                 (delete 'build)
                 (replace 'install
                   (lambda* (#:key outputs #:allow-other-keys)
                     (begin
                       (use-modules (guix build utils))
                       (let* ((python (string-append #$(this-package-input "python") "/bin"))
                              (bin (string-append #$output "/bin"))
                              (target (string-append bin "/payload_dumper"))
                              (version (python-version #$(this-package-input "python")))
                              (pydir (string-append #$output "/lib/python" version "/site-packages")))
                         (mkdir-p bin)
                         (copy-file (string-append #$source "/payload_dumper.py") target)
                         (install-file "update_metadata_pb2.py" pydir)
                         ;; Can't use patch-shebang possibly due to a Unicode BOM
                         (substitute* target
                           (("/usr/bin/env python") (which "python3")))
                         (chmod target #o555))))))))
      (synopsis "Android OTA payload dumper.")
      (description "Script to extract the payload of incremental and full OTA's.")
      (home-page "https://github.com/vm03/payload_dumper")
      (license license:unlicense))))
