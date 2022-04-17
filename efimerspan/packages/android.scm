(define-module (franesio packages android)
  #:use-module (gnu packages android)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages))

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
