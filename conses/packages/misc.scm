(define-module (conses packages misc)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:))

(define-public gnu-meditate-logo
  (package
    (name "gnu-meditate-logo")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "https://www.gnu.org/graphics/meditate.png")
       (sha256 (base32 "1i8cb417hzm0ly5mi2m7ccf0qdpamqdgbrs7h33xhblfjx282pvg"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       ,#~(begin
            (use-modules (guix build utils))
            (let ((filename (basename #$source)))
              (install-file #$source #$output)
              (chdir #$output)
              (rename-file filename "meditate.png"))
            #t)))
    (home-page "https://www.gnu.org/graphics/meditate.en.html")
    (synopsis "Levitating, Meditating, Flute-playing gnu color drawing.")
    (description "This color drawing depicts a levitating gnu, deep in in meditation, wrapped
snugly in a gold robe. Both he and his computer float gracefully above the floor, only his tail
lightly touches the ground.")
    (license license:gpl3+)))
