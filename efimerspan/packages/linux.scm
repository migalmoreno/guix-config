(define-module (efimerspan packages linux)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages linux))

(define-public wireplumber-0.4.5
  (package
    (inherit wireplumber)
    (version "0.4.5")
    (source
     (origin
       (inherit (package-source wireplumber))
       (uri (git-reference
             (url
              "https://gitlab.freedesktop.org/pipewire/wireplumber.git")
             (commit version)))
       (file-name (git-file-name "wireplumber" version))
       (sha256
        (base32 "1k56i5cardwr03vkldg68714hyksyp1vb0b315yw1bilaj2ka30i"))))))
