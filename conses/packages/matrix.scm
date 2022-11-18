(define-module (conses packages matrix)
  #:use-module (conses packages golang)
  #:use-module (conses packages python-xyz)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages video)
  #:use-module (gnu packages matrix)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages syncthing)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-crypto)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix build-system go)
  #:use-module (guix build-system python)
  #:use-module (guix build utils)
  #:use-module (guix build python-build-system)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:))

(define-public synapse-next
  (package
    (inherit synapse)
    (name "synapse-next")
    (version "1.62.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "matrix-synapse" version))
       (sha256
        (base32 "0y0kx79dwhnywmf4ijhn5zmawmi3xcxm02wwvqm422f5blxqg1np"))))
    (arguments
     `(#:tests? #f))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs synapse)
       (append python-matrix-common
               python-ijson
               python-cryptography-next)
       (replace "python-twisted" python-twisted-next)
       (replace "python-pyopenssl" python-pyopenssl-next)
       (replace "python-service-identity" python-service-identity-next)
       (replace "python-txacme" python-txacme-next)
       (replace "python-treq" python-treq-next)
       (replace "python-matrix-synapse-ldap3" python-matrix-synapse-ldap3-next)
       (replace "python-urllib3" python-urllib3-next)
       (replace "python-requests" python-requests-next)
       (replace "python-pysaml2" python-pysaml2-next)
       (replace "python-jsonschema" python-jsonschema-next)))))

(define-public pantalaimon-next
  (package
    (inherit pantalaimon)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs pantalaimon)
       (replace "python-keyring" python-keyring-next)))))

(define-public mautrix-whatsapp
  (package
    (name "mautrix-whatsapp")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/mautrix/whatsapp")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dj13lnwgq48fv7njdjhksrigv6ahf0aliknrmih25dkla4gba64"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "maunium.net/go/mautrix-whatsapp"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'fix-symlinks
            (lambda _
              (delete-file-recursively "src/go.mau.fi/whatsmeow")
              (copy-recursively (string-append #$(this-package-input "go-mau-fi-whatsmeow")
                                               "/src/go.mau.fi/whatsmeow")
                                (string-append (getenv "GOPATH") "/src/go.mau.fi/whatsmeow"))
              (delete-file-recursively "src/maunium.net/go/mautrix")
              (copy-recursively (string-append #$(this-package-input "go-maunium-net-go-mautrix")
                                               "/src/maunium.net/go/mautrix")
                                (string-append (getenv "GOPATH") "/src/maunium.net/go/mautrix"))
              #t)))))
    (native-inputs
     (list libolm ffmpeg))
    (propagated-inputs
     (list
      go-github-com-gorilla-websocket
      go-github-com-skip2-go-qrcode
      go-github-com-tidwall-sjson
      go-github-com-tidwall-gjson
      go-mau-fi-whatsmeow
      go-maunium-net-go-maulogger
      go-maunium-net-go-mautrix
      go-maunium-net-go-mauflag
      go-github-com-gorilla-mux
      go-github-com-mattn-go-sqlite3
      go-github-com-lib-pq
      go-github-com-yuin-goldmark
      go-golang-org-x-text
      go-github-com-prometheus-client-golang
      go-google-golang-org-protobuf-proto
      go-golang-org-x-image
      go-gopkg-in-yaml-v2))
    (home-page "https://github.com/mautrix/whatsapp")
    (synopsis "A Matrix-WhatsApp puppeting bridge.")
    (description "A Matrix-WhatsApp puppeting bridge based on @code{whatsmeow}.")
    (license license:agpl3)))
