(define-module (conses packages golang)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages protobuf)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module ((guix licenses) #:prefix license:))

(define-public go-filippo-io-edwards25519
  (let ((commit "383e08737b4a509c950b7c284100c55bb1816a16")
        (revision "0"))
    (package
      (name "go-filippo-io-edwards25519")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/FiloSottile/edwards25519")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "04kaxiv2n0kphh8w7qfdfa45jwr4yvafly2ggllir1xwy6npdgi0"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "filippo.io/edwards25519"))
      (home-page "https://github.com/FiloSottile/edwards25519")
      (synopsis "A safer, faster, and more powerful low-level
edwards25519 Go implementation.")
      (description "This library implements the edwards25519 elliptic curve, exposing
the necessary APIs to build a wide array of higher-level primitives.")
      (license license:bsd-3))))

(define-public go-github-com-skip2-go-qrcode
  (let ((commit "da1b6568686e89143e94f980a98bc2dbd5537f13")
        (revision "0"))
    (package
      (name "go-github-com-skip2-go-qrcode")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/skip2/go-qrcode")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0pghd6y2x8a5fqy4rjn4d8j5jcslb236naycdza5an7vyvinsgs9"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/skip2/go-qrcode"
         #:tests? #f))
      (home-page "https://github.com/skip2/go-qrcode")
      (synopsis "QR Code encoder (Go)")
      (description "Package @code{qrcode} implements a QR Code encoder. A QR Code is a matrix
(two-dimensional) barcode. Arbitrary content may be encoded, with URLs being a popular choice.")
      (license license:expat))))

(define-public go-github-com-tidwall-sjson
  (package
    (name "go-github-com-tidwall-sjson")
    (version "1.2.4")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/tidwall/sjson")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p3x52gnznmyld6k9ii3qkmb09vh5sd7zycfm1pjn6mzidzrhvk3"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/tidwall/sjson"))
    (propagated-inputs
     (list
      go-github-com-tidwall-gjson
      go-github-com-tidwall-pretty))
    (home-page "https://github.com/tidwall/sjson")
    (synopsis "Set JSON values very quickly in Go.")
    (description "SJSON is a Go package that provides a very fast and simple way to set a
value in a JSON document.")
    (license license:expat)))

(define-public go-maunium-net-go-mauflag
  (package
    (name "go-maunium-net-go-mauflag")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/tulir/mauflag")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09jv1819jwq5i29y6ngf4j4ii6qwlshydvprfvsfplc419dkz1vx"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "maunium.net/go/mauflag"))
    (home-page "https://github.com/tulir/mauflag")
    (synopsis "An extendable argument parser for Golang.")
    (description "An extendable argument parser for Golang. Mostly follows the GNU Program
Argument Syntax conventions.")
    (license license:gpl3)))

(define-public go-github-com-tidwall-gjson
  (package
    (name "go-github-com-tidwall-gjson")
    (version "1.14.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/tidwall/gjson")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00pw69iq631kjkym0ymbn20d7az1dsxlzc38hqym2y4igvmdpk5z"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/tidwall/gjson"))
    (propagated-inputs
     (list
      go-github-com-tidwall-match
      go-github-com-tidwall-pretty))
    (home-page "https://github.com/tidwall/gjson")
    (synopsis "Get JSON values very quickly - JSON parser for Go.")
    (description "GJSON is a Go package that provides a fast and simple way to get values
from a JSON document. It has features like one line retrieval, dot notation paths, iteration,
and parsing JSON lines.")
    (license license:expat)))

(define-public go-github-com-tidwall-match
  (package
    (name "go-github-com-tidwall-match")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/tidwall/match")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n25md63xr5m66r6zc77n6fgcpv2ljrlk92ivp9hvp8xya22as9k"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/tidwall/match"))
    (home-page "https://github.com/tidwall/match")
    (synopsis "Simple string pattern matcher for Go.")
    (description "@code{Match} is a very simple pattern matcher where @code{*} matches on any
number of characters and @code{?} matches on any one character.")
    (license license:expat)))

(define-public go-github-com-tidwall-pretty
  (package
    (name "go-github-com-tidwall-pretty")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/tidwall/pretty")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11zi5hxb75yapgxq67r4lmv8n910iqmw7994ig1fy4gnr4d51i3s"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/tidwall/pretty"))
    (home-page "https://github.com/tidwall/pretty")
    (synopsis "Efficient JSON beautifier and compactor for Go.")
    (description "@code{Pretty} is a Go package that provides fast methods for formatting
JSON for human readability , or to compact JSON for smaller payloads.")
    (license license:expat)))

(define-public go-google-golang-org-protobuf-proto
  (package
    (name "go-google-golang-org-protobuf-proto")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/protocolbuffers/protobuf-go")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nzcc4qc00afi24nb7nlnwyzvvr6b8s8qdrn1sw085nygh2y2x8r"))))
    (build-system go-build-system)
    (native-inputs
     (list protobuf))
    (arguments
     '(#:import-path "google.golang.org/protobuf/proto"
       #:unpack-path "google.golang.org/protobuf"
       #:tests? #f))
    (home-page "https://github.com/protocolbuffers/protobuf-go")
    (synopsis "Go support for Google's protocol buffers.")
    (description "This project hosts the Go implementation for protocol buffers,
which is a language-neutral, platform-neutral, extensible mechanism for serializing
structured data. The protocol buffer language is a language for specifying the schema
for structured data.")
    (license license:bsd-3)))

(define-public go-google-golang-org-protobuf-reflect-protoreflect
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-reflect-protoreflect")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/reflect/protoreflect"))))))

(define-public go-google-golang-org-protobuf-reflect-protoregistry
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-reflect-protoregistry")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/reflect/protoregistry"))))))

(define-public go-google-golang-org-protobuf-encoding-protowire
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-encoding-protowire")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/encoding/protowire"))))))

(define-public go-google-golang-org-protobuf-encoding-prototext
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-encoding-prototext")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/encoding/prototext"))))))

(define-public go-google-golang-org-protobuf-internal-encoding-text
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-internal-encoding-text")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/internal/encoding/text"))))))

(define-public go-google-golang-org-protobuf-internal-encoding-messageset
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-internal-encoding-messageset")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/internal/encoding/messageset"))))))

(define-public go-google-golang-org-protobuf-internal-encoding-defval
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-internal-encoding-defval")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/internal/encoding/defval"))))))

(define-public go-google-golang-org-protobuf-internal-encoding-tag
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-internal-encoding-tag")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/internal/encoding/tag"))))))

(define-public go-google-golang-org-protobuf-internal-errors
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-internal-errors")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/internal/errors"))))))

(define-public go-google-golang-org-protobuf-internal-set
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-internal-set")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/internal/set"))))))

(define-public go-google-golang-org-protobuf-internal-descfmt
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-internal-descfmt")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/internal/descfmt"))))))

(define-public go-google-golang-org-protobuf-internal-descopts
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-internal-descopts")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/internal/descopts"))))))

(define-public go-google-golang-org-protobuf-internal-flags
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-internal-flags")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/internal/flags"))))))

(define-public go-google-golang-org-protobuf-internal-genid
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-internal-genid")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/internal/genid"))))))

(define-public go-google-golang-org-protobuf-internal-order
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-internal-order")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/internal/order"))))))

(define-public go-google-golang-org-protobuf-internal-pragma
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-internal-pragma")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/internal/pragma"))))))

(define-public go-google-golang-org-protobuf-internal-strs
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-internal-strs")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/internal/strs"))))))

(define-public go-google-golang-org-protobuf-internal-detrand
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-internal-detrand")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/internal/detrand"))))))

(define-public go-google-golang-org-protobuf-internal-filedesc
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-internal-filedesc")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/internal/filedesc"))))))

(define-public go-google-golang-org-protobuf-internal-filetype
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-internal-filetype")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/internal/filetype"))))))

(define-public go-google-golang-org-protobuf-internal-impl
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-internal-impl")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/internal/impl"))))))

(define-public go-google-golang-org-protobuf-internal-version
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-internal-version")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/internal/version"))))))

(define-public go-google-golang-org-protobuf-runtime-protoiface
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-runtime-protoiface")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/runtime/protoiface"))))))

(define-public go-google-golang-org-protobuf-runtime-protoimpl
  (let ((pkg go-google-golang-org-protobuf-proto))
    (package
      (inherit pkg)
      (name "go-google-golang-org-protobuf-runtime-protoimpl")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "google.golang.org/protobuf/runtime/protoimpl"))))))

(define-public go-maunium-net-go-maulogger
  (package
    (name "go-maunium-net-go-maulogger")
    (version "2.3.2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/tulir/maulogger")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kxwx2fby58iws78dlb4cgbgwak35y5b9hswrlrv4gi0pbawwnhn"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "maunium.net/go/maulogger"))
    (home-page "https://github.com/tulir/maulogger")
    (synopsis "A logger for Go programs.")
    (description "A logger in go.")
    (license license:mpl2.0)))

(define-public go-mau-fi-libsignal
  (let ((commit "c40c839ee6a06aca2dfc1a1b78b7f56ac5078832")
        (revision "0"))
    (package
      (name "go-mau-fi-libsignal")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/tulir/libsignal-protocol-go")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0hb0nz792715556iw94cvk0rig1vs1jrg4rv612w4qk7vah2kf1s"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "go.mau.fi/libsignal"
         #:unpack-path "go.mau.fi/libsignal"))
      (propagated-inputs
       (list
        go-filippo-io-edwards25519
        go-github-com-gorilla-websocket
        go-golang-org-x-crypto
        go-google-golang-org-protobuf-proto
        go-google-golang-org-protobuf-encoding-protowire
        go-google-golang-org-protobuf-internal-encoding-messageset
        go-google-golang-org-protobuf-internal-errors
        go-google-golang-org-protobuf-internal-flags
        go-google-golang-org-protobuf-internal-genid
        go-google-golang-org-protobuf-internal-order
        go-google-golang-org-protobuf-internal-pragma
        go-google-golang-org-protobuf-internal-strs
        go-google-golang-org-protobuf-reflect-protoreflect
        go-google-golang-org-protobuf-reflect-protoregistry
        go-google-golang-org-protobuf-runtime-protoiface
        go-google-golang-org-protobuf-runtime-protoimpl
        go-google-golang-org-protobuf-internal-detrand
        go-google-golang-org-protobuf-internal-filedesc
        go-google-golang-org-protobuf-internal-filetype
        go-google-golang-org-protobuf-internal-impl
        go-google-golang-org-protobuf-internal-version
        go-google-golang-org-protobuf-encoding-prototext
        go-google-golang-org-protobuf-internal-descfmt
        go-google-golang-org-protobuf-internal-descopts
        go-google-golang-org-protobuf-internal-encoding-defval
        go-google-golang-org-protobuf-internal-encoding-tag
        go-google-golang-org-protobuf-internal-encoding-text
        go-google-golang-org-protobuf-internal-set))
      (home-page "https://github.com/tulir/libsignal-protocol-go")
      (synopsis "Go implementation of the Signal protocol for WhatsApp.")
      (description "Libsignal-protocol-go is a Go implementation of the Signal Client Protocol.")
      (license license:gpl3))))

(define-public go-mau-fi-libsignal-ecc
  (let ((pkg go-mau-fi-libsignal))
    (package
      (inherit pkg)
      (name "go-mau-fi-libsignal-ecc")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "go.mau.fi/libsignal/ecc"))))))

(define-public go-mau-fi-libsignal-groups
  (let ((pkg go-mau-fi-libsignal))
    (package
      (inherit pkg)
      (name "go-mau-fi-libsignal-groups")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "go.mau.fi/libsignal/groups"))))))

(define-public go-mau-fi-libsignal-cipher
  (let ((pkg go-mau-fi-libsignal))
    (package
      (inherit pkg)
      (name "go-mau-fi-libsignal-cipher")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "go.mau.fi/libsignal/cipher"))))))

(define-public go-mau-fi-libsignal-kdf
  (let ((pkg go-mau-fi-libsignal))
    (package
      (inherit pkg)
      (name "go-mau-fi-libsignal-kdf")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "go.mau.fi/libsignal/kdf"))))))

(define-public go-mau-fi-libsignal-keys-identity
  (let ((pkg go-mau-fi-libsignal))
    (package
      (inherit pkg)
      (name "go-mau-fi-libsignal-keys-identity")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "go.mau.fi/libsignal/keys/identity"))))))

(define-public go-mau-fi-libsignal-keys-prekey
  (let ((pkg go-mau-fi-libsignal))
    (package
      (inherit pkg)
      (name "go-mau-fi-libsignal-keys-prekey")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "go.mau.fi/libsignal/keys/prekey"))))))

(define-public go-mau-fi-libsignal-keys-chain
  (let ((pkg go-mau-fi-libsignal))
    (package
      (inherit pkg)
      (name "go-mau-fi-libsignal-keys-chain")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "go.mau.fi/libsignal/keys/chain"))))))

(define-public go-mau-fi-libsignal-keys-message
  (let ((pkg go-mau-fi-libsignal))
    (package
      (inherit pkg)
      (name "go-mau-fi-libsignal-keys-message")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "go.mau.fi/libsignal/keys/message"))))))

(define-public go-mau-fi-libsignal-keys-root
  (let ((pkg go-mau-fi-libsignal))
    (package
      (inherit pkg)
      (name "go-mau-fi-libsignal-keys-root")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "go.mau.fi/libsignal/keys/root"))))))

(define-public go-mau-fi-libsignal-keys-session
  (let ((pkg go-mau-fi-libsignal))
    (package
      (inherit pkg)
      (name "go-mau-fi-libsignal-keys-session")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "go.mau.fi/libsignal/keys/session"))))))

(define-public go-mau-fi-libsignal-logger
  (let ((pkg go-mau-fi-libsignal))
    (package
      (inherit pkg)
      (name "go-mau-fi-libsignal-logger")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "go.mau.fi/libsignal/logger"))))))

(define-public go-mau-fi-libsignal-ratchet
  (let ((pkg go-mau-fi-libsignal))
    (package
      (inherit pkg)
      (name "go-mau-fi-libsignal-ratchet")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "go.mau.fi/libsignal/ratchet"))))))

(define-public go-mau-fi-libsignal-serialize
  (let ((pkg go-mau-fi-libsignal))
    (package
      (inherit pkg)
      (name "go-mau-fi-libsignal-serialize")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "go.mau.fi/libsignal/serialize"))))))

(define-public go-mau-fi-libsignal-protocol
  (let ((pkg go-mau-fi-libsignal))
    (package
      (inherit pkg)
      (name "go-mau-fi-libsignal-protocol")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "go.mau.fi/libsignal/protocol"))))))

(define-public go-mau-fi-libsignal-session
  (let ((pkg go-mau-fi-libsignal))
    (package
      (inherit pkg)
      (name "go-mau-fi-libsignal-session")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "go.mau.fi/libsignal/session"))))))

(define-public go-mau-fi-libsignal-signalerror
  (let ((pkg go-mau-fi-libsignal))
    (package
      (inherit pkg)
      (name "go-mau-fi-libsignal-signalerror")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "go.mau.fi/libsignal/signalerror"))))))

(define-public go-mau-fi-libsignal-state-record
  (let ((pkg go-mau-fi-libsignal))
    (package
      (inherit pkg)
      (name "go-mau-fi-libsignal-state-record")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "go.mau.fi/libsignal/state/record"))))))

(define-public go-mau-fi-libsignal-state-store
  (let ((pkg go-mau-fi-libsignal))
    (package
      (inherit pkg)
      (name "go-mau-fi-libsignal-state-store")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "go.mau.fi/libsignal/state/store"))))))

(define-public go-mau-fi-libsignal-util-bytehelper
  (let ((pkg go-mau-fi-libsignal))
    (package
      (inherit pkg)
      (name "go-mau-fi-libsignal-util-bytehelper")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "go.mau.fi/libsignal/util/bytehelper"))))))

(define-public go-mau-fi-libsignal-util-keyhelper
  (let ((pkg go-mau-fi-libsignal))
    (package
      (inherit pkg)
      (name "go-mau-fi-libsignal-util-keyhelper")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "go.mau.fi/libsignal/util/keyhelper"))))))

(define-public go-mau-fi-libsignal-util-optional
  (let ((pkg go-mau-fi-libsignal))
    (package
      (inherit pkg)
      (name "go-mau-fi-libsignal-util-optional")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "go.mau.fi/libsignal/util/optional"))))))

(define-public go-mau-fi-libsignal-util-errorhelper
  (let ((pkg go-mau-fi-libsignal))
    (package
      (inherit pkg)
      (name "go-mau-fi-libsignal-util-errorhelper")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path import-path)
          "go.mau.fi/libsignal/util/errorhelper"))))))

(define-public go-mau-fi-libsignal-util-medium
  (let ((pkg go-mau-fi-libsignal))
    (package
      (inherit pkg)
      (name "go-mau-fi-libsignal-util-medium")
      (arguments
       (substitute-keyword-arguments (package-arguments pkg)
         ((#:import-path _)
          "go.mau.fi/libsignal/util/medium"))))))

(define-public go-mau-fi-whatsmeow
  (let ((commit "b64b2b2039d4a1461adac3490dade25e11a8e7c0")
        (revision "0"))
    (package
     (name "go-mau-fi-whatsmeow")
     (version (git-version "0" revision commit))
     (source
      (origin
        (method git-fetch)
        (uri
         (git-reference
          (url "https://github.com/tulir/whatsmeow")
          (commit commit)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0krmdkzi6ipkx0vvsyx10iszqg8iw1i68l16v6vgy7qpizyi5cm1"))))
     (build-system go-build-system)
     (arguments
      (list
       #:import-path "go.mau.fi/whatsmeow"))
     (propagated-inputs
      (list
       go-github-com-gorilla-websocket
       go-golang-org-x-crypto
       go-mau-fi-libsignal-ecc
       go-mau-fi-libsignal-groups
       go-mau-fi-libsignal-cipher
       go-mau-fi-libsignal-kdf
       go-mau-fi-libsignal-keys-identity
       go-mau-fi-libsignal-keys-prekey
       go-mau-fi-libsignal-keys-chain
       go-mau-fi-libsignal-keys-message
       go-mau-fi-libsignal-keys-root
       go-mau-fi-libsignal-keys-session
       go-mau-fi-libsignal-ratchet
       go-mau-fi-libsignal-logger
       go-mau-fi-libsignal-protocol
       go-mau-fi-libsignal-serialize
       go-mau-fi-libsignal-session
       go-mau-fi-libsignal-signalerror
       go-mau-fi-libsignal-state-record
       go-mau-fi-libsignal-state-store
       go-mau-fi-libsignal-util-bytehelper
       go-mau-fi-libsignal-util-keyhelper
       go-mau-fi-libsignal-util-optional
       go-mau-fi-libsignal-util-medium
       go-mau-fi-libsignal-util-errorhelper
       go-google-golang-org-protobuf-proto
       go-google-golang-org-protobuf-reflect-protoreflect
       go-google-golang-org-protobuf-encoding-protowire
       go-google-golang-org-protobuf-reflect-protoregistry
       go-google-golang-org-protobuf-internal-encoding-messageset
       go-google-golang-org-protobuf-internal-errors
       go-google-golang-org-protobuf-internal-flags
       go-google-golang-org-protobuf-internal-genid
       go-google-golang-org-protobuf-internal-order
       go-google-golang-org-protobuf-internal-pragma
       go-google-golang-org-protobuf-internal-strs
       go-google-golang-org-protobuf-runtime-protoiface
       go-google-golang-org-protobuf-runtime-protoimpl
       go-google-golang-org-protobuf-internal-detrand
       go-google-golang-org-protobuf-internal-filedesc
       go-google-golang-org-protobuf-internal-filetype
       go-google-golang-org-protobuf-internal-impl
       go-google-golang-org-protobuf-internal-version
       go-google-golang-org-protobuf-encoding-prototext
       go-google-golang-org-protobuf-internal-encoding-text
       go-google-golang-org-protobuf-internal-descfmt
       go-google-golang-org-protobuf-internal-descopts
       go-google-golang-org-protobuf-internal-set
       go-google-golang-org-protobuf-internal-encoding-defval
       go-google-golang-org-protobuf-internal-encoding-tag))
     (home-page "https://github.com/tulir/whatsmeow")
     (synopsis "Go library for the WhatsApp web multidevice API.")
     (description "@code{whatsmeow} is a Go library for the WhatsApp web multidevice API.")
     (license license:mpl2.0))))

(define-public go-maunium-net-go-mautrix
  (let ((commit "f8f9fe45cbe184d51b6977c345f09c59a848a998")
        (revision "0"))
    (package
      (name "go-maunium-net-go-mautrix")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/mautrix/go")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0zwxnq9cx7hcn7wmmgikb08w2f8qk9jkdzhafqxps1hlvcc8qp8i"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "maunium.net/go/mautrix"))
      (propagated-inputs
       (list
        go-golang-org-x-net
        go-golang-org-x-crypto
        go-github-com-stretchr-testify))
      (home-page "https://github.com/mautrix/go")
      (synopsis "A Golang Matrix framework")
      (description "A Golang Matrix framework. In addition to the basic client API
features the original project has, this framework also has:
@itemize
@item Appservice support (Intent API like mautrix-python, room state storage, etc)
@item End-to-end encryption support (incl. interactive SAS verification)
@item Structs for parsing event content
@item Helpers for parsing and generating Matrix HTML
@item Helpers for handling push rules
@end itemize")
      (license license:mpl2.0))))
