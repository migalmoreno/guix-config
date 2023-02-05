(define-module (conses packages python-xyz)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages image)
  #:use-module (gnu packages check)
  #:use-module (gnu packages matrix)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:))

(define-public python-incremental-next
  (package
    (inherit python-incremental)
    (name "python-incremental-next")
    (version "21.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "incremental" version))
       (sha256
        (base32
         "0mvgbmsnv1c8ziydw41jjkivc0zcqyzli7frcpvbkxj8zxddxx82"))))))

(define-public python-twisted-next
  (package
    (inherit python-twisted)
    (name "python-twisted-next")
    (version "22.10.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Twisted" version ".tar.gz"))
              (sha256
               (base32
                "0cad5n7h8nd0nyl865sn8l4ja0mkwbx9n41cnkklcpsgm50bvb1j"))))
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'adjust-for-older-attrs
            (lambda _
              (substitute* (find-files "." "\\.py$")
                (("from attrs\\b")
                 "from attr")))))))
    (native-inputs
     (list
      python-wheel))
    (inputs
     (list (list python "tk")))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-twisted)
       (append python-typing-extensions python-bcrypt
               python-attrs
               python-cryptography-next python-pyasn1)
       (replace "python-incremental" python-incremental-next)))))

(define-public python-clickgen
  (package
    (name "python-clickgen")
    (version "1.1.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "clickgen" version))
       (sha256
        (base32 "17abhldc30ya7kag7rpb1399wlajcavmpssl34wf8icaq3iblrr5"))))
    (build-system python-build-system)
    (propagated-inputs
     (list
      python-pillow
      python-toml))
    (inputs
     (list libx11
           libpng
           libxcursor))
    (native-inputs
     (list
      python-setuptools
      python-wheel))
    (home-page "https://github.com/ful1e5/clickgen")
    (synopsis "The hassle-free cursor building toolbox.")
    (description "clickgen is an API for building X11 and Windows cursors from .png files.
clickgen is using @code{anicursorgen} and @code{xcursorgen} under the hood.")
    (license license:expat)))

(define-public python-msal
  (package
    (name "python-msal")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "msal" version))
       (sha256
        (base32 "114h741mvd2vw3wnclg81cpd1l0j8hrvi38plgb7z8j6p96v03r4"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-cryptography python-mock python-pyjwt python-requests))
    (home-page
     "https://github.com/AzureAD/microsoft-authentication-library-for-python")
    (synopsis "Microsoft Authentication Library for Python.")
    (description
     "The Microsoft Authentication Library (MSAL) for Python library enables your app
to access the Microsoft Cloud by supporting authentication of users with
Microsoft Azure Active Directory accounts (AAD) and Microsoft Accounts (MSA)
using industry standard OAuth2 and OpenID Connect.")
    (license license:expat)))

(define-public python-matrix-common
  (package
    (name"python-matrix-common")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "matrix_common" version))
       (sha256
        (base32 "0lrqzb6s57fxp0kwffdqnkr2pj9aia459cv1b95b55dxlq1cz7d9"))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'build
                 (lambda _
                   (setenv "SOURCE_DATE_EPOCH"
                           (number->string (* 10 366 24 60 60)))
                   (invoke "python" "-m" "build" "--wheel" "--no-isolation" ".")))
               (replace 'install
                 (lambda _
                   (let ((whl (car (find-files "dist" "\\.whl$"))))
                     (invoke "pip" "--no-cache-dir" "--no-input"
                             "install" "--no-deps" "--prefix" #$output whl))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "pytest" "-vv" "tests")))))))
    (native-inputs
     (list
      python-pypa-build
      python-setuptools-scm
      python-pytest
      python-wheel))
    (home-page "https://github.com/matrix-org/matrix-python-common")
    (synopsis "Common code for Synapse, Sydent and Sygnal.")
    (description #f)
    (license license:asl2.0)))

(define-public python-pyopenssl-next
  (package
    (inherit python-pyopenssl)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-pyopenssl)
       (replace "python-cryptography" python-cryptography-next)))))

(define-public python-service-identity-next
  (package
    (inherit python-service-identity)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-service-identity)
       (replace "python-pyopenssl" python-pyopenssl-next)))))

(define-public python-urllib3-next
  (package
    (inherit python-urllib3)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-urllib3)
       (replace "python-cryptography" python-cryptography-next)
       (replace "python-pyopenssl" python-pyopenssl-next)))))

(define-public python-txsni-next
  (package
    (inherit python-txsni)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-txsni)
       (replace "python-service-identity" python-service-identity-next)
       (replace "python-pyopenssl" python-pyopenssl-next)))))

(define-public python-josepy-next
  (package
    (inherit python-josepy)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-josepy)
       (replace "python-cryptography" python-cryptography-next)
       (replace "python-pyopenssl" python-pyopenssl-next)))))

(define-public python-acme-next
  (package
    (inherit python-acme)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-acme)
       (replace "python-josepy" python-josepy-next)
       (replace "python-requests" python-requests-next)
       (replace "python-requests-toolbelt" python-requests-toolbelt-next)
       (replace "python-pyopenssl" python-pyopenssl-next)
       (replace "python-cryptography" python-cryptography-next)))))

(define-public python-requests-toolbelt-next
  (package
    (inherit python-requests-toolbelt)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-requests-toolbelt)
       (replace "python-requests" python-requests-next)))))

(define-public python-txacme-next
  (package
    (inherit python-txacme)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-txacme)
       (replace "python-josepy" python-josepy-next)
       (replace "python-acme" python-acme-next)
       (replace "python-txsni" python-txsni-next)
       (replace "python-treq" python-treq-next)
       (replace "python-acme" python-acme-next)))))

(define-public python-requests-next
  (package
    (inherit python-requests)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-requests)
       (replace "python-urllib3" python-urllib3-next)))))

(define-public python-treq-next
  (package
    (inherit python-treq)
    (name "python-treq-next")
    (version "20.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "treq" version))
       (sha256
        (base32
         "18kdk11d84lyxj6dz183nblc6c6r4rj1hk0lpsyiykzgbakjrkc3"))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-treq)
       (replace "python-requests" python-requests-next)
       (replace "python-twisted" python-twisted-next)
       (replace "python-service-identity" python-service-identity-next)
       (replace "python-incremental" python-incremental-next)))))

(define-public python-matrix-synapse-ldap3-next
  (package
    (inherit python-matrix-synapse-ldap3)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-matrix-synapse-ldap3)
       (replace "python-service-identity" python-service-identity-next)))))

(define-public python-pysaml2-next
  (package
    (inherit python-pysaml2)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-pysaml2)
       (replace "python-cryptography" python-cryptography-next)
       (replace "python-pyopenssl" python-pyopenssl-next)
       (replace "python-requests" python-requests-next)))))

(define-public python-werkzeug-next
  (package
    (inherit python-werkzeug)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-werkzeug)
       (replace "python-requests" python-requests-next)))))

(define-public python-flask-next
  (package
    (inherit python-flask)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-flask)
       (replace "python-werkzeug" python-werkzeug-next)))))

(define-public python-flask-session-next
  (package
    (inherit python-flask-session)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-flask-session)
       (replace "python-flask" python-flask-next)))))

(define-public python-secretstorage-next
  (package
    (inherit python-secretstorage)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-secretstorage)
       (replace "python-cryptography" python-cryptography-next)))))

(define-public python-keyring-next
  (package
    (inherit python-keyring)
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-keyring)
       (replace "python-secretstorage" python-secretstorage-next)))))