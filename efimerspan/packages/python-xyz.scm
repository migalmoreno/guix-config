(define-module (efimerspan packages python-xyz)
  #:use-module (guix packages)
  #:use-module (guix build-system)
  #:use-module (guix build-system python)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages matrix)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages check)
  #:use-module (gnu packages freedesktop)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:))

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
    (propagated-inputs (list python-pillow))
    (inputs
     (list libx11 libpng libxcursor))
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

(define-public python-bsdiff4
  (package
    (name "python-bsdiff4")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bsdiff4" version))
       (sha256
        (base32 "0jsxf2var03gfxjkwv6ia2pw0y92ml9b4r0mzl1xmzgg49szrkw7"))))
    (build-system python-build-system)
    (home-page "https://github.com/ilanschnell/bsdiff4")
    (synopsis "Binary diff and patch using the BSDIFF4-format")
    (description "This code is mostly derived from cx_bsdiff @url{http://cx-bsdiff.sourceforge.net/},
 which in turn was derived from bsdiff, the standalone utility produced for BSD which can be found at
@url{http://www.daemonology.net/bsdiff}. In addition to the two functions (diff and patch) cx_bsdiff
provides, this package includes an interface to the BSDIFF4-format, command line interfaces via bsdiff4
and bspatch4 and tests.")
    (license license:bsd-4)))

(define-public python-matrix-common
  (package
    (name"python-matrix-common")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "matrix_common" version))
       (sha256
        (base32 "1bgdhzvqs51z079zjszhd5xqb100mbr5w8gpxs9z31r5xmi5nw7a"))))
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
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-treq)
       (replace "python-requests" python-requests-next)
       (replace "python-service-identity" python-service-identity-next)))))

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
