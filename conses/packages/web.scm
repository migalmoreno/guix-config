(define-module (conses packages web)
  #:use-module (conses packages python-xyz)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages web)
  #:use-module (gnu packages time)
  #:use-module (gnu packages check)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:))

(define-public nginx-with-dav
  (package
    (inherit nginx)
    (name "nginx-with-dav")
    (arguments
     (substitute-keyword-arguments (package-arguments nginx)
         ((#:configure-flags configure-flags)
          #~(cons* "--with-http_dav_module"
                   #$configure-flags))))))

(define-public whoogle-search
  (package
    (name "whoogle-search")
    (version "0.8.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "whoogle-search" version))
              (sha256
               (base32
                "0h8cl9bkd3vx17kbvcnmc8cy6pc29lxr0drxm84kj37ka788cj2g"))))
    (build-system python-build-system)
    (arguments
     (list #:tests? #f)) ;needs network
    (propagated-inputs (list python-attrs
                             python-beautifulsoup4
                             python-cachelib
                             python-certifi
                             python-cffi
                             python-brotli
                             python-chardet
                             python-click
                             python-cryptography
                             python-cssutils
                             python-defusedxml
                             python-flask
                             python-flask-session
                             python-idna
                             python-itsdangerous
                             python-jinja2
                             python-markupsafe
                             python-more-itertools
                             python-packaging
                             python-pluggy
                             python-py
                             python-pycodestyle
                             python-pycparser
                             python-pyopenssl
                             python-pyparsing
                             python-pysocks
                             python-dateutil
                             python-requests
                             python-soupsieve
                             python-stem
                             python-urllib3
                             python-waitress
                             python-wcwidth
                             python-werkzeug
                             python-dotenv))
    (home-page "https://github.com/benbusby/whoogle-search")
    (synopsis "Self-hosted, ad-free, privacy-respecting metasearch engine")
    (description
     "Get Google results, but without any ads, javascript, AMP links,
cookies, or IP address tracking.  Quick and simple to implement as a primary search
engine replacement on both desktop and mobile.")
    (license license:expat)))
