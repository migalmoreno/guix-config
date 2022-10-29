(define-module (conses packages web)
  #:use-module (conses packages python-xyz)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages web)
  #:use-module (gnu packages time)
  #:use-module (gnu packages check)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
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
    (version "0.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "whoogle-search" version))
       (sha256
        (base32 "04bvxwsjqqjn2w3q6zgsh8an45jqx077pdkkvq2y3lq5w56rxkc7"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'check))))
    (propagated-inputs
     (list
      python-attrs
      python-beautifulsoup4
      python-cachelib
      python-certifi
      python-cffi
      python-chardet
      python-click
      python-cryptography-next
      python-cssutils
      python-defusedxml
      python-flask-next
      python-flask-session-next
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
      python-pyopenssl-next
      python-pyparsing
      python-pysocks
      python-pytest
      python-dateutil
      python-requests-next
      python-soupsieve
      python-stem
      python-urllib3-next
      python-waitress
      python-wcwidth
      python-werkzeug-next
      python-dotenv))
    (home-page "https://github.com/benbusby/whoogle-search")
    (synopsis "A self-hosted, ad-free, privacy-respecting metasearch engine.")
    (description "Get Google results, but without any ads, javascript, AMP links,
cookies, or IP address tracking.")
    (license license:expat)))
