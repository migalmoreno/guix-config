(define-module (efimerspan packages python-xyz)
  #:use-module (guix packages)
  #:use-module (guix build-system)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system python)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages check)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gnupg)
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

(define-public python-oauth2ms
  (let ((commit "a1ef0cabfdea57e9309095954b90134604e21c08")
        (revision "0"))
    (package
      (name "python-oauth2ms")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/harishkrupo/oauth2ms")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "09fhhr6yhnrdp2jx14dryk8g9krn1dn13y1ycylc5qvvvrvfd5la"))))
      (propagated-inputs
       (list isync python-pyxdg python-gnupg python-msal))
      (build-system trivial-build-system)
      (synopsis "XOauth2 compatible O365 token fetcher.")
      (description "This tool can be used to fetch oauth2 tokens from the Microsoft
identity endpoint. Additionally, it can encode the token in the XOAUTH2 format to be
used as authentication in IMAP mail servers.")
      (license license:asl2.0)
      (home-page "https://github.com/harishkrupo/oauth2ms"))))

(define-public python-pmbootstrap
  (package
    (name "python-pmbootstrap")
    (version "1.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pmbootstrap" version))
       (sha256
        (base32 "1cxycx0wcgs5lhsfisdrvbvhd46i2baz0r7iixzngqv98h8lvzvg"))))
    (build-system python-build-system)
    (home-page "https://gitlab.com/postmarketOS/pmbootstrap")
    (synopsis
     "A sophisticated chroot / build / flash tool to develop and install postmarketOS.")
    (description
     "This package provides a sophisticated chroot / build / flash tool to develop and
install postmarketOS.")
    (license license:asl2.0)))

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
    (synopsis "binary diff and patch using the BSDIFF4-format")
    (description "This code is mostly derived from cx_bsdiff @url{http://cx-bsdiff.sourceforge.net/},
 which in turn was derived from bsdiff, the standalone utility produced for BSD which can be found at
@url{http://www.daemonology.net/bsdiff}. In addition to the two functions (diff and patch) cx_bsdiff
provides, this package includes an interface to the BSDIFF4-format, command line interfaces via bsdiff4
and bspatch4 and tests.")
    (license license:bsd-4)))

(define-public python-payload-dumper
  (let ((commit "60224410cbe9e937cc158d5eb376b56d8b40e12b")
        (revision "0"))
    (package
     (name "python-payload-dumper")
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
                      (let* ((python (string-append #$python-3 "/bin"))
                             (bin (string-append #$output "/bin"))
                             (target (string-append bin "/payload_dumper"))
                             (version (python-version #$python-3))
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
