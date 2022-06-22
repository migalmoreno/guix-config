(define-module (efimerspan packages matrix)
  #:use-module (efimerspan packages python-xyz)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (guix build utils)
  #:use-module (guix build python-build-system)
  #:use-module (guix download)
  #:use-module (gnu packages matrix)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-crypto))

(define-public synapse-next
  (package
    (inherit synapse)
    (name "synapse-next")
    (version "1.61.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "matrix-synapse" version))
       (sha256
        (base32 "0gzrqmjnr0vsyigviw9a1vrywqhlax6bf66r5vbfwhgf3rnjyiml"))))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'loosen-requirements
           (lambda _
             (substitute* "setup.py"
               (("matrix-common>=1.1.0,<1.2.0")
                "matrix_common")))))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs synapse)
       (append python-matrix-common
                python-ijson
                python-cryptography-next)
       (replace "python-pyopenssl" python-pyopenssl-next)
       (replace "python-service-identity" python-service-identity-next)
       (replace "python-txacme" python-txacme-next)
       (replace "python-treq" python-treq-next)
       (replace "python-matrix-synapse-ldap3" python-matrix-synapse-ldap3-next)
       (replace "python-urllib3" python-urllib3-next)
       (replace "python-requests" python-requests-next)
       (replace "python-pysaml2" python-pysaml2-next)))))
