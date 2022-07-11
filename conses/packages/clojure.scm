(define-module (conses packages clojure)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (gnu packages clojure))

(define-public clojure-tools-next
  (package
    (inherit clojure-tools)
    (name "clojure-tools-next")
    (version "1.11.1.1149")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.clojure.org/install/clojure-tools-"
                           version
                           ".tar.gz"))
       (sha256
        (base32 "196wl87br8zg3npfwpr5c1q0knxd1810vzgb0b6h195hyjf6i210"))
       (snippet
        `(delete-file ,(string-append "clojure-tools-" version ".jar")))))
        (arguments
         (substitute-keyword-arguments (package-arguments clojure-tools)
           ((#:install-plan plan)
            `(cons* '("tools.edn" "lib/clojure/")
                    ,plan))))))
