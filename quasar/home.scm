(define-module (quasar home)
  #:use-module (srfi srfi-1)
  #:export (%channel-root
            boolean->one-or-zero))

(define %channel-root
  (canonicalize-path
   (find (lambda (path)
           (file-exists? (string-append path "/.guix-channel")))
         %load-path)))

(define (boolean->one-or-zero bool)
  "Convert a boolean BOOL to 1 or 0."
  (if (eq? bool #t) 1 0))
