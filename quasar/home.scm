(define-module (quasar home)
  #:use-module (srfi srfi-28)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (%project-root
            boolean->one-or-zero
            password-store-get))

(define %project-root
  (string-append (passwd:dir (getpwnam (getenv "USER"))) "/src/guixrc"))

(define (boolean->one-or-zero bool)
  "Convert a boolean BOOL to 1 or 0."
  (if (eq? bool #t) 1 0))

(define (password-store-get entry)
  "Returns password for ENTRY."
  (let* ((port (open-input-pipe (format "pass ~a" entry)))
         (str (read-line port)))
    (close-pipe port)
    str))
