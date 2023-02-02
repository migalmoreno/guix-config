(define-module (conses utils)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1))

(define-public (maybe-procedure? x)
  (or (procedure? x) (not x)))

(define-public (make-feature-list . features)
  (append-map (lambda (f)
                (if (list? f) f (list f)))
              features))

(define-public %project-root
  (canonicalize-path
   (find (lambda (path)
           (every file-exists? (map (lambda (file)
                                      (string-append path "/" file))
                                    (list ".dir-locals.el"))))
         %load-path)))

(define-public (project-file subpath)
  (local-file (string-append %project-root "/" subpath)))
