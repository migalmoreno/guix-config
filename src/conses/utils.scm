(define-module (conses utils)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1))

(define-public (make-feature-list . features)
  (append-map (lambda (f)
                (if (list? f) f (list f)))
              features))

(define-public %project-root
  (canonicalize-path
   (dirname
    (find (lambda (path)
            (every file-exists? (map (lambda (file)
                                       (string-append (dirname path) "/" file))
                                     (list "src/conses"))))
          %load-path))))

(define-public (project-file subpath)
  (local-file (string-append %project-root "/" subpath)))
