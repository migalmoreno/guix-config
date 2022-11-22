(define-module (conses utils)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (project-file))

(define-public (maybe-symbol? x)
  (or (symbol? x) (not x)))

(define-public (maybe-procedure? x)
  (or (procedure? x) (not x)))

(define-public (maybe-number? x)
  (or (number? x) (not x)))

(define-public (number-or-list? x)
  (or (number? x) (list? x)))

(define-public (maybe-pair-or-string? x)
  (or (pair? x) (string? x) (not x)))

(define-public (maybe-list-of-file-likes? x)
  (or (and (list? x) (every file-like? x)) (not x)))

(define-public %project-root
  (canonicalize-path
   (find (lambda (path)
           (every file-exists? (map (lambda (file)
                                      (string-append path "/" file))
                                    (list ".dir-locals.el"
                                          "conses"))))
         %load-path)))

(define (project-file subpath)
  (local-file (string-append %project-root "/" subpath)))
