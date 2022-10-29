(define-module (conses serializers base)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:export (sexp-config?
            sexp-serialize
            serialize-sexp-config))

(define sexp-config? list?)

(define (sexp-serialize field-name val)
  (define (serialize-list-element elem)
    (cond
     ((gexp? elem) elem)
     (else
      #~(string-trim-right
         (with-output-to-string
           (lambda ()
             ((@ (ice-9 pretty-print) pretty-print)
              '#$elem #:max-expr-width 79)))
         #\newline))))

  #~(string-append
     #$@(interpose
         (map serialize-list-element val)
         "\n" 'suffix)))

(define serialize-sexp-config sexp-serialize)
