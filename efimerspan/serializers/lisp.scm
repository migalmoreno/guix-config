(define-module (efimerspan serializers lisp)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:export (lisp-config?
            lisp-serialize
            serialize-lisp-config))

(define lisp-config? list?)
(define (lisp-serialize field-name val)
  (define (serialize-list-element)
    (match-lambda
      ((? gexp? e) e)
      (e
       #~(string-trim-right
          (with-output-to-string
            (lambda ()
              ((@ (ice-9 pretty-print) pretty-print)
               '#$e #:max-expr-width 79)))
          #\newline))))

  #~(string-append
     #$@(interpose
         (map serialize-list-element val)
         "\n" 'suffix)))

(define serialize-lisp-config lisp-serialize)
