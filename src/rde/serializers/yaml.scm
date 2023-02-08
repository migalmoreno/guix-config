(define-module (rde serializers yaml)
  #:use-module (gnu home services utils)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:export (yaml-print
            yaml-config?
            serialize-yaml-config))

(define (pairs? lst)
  (and (list? lst) (every pair? lst)))

(define (yaml-config? config)
  (list? config))

(define (yaml-s-boolean v)
  (list (if v "true" "false")))

(define (yaml-s-number v)
  (list (number->string v)))

(define (yaml-s-string v)
  (list (format #f "~s" v)))

(define (yaml-s-symbol v)
  (list (object->snake-case-string
         (string->symbol
          (format #f "~a" v)))))

(define (yaml-s-key k)
  (cond
    ((symbol? k) (yaml-s-symbol k))
    ((string? k) (yaml-s-string k))
    (else (throw 'yaml-invalid-key k))))

(define (yaml-s-identity v)
  (list v))

(define (yaml-s-newline pretty?)
  (if pretty? (list "\n") '()))

(define (yaml-s-space pretty?)
  (if pretty? (list " ") '()))

(define (yaml-s-indentation level pretty?)
  (if pretty?
      (list (format #f "~v_" (- (* 2 level) 2)))
      '()))

(define (yaml-s-vector v level pretty?)
  (append
   (yaml-s-newline pretty?)
   (vector-fold
    (lambda (i acc e)
      (append acc
              (if (> i 0)
                  (yaml-s-newline pretty?)
                  '())
              (yaml-s-indentation (1+ level) pretty?)
              (list "- ")
              (match e
                ((? pairs? e) (yaml-s-vector-alist e (+ 1 level) pretty?))
                (_ (yaml-s-yaml e (1+ level) pretty?)))))
    '() v)))

(define (yaml-s-list v pretty?)
  (append
   (list "[")
   (interpose
    (append-map
     (lambda (x)
       (yaml-s-yaml x 0 pretty?))
     v)
    ", ")
   (list "]")))

(define (yaml-s-pair v level pretty?)
  (append
   (yaml-s-indentation level pretty?)
   (yaml-s-key (car v))
   (list ":")
   (yaml-s-space pretty?)
   (if (pairs? (cdr v))
       (yaml-s-newline pretty?)
       (list ""))
   (yaml-s-yaml (cdr v) level pretty?)))

(define (yaml-s-alist v level pretty?)
  (append
   (yaml-s-pair (car v) (1+ level) pretty?)
   (append-map
    (lambda (x)
      (append
       (yaml-s-newline pretty?)
       (yaml-s-pair x (1+ level) pretty?)))
    (cdr v))))

(define (yaml-s-vector-alist v level pretty?)
  (append
   (yaml-s-pair (car v) (- level (- level 1)) pretty?)
   (append-map
    (lambda (x)
      (append
       (yaml-s-newline pretty?)
       (yaml-s-pair x (1+ level) pretty?)))
    (cdr v))))

(define (yaml-s-yaml yaml level pretty?)
  (append
   (match yaml
     (() (list ""))
     ((? boolean? v) (yaml-s-boolean v))
     ((? number? v) (yaml-s-number v))
     ((? string? v) (yaml-s-key v))
     ((? symbol? v) (yaml-s-key v))
     ((? gexp? v) (yaml-s-identity v))
     ((? file-like? v) (yaml-s-identity v))
     ((? vector? v) (yaml-s-vector v level pretty?))
     ((? pairs? v) (yaml-s-alist v level pretty?))
     ((? list? v) (yaml-s-list v pretty?))
     (e (throw 'yaml-invalid yaml)))))

(define* (yaml-serialize yaml #:key (pretty? #t))
  "Returns a list of YAML strings which have to be concatenated. It supports gexps,
file-likes, vectors -> arrays, alists -> dictionaries, etc."
  `(,@(yaml-s-yaml yaml 0 pretty?) "\n"))

(define serialize-yaml-config yaml-serialize)

(define* (yaml-print yaml #:key (pretty? #t))
  "Prints the generated YAML, useful for debugging purposes."
  (display (apply string-append
                  (yaml-s-yaml yaml 0 pretty?))))
