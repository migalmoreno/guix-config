(define-module (conses features databases)
  #:use-module (conses utils)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu packages databases)
  #:use-module (gnu services databases)
  #:use-module (srfi srfi-1)
  #:export (feature-postgresql))

(define-public (list-of-postgresql-roles? lst)
  (and (list? lst) (every postgresql-role? lst)))

(define-public (maybe-list-of-postgresql-roles? x)
  (or (list-of-postgresql-roles? x) (not x)))

(define* (feature-postgresql
          #:key
          (postgresql postgresql)
          (postgresql-roles #f))
  (ensure-pred any-package? postgresql)
  (ensure-pred maybe-list-of-postgresql-roles? postgresql-roles)

  (define f-name 'postgresql)

  (define (get-system-services config)
    "Return system services related to PostgreSQL."
    (append
     (list
      (service postgresql-service-type
               (postgresql-configuration
                (postgresql postgresql))))
     (if postgresql-roles
         (list
          (service postgresql-role-service-type
                   (postgresql-role-configuration
                    (roles postgresql-roles))))
         '())))

  (define (get-home-services config)
    "Return home services related to PostgreSQL."
    (if (get-value 'emacs config)
        (list
         (rde-elisp-configuration-service
          f-name
          config
          `((with-eval-after-load 'ob-core
              (setq org-babel-default-header-args:sql
                    '((:engine . "postgresql")))))))
        '()))

  (feature
   (name f-name)
   (values `((,f-name . ,postgresql)))
   (system-services-getter get-system-services)
   (home-services-getter get-home-services)))
