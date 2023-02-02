(define-module (conses system services databases)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages databases)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (postgresql-role
            postgresql-role-configuration
            postgresql-role-service-type))

(define-record-type* <postgresql-role>
  postgresql-role make-postgresql-role
  postgresql-role?
  (name             postgresql-role-name) ;string
  (password         postgresql-role-password
                    (default #f))
  (permissions      postgresql-role-permissions
                    (default '(createdb login))) ;list
  (create-database? postgresql-role-create-database?  ;boolean
                    (default #f))
  (encoding postgresql-role-encoding
            (default "UTF8"))
  (collation postgresql-role-collation
             (default "en_US.utf8"))
  (ctype postgresql-role-ctype
         (default "en_US.utf8")))

(define-record-type* <postgresql-role-configuration>
  postgresql-role-configuration make-postgresql-role-configuration
  postgresql-role-configuration?
  (host             postgresql-role-configuration-host ;string
                    (default "/var/run/postgresql"))
  (log              postgresql-role-configuration-log ;string
                    (default "/var/log/postgresql_roles.log"))
  (roles            postgresql-role-configuration-roles
                    (default '()))) ;list

(define (postgresql-create-roles config)
  ;; See: https://www.postgresql.org/docs/current/sql-createrole.html for the
  ;; complete permissions list.
  (define (format-permissions permissions)
    (let ((dict '(bypassrls createdb createrole login replication superuser)))
      (string-join (filter-map (lambda (permission)
                                 (and (member permission dict)
                                      (string-upcase
                                       (symbol->string permission))))
                               permissions)
                   " ")))

  (define (roles->queries roles)
    (apply mixed-text-file "queries"
           (append-map
            (lambda (role)
              (match-record role <postgresql-role>
                (name password permissions create-database? encoding collation ctype)
                `("SELECT NOT(EXISTS(SELECT 1 FROM pg_catalog.pg_roles WHERE \
rolname = '" ,name "')) as not_exists;\n"
"\\gset\n"
"\\if :not_exists\n"
"CREATE ROLE \"" ,name "\""
" WITH " ,(format-permissions permissions)
,@(if password
      `(" PASSWORD '" ,password "'")
      '())
";\n"
,@(if create-database?
      `("CREATE DATABASE \"" ,name "\""
        " OWNER \"" ,name "\"\n"
        " ENCODING '" ,encoding "'\n"
        " LC_COLLATE '" ,collation "'\n"
        " LC_CTYPE '" ,ctype "'\n"
        " TEMPLATE template0;")
      '())
"\\endif\n")))
            roles)))

  (let ((host (postgresql-role-configuration-host config))
        (roles (postgresql-role-configuration-roles config)))
    #~(let ((psql #$(file-append postgresql "/bin/psql")))
        (list psql "-a" "-h" #$host "-f" #$(roles->queries roles)))))

(define (postgresql-role-shepherd-service config)
  (match-record config <postgresql-role-configuration>
    (log)
    (list (shepherd-service
           (requirement '(postgres))
           (provision '(postgres-roles))
           (one-shot? #t)
           (start
            #~(lambda args
                (let ((pid (fork+exec-command
                            #$(postgresql-create-roles config)
                            #:user "postgres"
                            #:group "postgres"
                            #:log-file #$log)))
                  (zero? (cdr (waitpid pid))))))
           (documentation "Create PostgreSQL roles.")))))

(define postgresql-role-service-type
  (service-type (name 'postgresql-role)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          postgresql-role-shepherd-service)))
                (compose concatenate)
                (extend (lambda (config extended-roles)
                          (match-record config <postgresql-role-configuration>
                            (host roles)
                            (postgresql-role-configuration
                             (host host)
                             (roles (append roles extended-roles))))))
                (default-value (postgresql-role-configuration))
                (description "Ensure the specified PostgreSQL roles are
created after the PostgreSQL database is started.")))
