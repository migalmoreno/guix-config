(define-module (conses features web)
  #:use-module (conses utils)
  #:use-module (conses packages web)
  #:use-module (conses system services web)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu services web)
  #:use-module (gnu packages web)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:export (feature-nginx
            feature-certbot
            feature-whoogle))

(define* (feature-nginx
          #:key
          (nginx nginx)
          (webdav? #f)
          domain)
  "Configure nginx, an advanced load balancer, web server, and
reverse proxy."
  (ensure-pred any-package? nginx)
  (ensure-pred boolean? webdav?)
  (ensure-pred string? domain)

  (define f-name 'nginx)

  (define (get-home-services config)
    "Return home services related to nginx."
    (list
     (rde-elisp-configuration-service
      f-name
      config
      `((add-to-list 'auto-mode-alist
                     '("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)))
      #:elisp-packages (list emacs-nginx-mode))))

  (define (get-system-services config)
    "Return system services related to nginx."
    (define domain (get-value 'domain config))
    (define letsencrypt-dir "/etc/letsencrypt/live")

    (list
     (service
      nginx-service-type
      (nginx-configuration
       (nginx nginx)
       (server-blocks
        (list
         (nginx-server-configuration
          (listen '("443 ssl http2"))
          (server-name (list domain (string-append "www." domain)))
          (root (string-append "/srv/http/" domain))
          (ssl-certificate (string-append letsencrypt-dir "/" domain "/fullchain.pem"))
          (ssl-certificate-key (string-append letsencrypt-dir "/" domain "/privkey.pem"))
          (locations
           (append
            (list
             (nginx-location-configuration
              (uri "/.well-known")
              (body '("root /srv/http;"))))
            (if webdav?
                (list
                 (nginx-location-configuration
                  (uri "/webdav")
                  (body
                   (list "root /srv/http/dav;"
                         "client_body_temp_path /srv/client_temp;"
                         "dav_methods PUT DELETE MKCOL COPY MOVE;"
                         "create_full_put_path on;"
                         "dav_access group:rw all:r;"))))
                '())
            (if (get-value 'synapse config)
                (list
                 (nginx-location-configuration
                  (uri "/.well-known/matrix/server")
                  (body
                   (list "default_type application/json;"
                         (format #f "return 200 '{\"m.server\": \"matrix.~a:443\"}';" domain)
                         "add_header Access-Control-Allow-Origin *;"))))
                '()))))))))))

  (feature
   (name f-name)
   (values `((,f-name . ,nginx)
             (domain . ,domain)))
   (home-services-getter get-home-services)
   (system-services-getter get-system-services)))

(define* (feature-certbot
          #:key
          (certbot certbot)
          email)
  "Configure Certbot, EFF's ACME client."
  (ensure-pred any-package? certbot)

  (define (get-system-services config)
    "Return system services related to Certbot."
    (define domain (get-value 'domain config))

    (list
     (service certbot-service-type
              (certbot-configuration
               (package certbot)
               (email email)
               (webroot "/srv/http")
               (certificates
                (list
                 (certificate-configuration
                  (name domain)
                  (domains `(,domain
                             ,(string-append "wwww." domain)
                             (when (get-value 'whoogle config)
                               (string-append "whoogle." domain))
                             (when (get-value 'synapse config)
                               (let ((homeserver (get-value 'matrix-homeserver config)))
                                 (string-drop
                                  homeserver
                                  (+ 1 (string-index-right homeserver #\/)))))))
                  (deploy-hook
                   (program-file
                    "nginx-deploy-hook"
                    #~(let ((pid (call-with-input-file "/var/run/nginx/pid" read)))
                        (kill pid SIGHUP)))))))))))

  (feature
   (name 'certbot)
   (values `((certbot . ,certbot)))
   (system-services-getter get-system-services)))

(define* (feature-whoogle
          #:key
          (whoogle-search whoogle-search))
  "Configure whoogle-search, a self-hosted, free, ad-free, and
privacy respecting metaseach engine."
  (ensure-pred file-like? whoogle-search)

  (define (get-system-services config)
    "Return system services related to Whoogle."
    (define domain (get-value 'domain config))
    (define letsencrypt-dir "/etc/letsencrypt/live")

    (list
     (service whoogle-service-type
              (whoogle-configuration
                  (whoogle whoogle-search)))
     (simple-service
      'whoogle-nginx-service
      nginx-service-type
      (if (get-value 'nginx config)
        (list
         (nginx-server-configuration
          (listen '("443 ssl http2"))
          (server-name (list (string-append "whoogle." domain)))
          (ssl-certificate (string-append letsencrypt-dir "/" domain "/fullchain.pem"))
          (ssl-certificate-key (string-append letsencrypt-dir "/" domain "/privkey.pem"))
          (locations
           (list
            (nginx-location-configuration
             (uri "/")
             (body
              (list "proxy_pass http://localhost:5000/;"
                    "proxy_set_header X-Forwarded-For $remote_addr;"
                    "proxy_set_header HOST $http_host;")))
            (nginx-location-configuration
             (uri "/.well-known")
             (body '("root /srv/http;")))))))
        '()))))

  (feature
   (name 'whoogle)
   (values `((whoogle . ,whoogle-search)))
   (system-services-getter get-system-services)))
