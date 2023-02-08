(define-module (rde features web)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (rde packages web)
  #:use-module (rde system services web)
  #:use-module (gnu services)
  #:use-module (gnu services web)
  #:use-module (gnu services certbot)
  #:use-module (gnu packages web)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (guix gexp)
  #:export (feature-web-settings
            feature-nginx
            feature-certbot
            feature-whoogle
            feature-alternative-frontends))

(define-public %nginx-deploy-hook
  (program-file
   "nginx-deploy-hook"
   #~(let ((pid (call-with-input-file "/var/run/nginx/pid" read)))
       (kill pid SIGHUP))))

(define-public %letsencrypt-acme-challenge
  (nginx-location-configuration
   (uri "/.well-known")
   (body '("root /srv/http;"))))

(define* (feature-web-settings
          #:key domain)
  (ensure-pred string? domain)

  (feature
   (name 'web-settings)
   (values `((web-settings . #t)
             (domain . ,domain)))))

(define* (feature-nginx
          #:key
          (nginx nginx)
          (webdav? #f))
  "Configure nginx, an advanced load balancer, web server, and
reverse proxy."
  (ensure-pred any-package? nginx)
  (ensure-pred boolean? webdav?)

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
    (require-value 'domain config)
    (define domain (get-value 'domain config))
    (define letsencrypt-dir (and domain (string-append "/etc/letsencrypt/live/" domain)))

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
          (ssl-certificate (string-append letsencrypt-dir "/fullchain.pem"))
          (ssl-certificate-key (string-append letsencrypt-dir "/privkey.pem"))
          (locations
           (append
            (list
             (nginx-location-configuration
              (uri "/.well-known/matrix/server")
              (body
               (list "default_type application/json;"
                     (format #f "return 200 '{\"m.server\": \"matrix.~a:443\"}';" domain)
                     "add_header Access-Control-Allow-Origin *;"))))
            (list %letsencrypt-acme-challenge)
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
                '()))))))))))

  (feature
   (name f-name)
   (values `((,f-name . ,nginx)))
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
    (require-value 'domain config)
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
                  (domains (list domain (string-append "www." domain)))
                  (deploy-hook %nginx-deploy-hook))))))))

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
    (define letsencrypt-dir (and domain (string-append "/etc/letsencrypt/live/whoogle." domain)))

    (append
     (list
      (service whoogle-service-type
               (whoogle-configuration
                (whoogle whoogle-search))))
     (if (get-value 'nginx config)
         (list
          (simple-service
           'add-whoogle-nginx-configuration
           nginx-service-type
           (list
            (nginx-server-configuration
             (listen '("443 ssl http2"))
             (server-name (list (string-append "whoogle." domain)))
             (ssl-certificate (string-append letsencrypt-dir "/fullchain.pem"))
             (ssl-certificate-key (string-append letsencrypt-dir "/privkey.pem"))
             (locations
              (list
               (nginx-location-configuration
                (uri "/")
                (body
                 (list "proxy_pass http://localhost:5000/;"
                       "proxy_set_header X-Forwarded-For $remote_addr;"
                       "proxy_set_header HOST $http_host;")))
               %letsencrypt-acme-challenge))))))
         '())
     (if (get-value 'certbot config)
         (list
          (simple-service
           'add-whoogle-ssl
           certbot-service-type
           (list
            (certificate-configuration
             (domains (list (string-append "whoogle." domain)))
             (deploy-hook %nginx-deploy-hook)))))
         '())))

  (feature
   (name 'whoogle)
   (values `((whoogle . ,whoogle-search)))
   (system-services-getter get-system-services)))

(define* (feature-alternative-frontends
          #:key
          (youtube-frontend "https://invidious.snopyta.org")
          (reddit-frontend "https://teddit.net")
          (instagram-frontend "https://picuki.com")
          (quora-frontend "https://quora.vern.cc")
          (google-frontend "https://search.sethforprivacy.com")
          (imgur-frontend "https://i.bcow.xyz")
          (medium-frontend "https://scribe.rip")
          (twitter-frontend "https://nitter.namazso.eu")
          (tiktok-frontend "https://tok.artemislena.eu")
          (fandom-frontend "https://bw.vern.cc"))
  (ensure-pred maybe-string? youtube-frontend)
  (ensure-pred maybe-string? reddit-frontend)
  (ensure-pred maybe-string? instagram-frontend)
  (ensure-pred maybe-string? quora-frontend)
  (ensure-pred maybe-string? google-frontend)
  (ensure-pred maybe-string? imgur-frontend)
  (ensure-pred maybe-string? medium-frontend)
  (ensure-pred maybe-string? twitter-frontend)
  (ensure-pred maybe-string? tiktok-frontend)
  (ensure-pred maybe-string? fandom-frontend)

  (feature
   (name 'alternative-frontends)
   (values (append
            `((alternative-frontends . #t))
            (make-feature-values
             youtube-frontend reddit-frontend instagram-frontend quora-frontend google-frontend
             imgur-frontend medium-frontend twitter-frontend tiktok-frontend fandom-frontend)))))
