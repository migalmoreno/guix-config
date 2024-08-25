(define-module (migalmoreno hosts cygnus)
  #:use-module (migalmoreno hosts lyra)
  #:use-module (migalmoreno utils)
  #:use-module (rde features base)
  #:use-module (rde features databases)
  #:use-module (rde features docker)
  #:use-module (rde features system)
  #:use-module (rde gexp)
  #:use-module (rde system services matrix)
  #:use-module (rde packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages version-control)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services certbot)
  #:use-module (gnu services cgit)
  #:use-module (gnu services docker)
  #:use-module (gnu services networking)
  #:use-module (gnu services rsync)
  #:use-module (gnu services ssh)
  #:use-module (gnu services version-control)
  #:use-module (gnu services web)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (guix build utils)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-26)
  #:use-module (sxml simple))

(define %file-systems
  (list
   (file-system
     (mount-point "/")
     (device (file-system-label "root"))
     (type "ext4"))))

(define %swap-devices
  (list
   (swap-space
    (target (file-system-label "swap")))))

(define %extra-packages
  (strings->packages "git" "rsync" "docker-compose"))

(define %nginx-robots-txt
  (nginx-location-configuration
   (uri "/robots.txt")
   (body
    (list "return 200 \"User-agent: *\\nDisallow: /\\n\";"))))

(define %nginx-crawlers-block
  "if ($http_user_agent ~* (Bytespider|Amazonbot|MJ12bot|DotBot|FriendlyCrawler)) { return 403; }")

(define %blog-services
  (list
   (service nginx-service-type
            (nginx-configuration
             (server-blocks
              (list
               (nginx-server-configuration
                (listen '("443 ssl http2"))
                (server-name
                 (list %default-domain
                       (string-append "www." %default-domain)))
                (root (string-append "/srv/http/" %default-domain))
                (ssl-certificate
                 (format #f "/etc/letsencrypt/live/~a/fullchain.pem"
                         %default-domain))
                (ssl-certificate-key
                 (format #f "/etc/letsencrypt/live/~a/privkey.pem"
                         %default-domain))
                (raw-content
                 (list
                  "error_page 404 = /404.html;"
                  %nginx-crawlers-block))
                (locations
                 (list
                  (nginx-location-configuration
                   (uri "/404.html")
                   (body
                    (list "internal;")))
                  %nginx-robots-txt
                  %letsencrypt-acme-challenge)))))
             (extra-content
              (string-append
               "limit_req_zone $binary_remote_addr zone=ip:20m rate=10r/s;\n"
               "limit_req_status 429;\n"))))
   (simple-service
    'add-blog-ssl-certificate
    certbot-service-type
    (list
     (certificate-configuration
      (domains (list %default-domain
                     (string-append "www." %default-domain)))
      (deploy-hook %nginx-deploy-hook))))))

(define %tubo-services
  (list
   (service oci-container-service-type
            (list
             (oci-container-configuration
              (image (string-append %default-username "/tubo"))
              (network "host")
              (ports
               '(("3000" . "3000"))))))
   (simple-service
    'add-tubo-nginx-configuration
    nginx-service-type
    (list
     (nginx-server-configuration
      (listen '("443 ssl http2"))
      (server-name (list "tubo.media"))
      (ssl-certificate
       "/etc/letsencrypt/live/tubo.media/fullchain.pem")
      (ssl-certificate-key
       "/etc/letsencrypt/live/tubo.media/privkey.pem")
      (raw-content (list %nginx-crawlers-block))
      (locations
       (list
        (nginx-location-configuration
         (uri "/")
         (body
          (list
           "limit_req zone=ip burst=20 nodelay;"
           "proxy_pass http://localhost:3000;"
           "proxy_set_header X-Forwarded-For $remote_addr;"
           "proxy_set_header HOST $http_host;")))
        %nginx-robots-txt)))
     (nginx-server-configuration
      (listen '("443 ssl http2"))
      (server-name (list (string-append "tubo." %default-domain)))
      (ssl-certificate
       (format #f "/etc/letsencrypt/live/tubo.~a/fullchain.pem"
               %default-domain))
      (ssl-certificate-key
       (format #f "/etc/letsencrypt/live/tubo.~a/privkey.pem"
               %default-domain))
      (raw-content (list %nginx-crawlers-block))
      (locations
       (list
        (nginx-location-configuration
         (uri "/")
         (body
          (list
           "return 301 https://tubo.media$request_uri;")))
        %nginx-robots-txt)))))
   (simple-service
    'add-tubo-ssl-certificate
    certbot-service-type
    (list
     (certificate-configuration
      (domains (list "tubo.media"))
      (deploy-hook %nginx-deploy-hook))))))

(define %matrix-services
  (list
   (service synapse-service-type
            (synapse-configuration
             (server-name "conses.eu")
             (enable-registration? #f)
             (public-base-url "https://matrix.conses.eu")
             (shared-secret (getenv "CYGNUS_SYNAPSE_SHARED_SECRET"))
             (postgresql-db? #t)
             (postgresql-db-password (getenv "CYGNUS_SYNAPSE_DB_PASSWORD"))))
   (service mautrix-whatsapp-service-type
            (mautrix-whatsapp-configuration
             (domain "conses.eu")
             (postgresql-db? #t)
             (postgresql-db-password
              (getenv "CYGNUS_MAUTRIX_WHATSAPP_DB_PASSWORD"))
             (encryption? #t)
             (permissions `(("conses.eu" . user)
                            ("@admin:conses.eu" . admin)))))
   (simple-service
    'add-matrix-nginx-configuration
    nginx-service-type
    (list
     (nginx-server-configuration
      (listen '("443 ssl http2"))
      (server-name (list "conses.eu" "www.conses.eu"))
      (ssl-certificate "/etc/letsencrypt/live/conses.eu/fullchain.pem")
      (ssl-certificate-key "/etc/letsencrypt/live/conses.eu/privkey.pem")
      (raw-content (list %nginx-crawlers-block))
      (locations
       (list
        (nginx-location-configuration
         (uri "/.well-known/matrix/server")
         (body
          (list "default_type application/json;"
                "return 200 '{\"m.server\": \"matrix.conses.eu:443\"}';"
                "add_header Access-Control-Allow-Origin *;")))
        %nginx-robots-txt)))
     (nginx-server-configuration
      (listen '("443 ssl http2"
                "[::]:443 ssl http2"
                "8448 ssl http2 default_server"
                "[::]:8448 ssl http2 default_server"))
      (server-name (list "matrix.conses.eu"))
      (ssl-certificate
       "/etc/letsencrypt/live/matrix.conses.eu/fullchain.pem")
      (ssl-certificate-key
       "/etc/letsencrypt/live/matrix.conses.eu/privkey.pem")
      (raw-content (list %nginx-crawlers-block))
      (locations
       (list
        (nginx-location-configuration
         (uri "~ ^(/_matrix|/_synapse/client)")
         (body
          (list "proxy_pass http://localhost:8008;"
                "proxy_set_header X-Forwarded-For $remote_addr;"
                "proxy_set_header X-Forwarded-Proto $scheme;"
                "proxy_set_header Host $host;"
                "client_max_body_size 50M;")))
        %nginx-robots-txt
        %letsencrypt-acme-challenge)))
     (nginx-server-configuration
      (listen '("443 ssl http2"
                "[::]:443 ssl http2"))
      (server-name (list "pantalaimon.conses.eu"))
      (ssl-certificate
       "/etc/letsencrypt/live/pantalaimon.conses.eu/fullchain.pem")
      (ssl-certificate-key
       "/etc/letsencrypt/live/pantalaimon.conses.eu/privkey.pem")
      (raw-content (list %nginx-crawlers-block))
      (locations
       (list
        (nginx-location-configuration
         (uri "/")
         (body
          (list "proxy_pass http://localhost:8009;"
                "proxy_set_header X-Forwarded-For $remote_addr;"
                "proxy_set_header HOST $http_host;")))
        %nginx-robots-txt
        %letsencrypt-acme-challenge)))))
   (simple-service
    'add-matrix-ssl-certificates
    certbot-service-type
    (map
     (lambda (domain)
       (certificate-configuration
        (domains (list domain))
        (deploy-hook %nginx-deploy-hook)))
     (append
      (list "conses.eu")
      (map (cut string-append <> ".conses.eu")
           (list "pantalaimon" "matrix")))))))

(define %whoogle-services
  (list
   (service whoogle-service-type)
   (simple-service
    'add-whoogle-nginx-configuration
    nginx-service-type
    (list
     (nginx-server-configuration
      (listen '("443 ssl http2"))
      (server-name (list (string-append "whoogle." %default-domain)))
      (ssl-certificate
       (format #f "/etc/letsencrypt/live/whoogle.~a/fullchain.pem"
               %default-domain))
      (ssl-certificate-key
       (format #f "/etc/letsencrypt/live/whoogle.~a/privkey.pem"
               %default-domain))
      (raw-content (list %nginx-crawlers-block))
      (locations
       (list
        (nginx-location-configuration
         (uri "/")
         (body
          (list "proxy_pass http://localhost:5000/;"
                "proxy_set_header X-Forwarded-For $remote_addr;"
                "proxy_set_header X-Forwarded-Proto $scheme;"
                "proxy_set_header X-NginX-Proxy true;"
                "proxy_set_header X-Real-IP $remote_addr;"
                "proxy_set_header Host $http_host;")))
        %nginx-robots-txt)))))
   (simple-service
    'add-whoogle-ssl-certificate
    certbot-service-type
    (list
     (certificate-configuration
      (domains (list (string-append "whoogle." %default-domain)))
      (deploy-hook %nginx-deploy-hook))))))

(define cgit-org2html
  (let ((commit "e34765572aeb328cc5d9bac08b9df2b34b39da46")
        (revision "0"))
    (package
     (name "cgit-org2html")
     (version (git-version "0.0.0" revision commit))
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/amartos/cgit-org2html")
                     (commit commit)))
               (file-name (git-file-name name version))
               (sha256
                (base32 "1s2yxs8pvb3gjwgd3nxm4gq54lmwrir9fb73sd595b9nyrwk4zaw"))))
     (build-system copy-build-system)
     (arguments
      (list #:phases #~(modify-phases %standard-phases
                         (add-after 'unpack 'fix-paths
                           (lambda _
                             (patch-shebang "org2html")))
                         (add-after 'fix-paths 'embed-absolute-file-name
                           (lambda _
                             (substitute* "org2html"
                               (("emacs")
                                (string-append #$(this-package-input "emacs-no-x")
                                               "/bin/emacs")))))
                         (add-after 'install 'post-install
                           (lambda _
                             (chmod (string-append #$output "/bin/org2html") #o777)))
                         (add-after 'post-install 'wrap-program
                           (lambda _
                             (wrap-program (string-append #$output "/bin/org2html")
                               `("ORG2HTML_CSS_PATH" =
                                 (,(string-append #$source "/css/org2html.css")))
                               `("CGIT_CONFIG" = ("/usr/share/cgit"))))))
            #:install-plan #~'(("org2html" "bin/org2html"))))
     (inputs (list emacs-no-x))
     (home-page "https://github.com/amartos/cgit-org2html")
     (synopsis "An Org mode files converter for cgit")
     (description "The @code{org2html} script brings support for Org-Mode formatted about files
to cgit.")
     (license license:gpl3))))

(define %cgit-responsive-css
  (origin
    (method url-fetch)
    (uri
     "https://raw.githubusercontent.com/MatejaMaric/responsive-cgit-css/master/cgit.css")
    (sha256
     (base32 "07l53sik7c6r3xj0fxc4gl9aw8315qgl5hhyh570l89fj4vy7yhc"))))

(define %version-control-services
  (list
   (service fcgiwrap-service-type
            (fcgiwrap-configuration
             (user "root")
             (group "git")))
   (service gitolite-service-type
            (gitolite-configuration
             (admin-pubkey %default-ssh-key)
             (rc-file
              (gitolite-rc-file
               (umask #o0027)
               (git-config-keys "gitweb\\..*")))))
   (service
    cgit-service-type
    (cgit-configuration
     (project-list "/var/lib/gitolite/projects.list")
     (section-from-path 1)
     (repository-directory "/var/lib/gitolite/repositories")
     (repository-sort "name")
     (case-sensitive-sort? #f)
     (root-desc (string-append %default-fullname "'s Git repositories"))
     (enable-git-config? #t)
     (enable-index-links? #t)
     (enable-index-owner? #f)
     (enable-commit-graph? #t)
     (enable-log-filecount? #t)
     (enable-log-linecount? #t)
     (readme ":README")
     (extra-options
      (list "readme=:README.md"
            "readme=:README.org"
            "js=/share/cgit/cgit.js"))
     (remove-suffix? #t)
     (head-include "/cgit/head.html")
     (footer "/cgit/footer")
     (css "/cgit/cgit.css")
     (clone-url
      (list (format #f "https://git.~a/$CGIT_REPO_URL " %default-domain)))
     (about-filter
      (program-file
       "cgit-about-formatting"
       (with-imported-modules '((guix build utils))
         #~(begin
             (use-modules (guix build utils)
                          (ice-9 match))
             (define (file-extension file)
               (let ((dot (string-rindex file #\.)))
                 (and dot (substring file (+ 1 dot) (string-length file)))))
             (with-directory-excursion #$(file-append cgit "/lib/cgit/filters/html-converters")
               (apply execl
                      (match (file-extension (cadr (command-line)))
                        ((or "org" #f) #$(file-append cgit-org2html "/bin/org2html"))
                        ((or "md" "markdown" "mdown" "mkd") "./md2html")
                        ("rst" "./rst2html")
                        ("txt" "./txt2html")
                        ((or "html" "htm") #$(file-append coreutils "/bin/cat")))
                      (command-line)))))))
     (favicon "/share/cgit/favicon.ico")
     (source-filter
      (program-file
       "cgit-syntax-highlighting"
       #~(apply execl #$(file-append cgit "/lib/cgit/filters/syntax-highlighting.py")
                (command-line))))
     (nginx
      (list
       (nginx-server-configuration
        (listen '("443 ssl http2"))
        (server-name (list (string-append "git." %default-domain)))
        (root cgit)
        (ssl-certificate
         (format #f "/etc/letsencrypt/live/git.~a/fullchain.pem"
                 %default-domain))
        (ssl-certificate-key
         (format #f "/etc/letsencrypt/live/git.~a/privkey.pem"
                 %default-domain))
        (try-files (list "$uri" "@cgit"))
        (raw-content (list %nginx-crawlers-block))
        (locations
         (list
          (nginx-location-configuration
           (uri "/cgit/")
           (body
            (list
             #~(format
                #f "alias ~a/;"
                #$(file-union
                   "cgit-style"
                   `(("cgit.css"
                      ,(mixed-text-file
                        "cgit.css"
                        (slurp-file-like
                         (file-append cgit "/share/cgit/cgit.css"))
                        (slurp-file-like %cgit-responsive-css)))
                     ("head.html"
                      ,(plain-file
                        "head.html"
                        (call-with-output-string
                         (lambda (port)
                           (sxml->xml
                            '(meta
                              (@ (name "viewport")
                                 (content
                                  "width=device-width, initial-scale=1.0")))
                            port)))))
                     ("footer"
                      ,(plain-file "footer" "")))))
             "try_files $uri =404;")))
          (nginx-location-configuration
           (uri "@cgit")
           (body
            (list
             "fastcgi_param SCRIPT_FILENAME $document_root/lib/cgit/cgit.cgi;"
             "fastcgi_param PATH_INFO $uri;"
             "fastcgi_param QUERY_STRING $args;"
             "fastcgi_param HTTP_HOST $server_name;"
             "fastcgi_pass 127.0.0.1:9000;")))
          %nginx-robots-txt)))))))
   (simple-service
    'add-cgit-ssl-certificate
    certbot-service-type
    (list
     (certificate-configuration
      (domains (list (string-append "git." %default-domain)))
      (deploy-hook %nginx-deploy-hook))))))

(define %extra-services
  (append
   (list
    (service dhcp-client-service-type)
    (service rsync-service-type
             (rsync-configuration
              (modules
               (list
                (rsync-module
                 (name "site")
                 (file-name (string-append "/srv/http/" %default-domain)))))))
    (service openssh-service-type
             (openssh-configuration
              (openssh (@ (gnu packages ssh) openssh-sans-x))
              (password-authentication? #f)
              (permit-root-login 'prohibit-password)
              (authorized-keys `(("root" ,%lyra-ssh-key ,%default-ssh-key)
                                 ("deneb" ,%default-ssh-key)))))
    (service certbot-service-type
            (certbot-configuration
             (email %default-email)
             (webroot "/srv/http"))))
   %blog-services
   %matrix-services
   %tubo-services
   %version-control-services
   %whoogle-services))

(define-public features
  (list
   (feature-host-info
    #:host-name "cygnus"
    #:timezone %default-timezone)
   (feature-base-services
    #:guix-authorized-keys
    (list %lyra-signing-key))
   (feature-bootloader
    #:bootloader-configuration
    (bootloader-configuration
     (bootloader grub-bootloader)
     (targets '("/dev/vda"))
     (terminal-outputs '(console))))
   (feature-base-packages
    #:base-home-packages '()
    #:system-packages %extra-packages)
   (feature-file-systems
    #:file-systems %file-systems
    #:swap-devices %swap-devices)
   (feature-custom-services
    #:system-services %extra-services)
   (feature-docker)
   (feature-postgresql)))
