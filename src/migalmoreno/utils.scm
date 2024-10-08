(define-module (migalmoreno utils)
  #:use-module (gnu services)
  #:use-module (gnu services web)
  #:use-module (gnu system keyboard)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix utils)
  #:use-module (rde features)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 ftw)
  #:export (feature-home-extension
            user-features))


;;; Common utilities

(define* (feature-home-extension #:key name service extension)
  (feature
   (name name)
   (home-services-getter
    (lambda (config)
      (list
       (simple-service
        name
        (get-value service config)
        (extension config)))))))

(define-public %project-root
  (canonicalize-path
   (dirname
    (find (lambda (path)
            (every file-exists?
                   (map (lambda (file)
                          (string-append (dirname path) "/" file))
                        (list "src/migalmoreno"))))
          %load-path))))

(define-public (project-file subpath)
  (local-file (string-append %project-root "/" subpath)))

(define (submodule-features submodule source palette)
  (append-map
   (lambda (file)
     (let* ((features (module-ref
                       (resolve-module
                        `(,@submodule
                          ,(string->symbol (file-sans-extension file))))
                       'features))
            (arity (procedure-minimum-arity features)))
       (if (or (eq? arity #f) (not (eq? (car arity) 2)))
           features
           (features source palette))))
   (scandir
    (file-sans-extension
     (search-path %load-path
                  (module-name->file-name
                   (module-name
                    (resolve-module submodule)))))
    (cut string-suffix? ".scm" <>))))

(define (user-features name . r)
  (apply submodule-features `(migalmoreno users ,(string->symbol name)) r))


;;; Defaults

(define-public %nginx-deploy-hook
  (program-file
   "nginx-deploy-hook"
   #~(let ((pid (call-with-input-file "/var/run/nginx/pid" read)))
       (kill pid SIGHUP))))

(define-public %letsencrypt-acme-challenge
  (nginx-location-configuration
   (uri "/.well-known")
   (body '("root /srv/http;"))))

(define-public %nonguix-signing-key
  (project-file "src/migalmoreno/keys/nonguix.pub"))

(define-public %default-ssh-keygrip
  "D6B4894600BB392AB2AEDE499CBBCF3E0620B7F6")

(define-public %default-kernel (@ (nongnu packages linux) linux))

(define-public %default-timezone "Europe/Madrid")

(define-public %default-fullname "Miguel Ángel Moreno")

(define-public %default-username "migalmoreno")

(define-public %default-domain "migalmoreno.com")

(define-public %default-email "mail@migalmoreno.com")

(define-public %default-ssh-key
  (plain-file
   (format #f "~a.pub" %default-username)
   "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJgHrggw/+ZcncBvWeRmSf/PfaiGVmU2xnuh9C3mfbLN\n"))

(define-public %default-kernel-arguments
  (list "quiet" "net.ifnames=0"))

(define-public %default-keyboard-layout
  (keyboard-layout
   "us,es"
   #:options '("grp:shifts_toggle"
               "caps:ctrl_modifier"
               "altwin:prtsc_rwin")))
