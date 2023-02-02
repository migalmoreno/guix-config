(define-module (conses features clojure)
  #:use-module (conses utils)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (rde home services emacs-xyz)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages)
  #:use-module (gnu packages clojure)
  #:use-module (gnu packages java)
  #:use-module (gnu packages node)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages compression)
  #:use-module (guix gexp)
  #:use-module (guix profiles)
  #:use-module (nongnu packages clojure)
  #:export (feature-clojure))

(define* (feature-clojure
          #:key
          (clojure clojure)
          (jdk (list openjdk17 "jdk"))
          (emacs-clj-deps-new-key "J"))
  "Set up and configure tooling for Clojure."
  (ensure-pred any-package? clojure)
  ;; (ensure-pred file-like? jdk)
  (ensure-pred string? emacs-clj-deps-new-key)

  (define f-name 'clojure)

  (define (get-home-services config)
    "Return home services related to Clojure."
    (list
     (simple-service
      'add-clojure-home-packages
      home-profile-service-type
      (list clojure
            leiningen
            unzip
            clojure-tools
            jdk))
     (simple-service
      'add-clojure-home-envs
      home-environment-variables-service-type
      '(("LEIN_HOME" . "$XDG_DATA_HOME/lein")))
     (rde-elisp-configuration-service
      f-name
      config
      `(;; (with-eval-after-load 'rde-keymaps
        ;;   (define-key rde-app-map (kbd ,emacs-clj-deps-new-key) 'clj-deps-new))
        (add-hook 'cider-docview-mode-hook 'toggle-truncate-lines)
        (with-eval-after-load 'cider
          (define-key cider-repl-mode-map (kbd "C-M-q") 'indent-sexp)
          (setq cider-words-of-inspiration '(""))
          (setq cider-repl-pop-to-buffer-on-connect nil)
          (setq cider-repl-display-in-current-window t)
          (setq cider-allow-jack-in-without-project t))
        ,@(if (get-value 'emacs-consult config)
              '((with-eval-after-load 'rde-completion
                  (add-to-list 'rde-completion-initial-narrow-alist '(cider-repl-mode . ?c)))
                (with-eval-after-load 'consult-imenu
                  (add-to-list 'consult-imenu-config
                               '(clojure-mode
                                 :toplevel "Variable"
                                 :types ((?f "Function" font-lock-function-name-face)
                                         (?m "Macro" font-lock-function-name-face)
                                         (?M "Method" font-lock-function-name-face)
                                         (?e "Event" font-lock-function-name-face)
                                         (?n "Namespace" font-lock-constant-face)
                                         (?k "Keyword" font-lock-keyword-face)
                                         (?c "Class" font-lock-type-face)
                                         (?t "Type" font-lock-type-face)
                                         (?v "Variable" font-lock-variable-name-face))))))
              '())
        (with-eval-after-load 'org
          (add-to-list 'org-structure-template-alist '("clj" . "src clojure"))
          (require 'ob-clojure)
          (require 'ob-java))
        (with-eval-after-load 'ob-core
          (setq org-babel-default-header-args:clojure
                '((:results . "scalar")
                  (:session . ""))))
        (with-eval-after-load 'ob-clojure
          (setq org-babel-clojure-backend 'cider)))
      #:elisp-packages (list emacs-clojure-mode
                             emacs-cider
                             ;; emacs-clj-deps-new
                             ))))

  (feature
   (name f-name)
   (values `((,f-name . ,clojure)))
   (home-services-getter get-home-services)))
