(define-module (quasar home services clojure)
  #:use-module (conses home services emacs)
  #:use-module (rde home services emacs-xyz)
  #:use-module (conses packages emacs-xyz)
  #:use-module (nongnu packages clojure)
  #:use-module (gnu home-services base)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages)
  #:use-module (gnu packages clojure)
  #:use-module (gnu packages java)
  #:use-module (gnu packages node)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages compression)
  #:use-module (guix profiles)
  #:use-module (guix gexp)
  #:export (clojure-service))

(define (clojure-service)
  (list
   (home-generic-service 'home-clojure-packages
                         #:packages (list clojure
                                          leiningen
                                          unzip
                                          clojure-tools
                                          (list openjdk17 "jdk")
                                          node))
   (simple-service
    'home-clojure-envs
    home-environment-variables-service-type
    '(("LEIN_HOME" . "$XDG_DATA_HOME/lein")))
   (simple-service
    'home-clojure-templates
    home-emacs-tempel-service-type
    `(,#~"clojure-mode clojurescript-mode"
      (var "(def " p ")")
      (ns "(ns " p n> "(:require [" p " :as " p "]))")
      (fn "(fn [" p "]" n> r> ")")
      (defn "(defn " p n> "[" p "]" n> r> ")")
      (let "(let [" p "]" n> r> ")")
      (atom "(atom " p ")")
      (record "(defrecord " p n> "[" p "])")))
   (elisp-configuration-service
    `((define-key mode-specific-map "rc" 'cider-jack-in)
      (add-hook 'cider-docview-mode-hook 'toggle-truncate-lines)
      (with-eval-after-load 'cider
        (define-key cider-repl-mode-map (kbd "C-M-q") 'indent-sexp)
        (custom-set-variables
         '(cider-repl-pop-to-buffer-on-connect nil)
         '(cider-repl-display-in-current-window t)
         '(cider-allow-jack-in-without-project t)))
      ,#~""
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
                               (?v "Variable" font-lock-variable-name-face)))))
      ,#~""
      (with-eval-after-load 'org
        (add-to-list 'org-structure-template-alist '("clj" . "src clojure"))
        (add-to-list 'org-babel-load-languages '(clojure . t))
        (add-to-list 'org-babel-load-languages '(java . t)))
      (with-eval-after-load 'ob-core
        (setq org-babel-default-header-args:clojure
              '((:results . "scalar")
                (:session . ""))))
      ,#~""
      (with-eval-after-load 'ob-clojure
        (custom-set-variables
         '(org-babel-clojure-backend 'cider)))
      ,#~""
      (require 'clj-deps-new)
      (define-key mode-specific-map "jn" 'clj-deps-new))
    #:elisp-packages (list emacs-clojure-mode
                           emacs-cider
                           emacs-clj-deps-new))))
