(define-module (quasar home services lisp)
  #:use-module (quasar home)
  #:use-module (efimerspan home services emacs)
  #:use-module (efimerspan home services web-browsers)
  #:use-module (efimerspan home services lisp)
  #:use-module (gnu home-services base)
  #:use-module (gnu services)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages commencement)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-28)
  #:export (lisp-service))

(define (lisp-service)
  (list
   (service home-lisp-service-type
            (home-lisp-configuration
             (sbclrc-lisp
              `((require "asdf")
                ,#~""
                (let ((guix-profile (format nil "~a/.guix-home/profile/lib/" (uiop:getenv "HOME"))))
                  (when (and (probe-file guix-profile)
                             (ignore-errors (asdf:load-system "cffi")))
                    (push guix-profile
                          (symbol-value (find-symbol (string '*foreign-library-directories*)
                                                     (find-package 'cffi))))))
                ,#~""
                (let ((quicklisp-init (merge-pathnames ".local/share/quicklisp/setup.lisp"
                                                       (user-homedir-pathname))))
                  (when (probe-file quicklisp-init)
                    (load quicklisp-init)))))
             (slynk-lisp
              `((setf (cdr (assoc 'slynk:*string-elision-length* slynk:*slynk-pprint-bindings*)) nil)))))
   (home-generic-service 'home-lisp-files
                         #:packages (list sbcl cl-prove)
                         #:files
                         `((".config/common-lisp/source-registry.conf.d/10-personal-lisp.conf"
                            ,(plain-file "10-personal-lisp.conf"
                                         (format "(:tree \"~a/src/\")" (getenv "HOME"))))
                           (".config/common-lisp/source-registry.conf.d/20-guix-profile-cl.conf"
                            ,(plain-file "20-guix-profile-cl.conf"
                                         (format
                                          "(:tree \"~a/.guix-home/profile/share/common-lisp/source/\")"
                                          (getenv "HOME"))))
                           (".config/common-lisp/source-registry.conf.d/30-guix-profile-emacs.conf"
                            ,(plain-file "30-guix-profile-emacs.conf"
                                         (format
                                          "(:tree \"~a/.guix-home/profile/share/emacs/site-lisp/\")"
                                          (getenv "HOME"))))))
   (elisp-configuration-service
    `((with-eval-after-load 'lisp-mode
        (custom-set-variables
         '(inferior-lisp-program (executable-find "sbcl"))))
      ,#~""
      (add-hook 'debugger-mode-hook 'eb-prog--truncate-lines)
      (add-hook 'sly-mrepl-mode-hook 'eb-lisp-embellish-mrepl-output)
      (add-hook 'sly-mode-hook 'eb-lisp-sly-autoconnect)
      (add-to-list 'display-buffer-alist '("\\*sly-mrepl.*\\*"
                                           (display-buffer-no-window)
                                           (allow-no-window . t)))
      (with-eval-after-load 'sly
        (custom-set-variables
         '(sly-command-switch-to-existing-lisp 'always)
         '(sly-mrepl-history-file-name (let ((history-file (locate-user-emacs-file "sly-mrepl-history")))
                                         (unless (file-exists-p history-file)
                                           (make-empty-file history-file))))
         '(sly-description-autofocus t)
         '(sly-net-coding-system 'utf-8-unix)
         '(sly-connection-poll-interval 0.1)
         '(sly-enable-evaluate-in-emacs t)
         '(sly-keep-buffers-on-connection-close nil)))
      ,#~""
      (add-hook 'sly-mrepl-mode-hook 'eb-prog-output-embellisher)
      (with-eval-after-load 'sly-mrepl
        (let ((map sly-mode-map))
          (define-key map (kbd "C-c M-n") 'sly-mrepl-next-prompt)
          (define-key map (kbd "C-c M-p") 'sly-mrepl-previous-prompt))
        (setq sly-mrepl-pop-sylvester nil)
        (define-key sly-mrepl-mode-map (kbd "C-M-q") 'indent-sexp)
        (custom-set-variables
         '(sly-mrepl-prompt-formatter 'eb-lisp-sly-custom-prompt)))
      ,#~""
      (with-eval-after-load 'elisp-mode
        (let ((map emacs-lisp-mode-map))
          (define-key map (kbd "C-x C-e") 'pp-eval-last-sexp)
          (define-key map (kbd "M-:") 'pp-eval-expression)
          (define-key map (kbd "C-c C-m") 'pp-macroexpand-last-sexp)
          (define-key map (kbd "C-c C-c") 'embark-pp-eval-defun)))
      ,#~""
      (define-key mode-specific-map "ri" 'ielm)
      (with-eval-after-load 'ielm
        (custom-set-variables
         '(ielm-header "")
         '(ielm-noisy nil)))
      ,#~""
      (sp-use-paredit-bindings)
      (with-eval-after-load 'smartparens
        (custom-set-variables
         '(sp-highlight-pair-overlay nil))
        (setq sp-lisp-modes (nconc
                             '(minibuffer-inactive-mode
                               sly-mode
                               comint-mode
                               elisp-mode)
                             sp-lisp-modes))
        (dolist (command '(smartparens-mode smartparens-strict-mode))
                (mapc (lambda (hook) (add-hook (intern (format "%s-hook" hook)) command)) sp-lisp-modes))
        (require 'smartparens-config))
      ,#~""
      (with-eval-after-load 'xref
        (custom-set-variables
         '(xref-auto-jump-to-first-definition 'move)
         '(xref-auto-jump-to-first-xref 'move)
         '(xref-prompt-for-identifier '(not xref-find-definitions-other-window
                                            xref-find-definitions-other-frame))
         '(xref-show-xrefs-function 'consult-xref)
         '(xref-show-definitions-function 'consult-xref)))
      ,#~""
      (add-hook 'compilation-filter-hook 'eb-prog-output-embellisher)
      (with-eval-after-load 'compile
        (custom-set-variables
         '(compilation-ask-about-save nil)))
      ,#~""
      (with-eval-after-load 'paren
        (custom-set-variables
         '(show-paren-style 'mixed)))
      ,#~""
      (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
      (add-hook 'prog-mode-hook 'electric-pair-local-mode)
      (add-hook 'prog-mode-hook 'yas-minor-mode)
      ,#~""
      (with-eval-after-load 're-builder
        (custom-set-variables
         '(reb-re-syntax 'rx)))
      ,#~""
      (add-to-list 'org-structure-template-alist '("li" . "src lisp"))
      (add-to-list 'org-structure-template-alist '("el" . "src elisp"))
      (add-to-list 'org-babel-load-languages '(lisp . t))
      (with-eval-after-load 'ob-lisp
        (custom-set-variables
         '(org-babel-lisp-eval-fn 'sly-eval)))
      ,#~""
      (with-eval-after-load 'ob-core
        (setq org-babel-default-header-args:elisp
              '((:lexical . "t")
                (:results . "scalar"))
              org-babel-default-header-args:lisp
              '((:results . "scalar")))))
    #:elisp-packages (list emacs-sly
                           emacs-smartparens
                           emacs-rainbow-delimiters))
   (nyxt-configuration-service
    `((asdf:load-system :slynk)
      ,#~""
      (define-command-global start-slynk (&optional (slynk-port *swank-port*))
        "Starts a Slynk server to connect to via the underlying Lisp process
from an interface such as Emacs's SLY."
        (slynk:create-server :port slynk-port :dont-close t)
        (echo "Slynk process started at port ~a." slynk-port))
      ,#~""
      (unless nyxt::*run-from-repl-p*
        (start-slynk))))))
