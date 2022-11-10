(define-module (conses features lisp)
  #:use-module (conses features web-browsers)
  #:use-module (conses utils)
  #:use-module (conses home services lisp)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (rde home services emacs-xyz)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages lisp-check)
  #:use-module (gnu packages commencement)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:export (feature-lisp))

(define* (feature-lisp
          #:key
          (lisp sbcl)
          (extra-sbclrc '())
          (extra-slynk '())
          (extra-packages '()))
  (ensure-pred any-package? lisp)
  (ensure-pred lisp-config? extra-sbclrc)
  (ensure-pred lisp-config? extra-slynk)
  (ensure-pred list-of-packages? extra-packages)

  (define f-name 'lisp)

  (define (get-home-services config)
    "Return home services related to SBCL."
    (list
     (service home-lisp-service-type
              (home-lisp-configuration
               (lisp lisp)
               (sbclrc-lisp
                `((require :asdf)
                  (let ((guix-profile (format nil "~a/.guix-home/profile/lib/" (uiop:getenv "HOME"))))
                    (when (and (probe-file guix-profile)
                               (ignore-errors (asdf:load-system "cffi")))
                      (push guix-profile
                            (symbol-value (find-symbol (string '*foreign-library-directories*)
                                                       (find-package 'cffi))))))
                  (let ((quicklisp-init (merge-pathnames ".local/share/quicklisp/setup.lisp"
                                                         (user-homedir-pathname))))
                    (when (probe-file quicklisp-init)
                      (load quicklisp-init)))
                  ,@extra-sbclrc))
               (slynk-lisp
                `((setf (cdr (assoc 'slynk:*string-elision-length* slynk:*slynk-pprint-bindings*)) nil)
                  ,@extra-slynk))))
     (simple-service
      'home-lisp-xdg-configuration-files-service
      home-xdg-configuration-files-service-type
      `(("common-lisp/source-registry.conf.d/10-personal-lisp.conf"
         ,(plain-file "10-personal-lisp.conf"
                      (format #f "(:tree \"~a/src/\")" (get-value 'home-directory config))))
        ("common-lisp/source-registry.conf.d/20-guix-profile-cl.conf"
         ,(plain-file "20-guix-profile-cl.conf"
                      (format #f "(:tree \"~a/.guix-home/profile/share/common-lisp/source/\")"
                              (get-value 'home-directory config))))
        ("common-lisp/source-registry.conf.d/30-guix-profile-emacs.conf"
         ,(plain-file "30-guix-profile-emacs.conf"
                      (format #f "(:tree \"~a/.guix-home/profile/share/emacs/site-lisp/\")"
                              (get-value 'home-directory config))))))
     (simple-service
      'home-lisp-profile-service
      home-profile-service-type
      extra-packages)
     (simple-service
      'home-lisp-emacs-tempel-service
      home-emacs-tempel-service-type
      `(,#~"lisp-mode"
           (lambda "(lambda (" p ")" n> r> ")")
           (fun "(defun " p " (" p ")\n \"" p "\"" n> r> ")")
           (var "(defvar " p "\n  \"" p "\")")
           (cond "(cond" n "(" q "))" >)
           (let "(let (" p ")" n> r> ")")
           (let* "(let* (" p ")" n> r> ")")
           (dolist "(dolist (" p ")" n> r> ")")
           ,#~"emacs-lisp-mode"
           (lambda "(lambda (" p ")" n> r> ")")
           (fun "(defun " p " (" p ")\n \"" p "\"" n> r> ")")
           (var "(defvar " p "\n  \"" p "\")")
           (cond "(cond" n "(" q "))" >)
           (let "(let (" p ")" n> r> ")")
           (let* "(let* (" p ")" n> r> ")")
           (dolist "(dolist (" p ")" n> r> ")")
           (autoload ";;;###autoload")
           (pt "(point)")
           (local "(defvar-local " p "\n \"" p "\")")
           (const "(defconst " p "\n  \"" p "\")")
           (custom "(defcustom " p "\n \"" p "\"" n> ":type '" p ")")
           (face "(defface " p " '((t :inherit " p "))\n \"" p "\")")
           (group "(defgroup " p " nil\n \"" p "\"" n> ":group '" p n> ":prefix \"" p "-\")")
           (macro "(defmacro " p " (" p ")\n \"" p "\"" n> r> ")")
           (alias "(defalias '" p " '" p ")")
           (iflet "(if-let (" p ")" n> r> ")")
           (whenlet "(when-let (" p ")" n> r> ")")
           (iflet* "(if-let* (" p ")" n> r> ")")
           (whenlet* "(when-let* (" p ")" n> r> ")")
           (andlet "(and-let* (" p ")" n> r> ")")
           (pcase "(pcase " (p "scrutinee") n "(" q "))" >)
           (rec "(letrec (" p ")" n> r> ")")
           (dotimes "(dotimes (" p ")" n> r> ")")
           (loop "(cl-loop for " p " in " p " do" n> r> ")")
           (command "(defnn " p " (" p ")\n  \"" "\"" n> "(interactive" p ")" n> r> ")")
           (advice "(defun " (p "adv" name) " (&rest app)" n> p n> "(apply app))" n>
                   "(advice-add #'" (p "fun") " " (p ":around") " #'" (s name) ")")
           (provide "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
                    ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name))) " ends here" n)))
     (rde-elisp-configuration-service
      f-name
      config
      `((require 'sly)
        (defgroup configure-lisp nil
          "Lisp tools and extensions."
          :group 'configure)

        (defun configure-lisp-sly-autoconnect ()
          "Start a SLY REPL unless an active connection is already present."
          (unless (sly-connected-p)
            (save-excursion
              (sly))))

        (defun configure-lisp-sly-custom-prompt (_package nickname error-level next-entry-idx _condition)
          "Construct a custom SLY prompt with package NICKNAME, ERROR-LEVEL and NEXT-ENTRY-IDX."
          (let ((dir (propertize (abbreviate-file-name default-directory) 'font-lock-face 'diff-mode))
                (nick (propertize nickname 'font-lock-face 'sly-mode-line))
                (idx (propertize (number-to-string next-entry-idx) 'font-lock-face 'diff-mode))
                (err-level (when (cl-plusp error-level)
                             (concat (sly-make-action-button
                                      (format " [%d]" error-level)
                                      #'sly-db-pop-to-debugger-maybe)
                                     ""))))
            (concat "(" dir ")\n"
                    (propertize "<" 'font-lock-face 'sly-mrepl-prompt-face)
                    idx
                    (propertize ":" 'font-lock-face 'sly-mrepl-prompt-face)
                    nick
                    err-level
                    (propertize "> " 'font-lock-face 'sly-mrepl-prompt-face))))

        (defun configure-lisp-setup-sly-history ()
          "Create an empty history file for SLY if missing."
          (unless (file-exists-p sly-mrepl-history-file-name)
            (make-empty-file sly-mrepl-history-file-name)))

        (with-eval-after-load 'lisp-mode
          (setq inferior-lisp-program ,(file-append lisp "/bin/" (package-name lisp))))
        (add-hook 'debugger-mode-hook 'toggle-truncate-lines)
        (add-hook 'sly-mode-hook 'configure-lisp-sly-autoconnect)
        (add-hook 'sly-mode-hook 'configure-lisp-setup-sly-history)
        (add-to-list 'display-buffer-alist `(,(rx "*sly-mrepl" (* any) "*")
                                             (display-buffer-no-window)
                                             (allow-no-window . t)))
        (sly-setup)
        (with-eval-after-load 'sly
          (setq sly-words-of-encouragement '(""))
          (setq sly-command-switch-to-existing-lisp 'always)
          (setq sly-description-autofocus t)
          (setq sly-net-coding-system 'utf-8-unix)
          (setq sly-connection-poll-interval 0.1)
          (setq sly-enable-evaluate-in-emacs t)
          (setq sly-keep-buffers-on-connection-close nil))
        (with-eval-after-load 'sly-mrepl
          (define-key lisp-mode-map (kbd "C-c C-z") 'sly-mrepl)
          (let ((map sly-mode-map))
            (define-key map (kbd "C-c C-b") 'sly-eval-buffer)
            (define-key map (kbd "C-c C-q") 'sly-interrupt))
          (let ((map sly-mrepl-mode-map))
            (define-key map (kbd "C-M-q") 'indent-sexp)
            (define-key map (kbd "C-c C-z") 'sly-switch-to-most-recent)
            (define-key map (kbd "C-c M-n") 'sly-mrepl-next-prompt)
            (define-key map (kbd "C-c M-p") 'sly-mrepl-previous-prompt))
          (setq sly-mrepl-history-file-name (expand-file-name "emacs/sly-mrepl-history" (xdg-cache-home)))
          (setq sly-mrepl-prevent-duplicate-history t)
          (setq sly-mrepl-pop-sylvester nil)
          (setq sly-mrepl-prompt-formatter 'configure-lisp-sly-custom-prompt))
        (with-eval-after-load 'org
          (require 'ob-lisp)
          (add-to-list 'org-structure-template-alist '("li" . "src lisp")))
        (with-eval-after-load 'ob-lisp
          (setq org-babel-lisp-eval-fn 'sly-eval))
        (with-eval-after-load 'ob-core
          (setq org-babel-default-header-args:lisp
                '((:results . "scalar")))))
      #:elisp-packages (list emacs-sly)
      #:summary "Extensions for Lisp tooling"
      #:commentary "Provide extensions for Common Lisp programming utilities.")
     (rde-nyxt-configuration-service
      f-name
      config
      `((unless nyxt::*run-from-repl-p*
          (start-slynk))))))

  (feature
   (name f-name)
   (values `((,f-name . ,lisp)))
   (home-services-getter get-home-services)))
