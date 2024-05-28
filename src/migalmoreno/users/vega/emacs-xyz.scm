(define-module (migalmoreno users vega emacs-xyz)
  #:use-module ((migalmoreno presets emacs-xyz) #:prefix presets:)
  #:use-module (migalmoreno utils)
  #:use-module (rde features emacs)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde packages))

(define %extra-init-el
  `(,@presets:extra-init-el
    (add-hook 'after-init-hook 'server-start)
    (with-eval-after-load 'password-cache
      (setq password-cache t)
      (setq password-cache-expiry (* 60 10)))
    (with-eval-after-load 'pass
      (setq pass-show-keybindings nil))
    (with-eval-after-load 'epg-config
      (setq epg-pinentry-mode 'loopback))
    (with-eval-after-load 'password-store
      (setq password-store-time-before-clipboard-restore 60))
    (with-eval-after-load 'prog-mode
      (setq prettify-symbols-unprettify-at-point 'right-edge))
    (with-eval-after-load 'rde-completion
      (add-to-list 'rde-completion-initial-narrow-alist
                   '(cider-repl-mode . ?c)))
    (with-eval-after-load 'face-remap
      (setq text-scale-mode-step 1.075))
    (setq-default tab-width 2)
    (with-eval-after-load 'indent
      (setq tab-always-indent 'complete))
    (add-hook 'before-save-hook 'delete-trailing-whitespace)
    (with-eval-after-load 'mwheel
      (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)
                                          ((control) . 1)))
      (setq mouse-wheel-progressive-speed nil)
      (setq mouse-wheel-follow-mouse t)
      (setq scroll-conservatively 100)
      (setq mouse-autoselect-window nil)
      (setq what-cursor-show-names t)
      (setq focus-follows-mouse t))
    (winner-mode)
    (define-key ctl-x-map (kbd "C-b") 'ibuffer)
    (with-eval-after-load 'ibuffer
      (setq ibuffer-expert t))
    (with-eval-after-load 'image-mode
      (define-key image-mode-map "q" 'image-kill-buffer)
      (setq image-use-external-converter t))
    (with-eval-after-load 'js
      (setq js-indent-level 2))
    (cl-defun rde-org-do-promote (&optional (levels 1))
      "Allow promoting the current heading LEVELS high up the tree."
      (interactive "p")
      (save-excursion
        (if (org-region-active-p)
            (org-map-region (lambda ()
                              (dotimes (_ levels)
                                (org-promote)))
                            (region-beginning) (region-end))
          (dotimes (_ levels)
            (org-promote))))
      (org-fix-position-after-promote))
    (advice-add 'org-do-promote :override 'rde-org-do-promote)
    (define-key mode-specific-map (kbd "l") 'org-store-link)
    (add-hook 'org-mode-hook 'prettify-symbols-mode)
    (add-hook 'org-mode-hook 'org-fragtog-mode)
    (add-hook 'org-mode-hook 'variable-pitch-mode)
    (org-crypt-use-before-save-magic)
    (with-eval-after-load 'org
      (require 'org-tempo)
      (require 'org-timer)
      (let ((map org-mode-map))
        (define-key map (kbd "M-n") 'org-metaright)
        (define-key map (kbd "M-p") 'org-metaleft))
      (setq org-startup-folded 'content)
      (setq org-startup-with-inline-images t)
      (setq org-startup-with-latex-preview t)
      (setq org-extend-today-until 0)
      (setq org-use-fast-todo-selection 'expert)
      (setq org-log-done 'time)
      (setq org-log-into-drawer t)
      (setq org-special-ctrl-a/e t)
      (setq org-insert-heading-respect-content t)
      (setq org-auto-align-tags t)
      (setq org-tags-exclude-from-inheritance '("todo" "crypt"))
      (setq org-enforce-todo-dependencies t)
      (setq org-enforce-todo-checkbox-dependencies t)
      (setq org-archive-location "~/documents/archive.org::* From %s")
      (setq org-fast-tag-selection-single-key 'expert)
      (setq org-display-remote-inline-images 'cache)
      (setq org-image-actual-width nil)
      (setq org-pretty-entities t)
      (setq org-pretty-entities-include-sub-superscripts nil)
      (setq org-M-RET-may-split-line nil)
      (setq org-highest-priority ?A)
      (setq org-lowest-priority ?C)
      (setq org-default-priority ?B)
      (setq org-fontify-done-headline t)
      (add-to-list 'org-structure-template-alist
                   '("js" . "src js")))
    (with-eval-after-load 'org-list
      (setq org-list-demote-modify-bullet
            '(("+" . "-") ("-" . "+") ("*" . "+"))))
    (with-eval-after-load 'org-capture
      (setq org-capture-bookmark nil))
    (with-eval-after-load 'org-src
      (setq org-src-tab-acts-natively t)
      (setq org-src-window-setup 'current-window)
      (setq org-catch-invisible-edits 'show-and-error)
      (setq org-src-fontify-natively t))
    (with-eval-after-load 'org-keys
      (setq org-return-follows-link t))
    (with-eval-after-load 'org-download
      (setq org-download-image-dir "images")
      (setq org-download-image-org-width 300))
    (with-eval-after-load 'ob-core
      (require 'ob-js)
      (setq org-confirm-babel-evaluate nil))
    (with-eval-after-load 'ol
      (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file))
    (with-eval-after-load 'ox
      (setq org-export-preserve-breaks t)
      (require 'ox-md)
      (require 'ox-haunt))
    (with-eval-after-load 'css-mode
      (setq css-indent-offset 2))
    (with-eval-after-load 'mhtml-mode
      (define-key html-mode-map (kbd "M-o") nil))
    (defun rde-completion-crm-indicator (args)
      "Display a discernible indicator for `completing-read-multiple'."
      (cons (concat "[CRM] " (car args)) (cdr args)))
    (advice-add 'completing-read-multiple
                :filter-args 'rde-completion-crm-indicator)
    (define-key goto-map (kbd "a") 'consult-org-agenda)
    (define-key goto-map (kbd "h") 'consult-org-heading)
    (define-key ctl-x-map "b" 'consult-buffer)
    (define-key help-map "a" 'consult-apropos)
    (define-key ctl-x-map (kbd "M-:") 'consult-complex-command)
    (define-key ctl-x-4-map "b" 'consult-buffer-other-window)
    (with-eval-after-load 'consult
      (setq consult-narrow-key "C-=")
      (setq consult-widen-key "C--"))
    (define-key help-map "b" 'embark-bindings)
    (with-eval-after-load 'embark
      (setq embark-indicators '(embark-minimal-indicator))
      (setq embark-prompter 'embark-keymap-prompter)
      (setq prefix-help-command 'embark-prefix-help-command))
    (autoload 'corfu-history-mode "corfu-history")
    (corfu-history-mode)
    (with-eval-after-load 'corfu
      (let ((map corfu-map))
        (define-key map "\t" 'corfu-next)
        (define-key map (kbd "<tab>") 'corfu-next)
        (define-key map (kbd "<backtab>") 'corfu-previous)
        (define-key map (kbd "S-TAB") 'corfu-previous)
        (define-key map (kbd "M-p") 'corfu-doc-scroll-down)
        (define-key map (kbd "M-n") 'corfu-doc-scroll-up)
        (define-key map (kbd "M-d") 'corfu-doc-toggle))
      (require 'kind-icon)
      (setq corfu-auto-prefix 2)
      (setq global-corfu-modes '((not org-mode) t))
      (global-corfu-mode 1)
      (setq kind-icon-default-face 'corfu-default)
      (add-to-list 'corfu-margin-formatters 'kind-icon-margin-formatter)
      (set-face-attribute 'corfu-default nil :inherit 'fixed-pitch))
    (add-to-list 'auto-mode-alist '("\\.y[a]?ml\\'" . yaml-mode))
    (with-eval-after-load 'yaml-mode
      (define-key yaml-mode-map (kbd "RET") 'newline-and-indent))
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'eww-mode-hook 'eww-toggle-images)
    (with-eval-after-load 'window
      (setq split-window-keep-point t)
      (add-to-list 'display-buffer-alist
                   `(,(rx bos "*Embark Collect "
                          (or "Live" "Completions") "*")
                     nil
                     (window-parameters (mode-line-format . none))))
      (add-to-list 'display-buffer-alist
                   `(,(rx "*help" (* any) "*")
                     (display-buffer-reuse-window
                      display-buffer-same-window)
                     (reusable-frames . t)))
      (add-to-list 'display-buffer-alist
                   `(,(rx "*Org Links*")
                     display-buffer-no-window
                     (allow-no-window . t)))
      (add-to-list 'display-buffer-alist
                   `(,(rx "*org-roam*")
                     display-buffer-same-window)))
    (repeat-mode 1)
    (with-eval-after-load 'rde-keymaps
      (define-key rde-toggle-map "f" 'display-fill-column-indicator-mode))
    (setq copyright-names-regexp
          (format "%s <%s>" user-full-name user-mail-address))
    (add-hook 'after-save-hook 'copyright-update)
    (with-eval-after-load 'cider-repl
      (setq cider-repl-display-in-current-window t))
    (with-eval-after-load 'ange-ftp
      (setq ange-ftp-try-passive-mode t))
    (defun rde-git-email--get-current-project ()
      "Return the path of the current project.
Falls back to `default-directory'."
      (let ((dir (or (and (bound-and-true-p projectile-known-projects)
                          (projectile-project-root))
                     (and (bound-and-true-p project-list-file)
                          (if (and (listp (cdr (project-current)))
                                   (> (length (project-current)) 2))
                              (car (last (project-current)))
                            (cdr (project-current))))
                     (vc-root-dir)
                     default-directory)))
        dir))
    (advice-add 'git-email--get-current-project
                :override 'rde-git-email--get-current-project)
    (define-key ctl-x-map "g" 'magit)
    (add-hook 'magit-mode-hook 'toggle-truncate-lines)
    (with-eval-after-load 'project
      (define-key project-prefix-map "m" 'magit-project-status)
      (add-to-list 'project-switch-commands
                   '(magit-project-status "Show Magit Status")))
    (with-eval-after-load 'magit
      (define-key magit-mode-map "q" 'magit-kill-this-buffer)
      (setq magit-display-buffer-function
            'magit-display-buffer-same-window-except-diff-v1)
      (setq magit-pull-or-fetch t)
      (require 'forge)
      (setq forge-owned-accounts '((,%default-username)
                                   (,%default-username))))
    (with-eval-after-load 'vc
      (define-key vc-prefix-map "W" 'git-email-format-patch)
      (setq vc-follow-symlinks t)
      (setq vc-ignore-dir-regexp
            (format "%s\\|%s"
                    vc-ignore-dir-regexp tramp-file-name-regexp)))
    (with-eval-after-load 'ediff
      (setq ediff-window-setup-function 'ediff-setup-windows-plain))
    (with-eval-after-load 'git-email
      (require 'git-email-magit)
      (git-email-gnus-mode 1))
    (with-eval-after-load 'piem
      (setq piem-inboxes
            '(("rde-devel"
               :url "https://lists.sr.ht/~abcdw/rde-devel"
               :address "~abcdw/rde-devel@lists.sr.ht"
               :coderepo "~/src/guile/rde/")))
      (piem-gnus-mode 1))
    (with-eval-after-load 'rde-power-menu
      (define-key global-map (kbd "s-m") 'rde-power-menu))))

(define %extra-elisp-packages
  (strings->packages
   "emacs-tempel-collection" "emacs-ox-haunt" "emacs-pinentry"
   "emacs-yaml-mode" "emacs-nginx-mode" "emacs-pug-mode"
   "emacs-rainbow-delimiters" "emacs-kind-icon" "emacs-wgrep"
   "emacs-org-fragtog" "emacs-org-download" "emacs-org-make-toc" "emacs-forge"
   "emacs-git-email" "emacs-piem" "emacs-nix-mode"))

(define-public emacs-features
  (append
   (list
    (feature-emacs
     #:extra-early-init-el presets:extra-early-init-el
     #:extra-init-el %extra-init-el
     #:default-application-launcher? #f
     #:additional-elisp-packages %extra-elisp-packages
     #:emacs-server-mode? #f))
   presets:emacs-features))
