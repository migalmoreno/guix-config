(define-module (quasar home services emacs)
  #:use-module (quasar home)
  #:use-module (quasar home packages emacs-xyz)
  #:use-module (conses packages emacs-xyz)
  #:use-module (conses packages misc)
  #:use-module (conses home services emacs)
  #:use-module (conses home services web-browsers)
  #:use-module (gnu services)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages aspell)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home-services base)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home-services-utils)
  #:use-module (flat packages emacs)
  #:use-module (guix gexp)
  #:use-module (ice-9 ftw)
  #:export (emacs-service))

(define dired-service
  (list
   (home-generic-service 'home-dired-packages #:packages (list zip rsync))
   (elisp-configuration-service
    `((define-key global-map (kbd "s-d") 'dired-jump)
      (with-eval-after-load 'dired
        (add-hook 'dired-mode-hook 'dired-hide-details-mode)
        (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
        (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
        (custom-set-variables
         '(dired-dwim-target t)
         '(dired-listing-switches "-lA")
         '(delete-by-moving-to-trash nil)
         '(dired-kill-when-opening-new-dired-buffer t)
         '(dired-recursive-deletes 'always)
         '(dired-clean-confirm-killing-deleted-buffers nil)
         '(dired-recursive-copies 'always)
         '(dired-deletion-confirmer 'y-or-n-p)
         '(dired-rsync-options "--exclude .git/ --exclude .gitignore -az --info=progress2 --delete"))
        (let ((map dired-mode-map))
          (define-key map "e" 'emms-play-dired)
          (define-key map "q" 'kill-current-buffer)
          (define-key map (kbd "C-c C-r") 'dired-rsync)
          (define-key map "V" 'eb-files-open-externally)))
      ,#~""
      (with-eval-after-load 'ls-lisp
        (custom-set-variables
         '(ls-lisp-dirs-first t)
         '(ls-lisp-use-insert-directory-program nil))))
    #:elisp-packages (list emacs-dired-rsync))))

(define org-service
  (list
   (home-generic-service 'home-org-packages #:packages (list unoconv libreoffice))
   (elisp-configuration-service
    `((dolist (module '(org-indent org-tempo org-habit org-crypt org-protocol))
              (add-to-list 'org-modules module))
      ,#~""
      (add-hook 'org-mode-hook 'org-fragtog-mode)
      (add-hook 'org-mode-hook 'eb-org-minimal-ui-mode)
      ,#~""
      (let ((map mode-specific-map))
        (define-key map "l" 'org-store-link)
        (define-key map "c" 'org-capture))
      (with-eval-after-load 'org
        (let ((map org-mode-map))
          (define-key map (kbd "M-n") 'org-metaright)
          (define-key map (kbd "M-p") 'org-metaleft)
          (define-key map (kbd "C-c ob") 'org-switchb)
          (define-key map (kbd "C-c de") 'org-decrypt-entries))
        (with-eval-after-load 'consult
          (define-key org-mode-map (kbd "M-g h") 'consult-org-heading))
        (custom-set-variables
         '(org-directory (xdg-user-dir "DOCUMENTS"))
         '(org-agenda-files (list (expand-file-name "tasks.org" (xdg-user-dir "DOCUMENTS"))))
         '(org-return-follows-link t)
         '(org-startup-folded 'content)
         '(org-startup-indented t)
         '(org-startup-with-inline-images t)
         '(org-startup-with-latex-preview t)
         '(org-extend-today-until 0)
         '(org-use-fast-todo-selection 'expert)
         '(org-todo-keywords
           '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "INTR(i)" "|" "DONE(d!)")
             (sequence "GOING(g)" "|" "MISSED(m@)" "ATTENDED(a@)")))
         '(org-todo-keyword-faces
           '(("TODO" . "#ff665c")
             ("NEXT" . "#FCCE7B")
             ("PROG" . "#51afef")
             ("INTR" . "#a991f1")
             ("DONE" . "#7bc275")))
         '(org-highest-priority ?A)
         '(org-lowest-priority ?C)
         '(org-log-done 'time)
         '(org-log-into-drawer t)
         '(org-tags-exclude-from-inheritance '("project" "crypt"))
         '(org-default-priority ?B)
         '(org-default-notes-file "~/notes")
         '(org-enforce-todo-dependencies t)
         '(org-enforce-todo-checkbox-dependencies t)
         '(org-priority-faces '((?A . (:foreground "#FF665C" :weight bold))
                                (?B . (:foreground "#51AFEF"))
                                (?C . (:foreground "#4CA171"))))
         '(org-archive-location "~/org/archive.org::* From %s")
         '(org-outline-path-complete-in-steps nil)
         '(org-tag-alist
           '((:startgroup)
             (:endgroup)
             ("work" . ?w)
             ("emacs" . ?e)
             ("project" . ?p)
             ("linux" . ?l)
             ("education" . ?d)
             ("finance" . ?f)
             ("guix" . ?g)
             ("chore" . ?c)))
         '(org-fast-tag-selection-single-key 'expert)
         '(org-display-remote-inline-images 'cache)
         '(org-image-actual-width '(350))
         '(org-pretty-entities t)
         '(org-pretty-entities-include-sub-superscripts nil)
         '(org-M-RET-may-split-line nil)
         '(org-ellipsis " ⤵")
         '(org-hide-emphasis-markers t)
         '(org-fontify-done-headline t))
        ;; TODO: tweak the regexp to not apply this within source blocks
        (font-lock-add-keywords 'org-mode
                                '(("^ *\\([-]\\)[[:space:]][^[]+?"
                                   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))
                                  ("^ *\\([+]\\)[[:space:]][^[]+?"
                                   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "‣")))))))
      ,#~""
      (with-eval-after-load 'ob-core
          (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
      ,#~""
      (advice-add 'org-refile :after 'org-save-all-org-buffers)
      (with-eval-after-load 'org-refile
        (custom-set-variables
         '(org-refile-targets
           '((org-agenda-files . (:maxlevel . 1))))))
      ,#~""
      (with-eval-after-load 'org-capture
        (custom-set-variables
         '(org-capture-bookmark nil)
         '(org-capture-templates
           '(("t" "Tasks/Projects")
             ("tt" "TODO Entry" entry (file+olp "~/documents/tasks.org" "TODOs")
              "* TODO %? %^G\n%U\n"
              :empty-lines 1)
             ("tb" "Read It Later" entry (file+headline "~/documents/tasks.org" "Read It Later")
              "* %a %^G"
              :immediate-finish 1)
             ("tv" "Watch It Later" entry (file+headline "~/documents/tasks.org" "Watch It Later")
              "* %a %^G"
              :immediate-finish 1)
             ("th" "Habit Entry" entry (file+headline "~/documents/tasks.org" "Habits")
              "* TODO %? %^G\nSCHEDULED:%^t\n:PROPERTIES:\n:STYLE: habit\n:END:"
              :empty-lines 1)
             ("tc" "Critical TODO Entry" entry (file+headline "~/documents/tasks.org" "TODOs")
              "* INTR %? %^G\n%U\n"
              :empty-lines 1)
             ("ts" "TODO Entry with Subtasks" entry (file+headline "~/documents/tasks.org" "TODOs")
              "* TODO %? [/]\n- [ ]"
              :empty-lines 1)
             ("td" "Scheduled Task" entry (file+headline "~/documents/tasks.org" "TODOs")
              "* TODO %?\nSCHEDULED: %^T"
              :empty-lines 1)))))
      ,#~""
      (let ((map mode-specific-map))
        (define-key map "oa" 'org-agenda)
        (define-key map "oA" 'eb-org-agenda-open-dashboard))
      (add-hook 'org-agenda-mode-hook 'hack-dir-local-variables-non-file-buffer)
      (with-eval-after-load 'org-agenda
        (custom-set-variables
         '(org-agenda-start-with-log-mode t)
         '(org-agenda-todo-ignore-scheduled t)
         '(org-agenda-todo-ignore-deadlines t)
         '(org-agenda-todo-ignore-timestamp t)
         '(org-agenda-window-setup 'current-window)
         '(org-agenda-dim-blocked-tasks t)
         '(org-agenda-skip-scheduled-if-done t)
         '(org-agenda-skip-deadline-if-done t)
         '(org-agenda-compact-blocks nil)
         '(org-agenda-include-diary t)
         '(org-agenda-custom-commands
           '(("d" "Workflow"
              ((agenda "" ((org-deadline-warning-days 7)))
               (todo "INTR"
                     ((org-agenda-overriding-header "Critical Tasks")))
               (todo "PROG"
                     ((org-agenda-overriding-header "Tasks In Progress")))
               (todo "NEXT"
                     ((org-agenda-overriding-header "Tasks Not Yet Started")))
               (todo "TODO"
                     ((org-agenda-overriding-header "Tasks To Review"))))
              ((org-agenda-prefix-format
                '((agenda . " %i %?-12t% s")
                  (todo . "%?-12:c")
                  (tags . "%c")
                  (search . " %i %-12:c")))))))
         '(org-agenda-bulk-custom-functions
           '((?P (lambda nil
                   (org-agenda-priority 'set)))))))
      ,#~""
      (with-eval-after-load 'org-habit
        (custom-set-variables
         '(org-habit-graph-column 60)))
      ,#~""
      (with-eval-after-load 'ob-core
        (custom-set-variables
         '(org-confirm-babel-evaluate nil))
        (setq org-babel-default-header-args:sql
              '((:engine . "postgresql"))))
      ,#~""
      (with-eval-after-load 'org-src
        (custom-set-variables
         '(org-src-tab-acts-natively t)
         '(org-edit-src-content-indentation 0)
         '(org-src-window-setup 'current-window)
         '(org-catch-invisible-edits 'smart)
         '(org-src-fontify-natively t)))
      ,#~""
      (with-eval-after-load 'org-list
        (custom-set-variables
         '(org-list-demote-modify-bullet
           '(("+" . "-")
             ("-" . "+")
             ("*" . "-")))))
      ,#~""
      (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
      (with-eval-after-load 'visual-fill-column
        (setq-default visual-fill-column-center-text t)
        (custom-set-variables
         '(visual-fill-column-enable-sensible-window-split t)
         '(visual-fill-column-width 90)))
      ,#~""
      (org-crypt-use-before-save-magic)
      (with-eval-after-load 'org-crypt
        (custom-set-variables
         '(org-crypt-key nil)))
      ,#~""
      (add-hook 'org-timer-stop-hook 'eb-org-timer-reset)
      (advice-add 'org-timer-update-mode-line :override 'eb-org-timer-update-mode-line)
      (with-eval-after-load 'org-timer
        (custom-set-variables
         '(org-timer-format (concat (eb-look--position-item "") " %s  "))))
      ,#~""
      (setq org-roam-v2-ack t)
      (custom-set-variables
       '(org-roam-directory "~/notes"))
      (org-roam-db-autosync-enable)
      (let ((map mode-specific-map))
        (define-key map "nb" 'eb-org-roam-switch-to-buffer)
        (define-key map "nl" 'org-roam-buffer-toggle)
        (define-key map "nf" 'org-roam-node-find)
        (define-key map "nr" 'org-roam-ref-find)
        (define-key map "nc" 'org-roam-capture)
        (define-key map "ni" 'org-roam-node-insert)
        (define-key map "nI" 'eb-org-roam-node-insert-immediate)
        (define-key map "ndn" 'org-roam-dailies-capture-today)
        (define-key map "ndd" 'org-roam-dailies-goto-today)
        (define-key map "ndY" 'org-roam-dailies-capture-yesterday)
        (define-key map "ndT" 'org-roam-dailies-capture-tomorrow)
        (define-key map "ndy" 'org-roam-dailies-goto-yesterday)
        (define-key map "ndt" 'org-roam-dailies-goto-tomorrow)
        (define-key map "ndC" 'org-roam-dailies-capture-date)
        (define-key map "ndc" 'org-roam-dailies-goto-date))
      (with-eval-after-load 'org-roam
        (let ((map org-mode-map))
          (define-key map (kbd "C-TAB") 'completion-at-point)
          (define-key map (kbd "C-c r r") 'org-roam-ref-add)
          (define-key map (kbd "C-c r R") 'org-roam-ref-remove)
          (define-key map (kbd "C-c r f") 'org-roam-ref-find)
          (define-key map (kbd "C-c r t") 'org-roam-tag-add)
          (define-key map (kbd "C-c r T") 'org-roam-tag-remove)
          (define-key map (kbd "C-c r a") 'org-roam-alias-add)
          (define-key map (kbd "C-c r A") 'org-roam-alias-remove)
          (define-key map (kbd "C-c r O") 'eb-org-roam-open-ref)
          (define-key map (kbd "C-c n df") 'org-roam-dailies-goto-next-note)
          (define-key map (kbd "C-c n db") 'org-roam-dailies-goto-previous-note))
        (custom-set-variables
         '(org-roam-capture-templates
           '(("d" "default" plain "%?"
              :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                 "#+title: ${title}\n#+filetags: :${Topic}:\n")
              :unnarrowed t)
             ("e" "Programming Concept" plain
              "#+begin_src %^{Language|elisp|lisp|scheme|clojure}\n%?\n#+end_src\n"
              :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                 ":PROPERTIES:\n:DATA_TYPE: %^{Data Type|Function|Method\
|Variable|Macro|Procedure}\n:END:\n#+title: ${title}\n#+filetags: :${Topic}:")
              :unnarrowed t)
             ("r" "Referenced Concept" plain
              "%?"
              :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                                 ":PROPERTIES:\n:ROAM_REFS: %^{Reference}\n:END:\n\
#+title: ${title}\n#+filetags: :${Topic}:")
              :unnarrowed t)
             ("w" "Web Resource" plain
              "%?"
              :if-new (file+head "%<%Y%m%d%H%M%S>-${title}.org"
                                 ":PROPERTIES:\n:ROAM_REFS: %l\n:END:\n\
#+title: ${title}\n#+filetags: :${Topic}:")
              :unnarrowed t)
             ("r" "Recipe"
              "* Ingredients\n- %?\n* Directions"
              :if-new (file+head "%<%Y%m%d%H%M%S>-${title}.org"
                                 ":PROPERTIES:\n:ROAM_REFS: %l\n:MEAL_TYPE: %^{Meal Type|Lunch\
|Breakfast|Appetizer|Dessert}\n:END:\n#+title: ${title}\n#+filetags: :cooking:")
              :unnarrowed t)
             ("b" "Book" plain
              "* Chapters\n%?"
              :if-new (file+head "%<%Y%M%d%H%M%S>-${slug}.org"
                                 ":PROPERTIES:\n:AUTHOR: ${Author}\n:DATE: ${Date}\n\
:PUBLISHER: ${Publisher}\n:EDITION: ${Edition}\n:END:\n#+title: ${title}\n#+filetags: :${Topic}:")
              :unnarrowed t))))
      ,#~""
      (with-eval-after-load 'org-roam-node
        (custom-set-variables
         '(org-roam-completion-everywhere t)
         '(org-roam-node-display-template "${title:*} ${tags:35}")))
      ,#~""
      (with-eval-after-load 'org-roam-dailies
        (custom-set-variables
         '(org-roam-dailies-directory "journal")
         '(org-roam-dailies-capture-templates
           '(("d" "default" entry
              "* %<%I:%M %p>: %?"
              :if-new (file+head "%<%Y-%m-%d>.org"
                                 "#+title: %<%Y-%m-d>\n"))))))
      ,#~""
      (with-eval-after-load 'org-superstar
        (setq org-superstar-headline-bullets-list '(?\s)
              org-superstar-leading-bullet ?\s
              org-superstar-prettify-item-bullets nil))
      ,#~""
      (with-eval-after-load 'embark
        (embark-define-keymap embark-roam-ref-map
                              "Keymap for actions to be triggered on Org Roam node `ROAM-REFS'."
                              :parent embark-url-map
                              ("RET" browse-url-generic)
                              ("v" eb-media-mpv-start)
                              ("c" browse-url-chromium))
        (add-to-list 'embark-keymap-alist '(org-roam-ref . embark-roam-ref-map)))
      ,#~""
      (with-eval-after-load 'ol
        (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file))
      ,#~""
      (with-eval-after-load 'ox
        (custom-set-variables
         '(org-export-with-date nil)
         '(org-export-preserve-breaks t))))
      (eb-org-agenda-appt-mode)
      (advice-add 'org-redo :after 'eb-org-agenda-to-appt)
      (add-hook 'org-capture-after-finalize-hook 'eb-org-agenda-to-appt))
    #:elisp-packages (list emacs-org
                           emacs-org-contrib
                           emacs-org-mime
                           emacs-org-make-toc
                           emacs-org-roam
                           emacs-org-appear
                           emacs-org-superstar
                           emacs-visual-fill-column))
   (nyxt-configuration-service
    `((define-command org-capture ()
        "Captures the current page in Org mode."
        (eval-in-emacs
         `(org-link-set-parameters
           "nyxt"
           :store (lambda ()
                    (org-link-store-props
                     :type "nyxt"
                     :link ,(quri:render-uri (url (current-buffer)))
                     :description ,(title (current-buffer)))))
         `(org-capture nil "tb"))
        (echo "Org mode note stored"))
      ,#~""
      (define-command org-roam-capture ()
        "Captures the current page as an Org Roam node."
        (eval-in-emacs
         `(org-link-set-parameters
           "nyxt"
           :store (lambda ()
                    (org-link-store-props
                     :type "nyxt"
                     :link ,(quri:render-uri (url (current-buffer)))
                     :description ,(title (current-buffer)))))
         `(org-roam-capture nil "w")))
      (define-key *custom-keymap*
        "M-c c" 'org-capture
        "M-c n" 'org-roam-capture)))))

(define completion-service
  (list
   (home-generic-service 'completion-packages #:packages (list ripgrep fd))
   (elisp-configuration-service
    `((vertico-mode)
      (with-eval-after-load 'vertico
        (define-key vertico-map "?" 'minibuffer-completion-help)
        (define-key vertico-map (kbd "M-RET") 'minibuffer-force-complete-and-exit)
        (define-key vertico-map (kbd "M-TAB") 'minibuffer-complete)
        (custom-set-variables
         '(vertico-cycle t)
         '(vertico-resize 'grow-only)))
      ,#~""
      (savehist-mode)
      (with-eval-after-load 'savehist
        (custom-set-variables
         '(history-length 1000)))
      ,#~""
      (require 'orderless)
      (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode)
      (with-eval-after-load 'minibuffer
        (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
              enable-recursive-minibuffers t
              completion-in-region-function (lambda (&rest args)
                                              (apply (if vertico-mode
                                                         'consult-completion-in-region
                                                         'completion--in-region)
                                                     args))
              completion-category-defaults nil)
        (custom-set-variables
         '(read-file-name-completion-ignore-case t)
         '(read-buffer-completion-ignore-case t)
         '(read-extended-command-predicate 'command-completion-default-include-p)
         '(completion-cycle-threshold t)
         '(completion-styles '(orderless basic))
         '(completion-category-overrides '((file (styles partial-completion))))))
      ,#~""
      (all-the-icons-completion-mode)
      (with-eval-after-load 'all-the-icons
        (add-hook 'marginalia-mode-hook 'all-the-icons-completion-marginalia-setup))
      ,#~""
      (advice-add 'completing-read-multiple :filter-args 'eb-completion-crm-indicator)
      (advice-add 'completing-read-multiple
                  :override 'consult-completing-read-multiple)
      ,#~""
      (autoload 'embark-pp-eval-defun "embark")
      (define-key global-map (kbd "C-.") 'embark-act)
      (define-key global-map (kbd "M-.") 'embark-dwim)
      (with-eval-after-load 'embark
        (setq prefix-help-command 'embark-prefix-help-command)
        (add-to-list 'display-buffer-alist
                     '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                       nil
                       (window-parameters (mode-line-format . none))))
        (custom-set-variables
         '(embark-indicators '(embark-minimal-indicator))
         '(embark-prompter 'embark-keymap-prompter)))
      ,#~""
      (let ((map global-map))
        (define-key map (kbd "M-s r") 'consult-ripgrep)
        (define-key map (kbd "C-s") 'consult-line)
        (define-key map (kbd "M-y") 'consult-yank-pop)
        (define-key ctl-x-4-map "b" 'consult-buffer-other-window)
        (define-key goto-map "a" 'consult-org-agenda)
        (define-key goto-map "f" 'consult-find)
        (define-key goto-map "i"  'consult-imenu)
        (define-key ctl-x-map "b" 'consult-buffer)
        (define-key goto-map "g" 'consult-goto-line)
        (define-key ctl-x-map (kbd "M-:") 'consult-complex-command)
        (define-key isearch-mode-map (kbd "M-s e") 'consult-isearch-history))
    (define-key minibuffer-mode-map (kbd "C-c C-r") 'consult-history)
    (add-hook 'minibuffer-setup-hook 'eb-completion-consult-initial-narrow)
    (with-eval-after-load 'consult
      (setq consult-buffer-sources
            (append
             consult-buffer-sources
             '(eb-org-buffer-source
               eb-chat-telega-buffer-source
               eb-chat-erc-buffer-source
               eb-exwm-buffer-source
               eb-shell-buffer-source)))
      (custom-set-variables
       '(consult-find-args "fd . -H -F -t f -E .git node_modules .cache")
       '(consult-narrow-key (kbd "C-="))
       '(consult-widen-key (kbd "C--")))
      (consult-customize
       consult--source-buffer consult-ripgrep consult-buffer
       consult-bookmark consult--source-bookmark
       consult-recent-file consult--source-recent-file
       :preview-key (kbd "M-.")))
    (custom-set-variables
     '(eb-completion-initial-narrow-alist
       `((erc-mode . ?e)
         (org-mode . ?o)
         (exwm-mode . ?x)
         (telega-root-mode . ?t)
         (telega-chat-mode . ?t)
         (comint-mode . ?c)
         (cider-repl-mode . ?c))))
      ,#~""
      (marginalia-mode)
      ,#~""
      (global-corfu-mode)
      (add-hook 'corfu-mode-hook 'corfu-doc-mode)
      (with-eval-after-load 'corfu
        (custom-set-variables
         '(corfu-auto t)
         '(corfu-cycle t)
         '(corfu-auto-prefix 2)
         '(corfu-auto-delay 0.2)
         '(corfu-echo-documentation '(1.0 . 0.2))
         '(corfu-quit-at-boundary t)
         '(corfu-preselect-first nil))
        (let ((map corfu-map))
          (define-key map "\t" 'corfu-next)
          (define-key map (kbd "<tab>") 'corfu-next)
          (define-key map (kbd "<backtab>") 'corfu-previous)
          (define-key map (kbd "S-TAB") 'corfu-previous)
          (define-key map (kbd "M-p") 'corfu-doc-scroll-down)
          (define-key map (kbd "M-n") 'corfu-doc-scroll-up)
          (define-key map (kbd "M-d") 'corfu-doc-toggle))
        (require 'kind-icon)
        (custom-set-variables
         '(kind-icon-default-face 'corfu-default))
        (add-to-list 'corfu-margin-formatters 'kind-icon-margin-formatter)
        (set-face-attribute 'corfu-default nil :inherit 'fixed-pitch)))
    #:elisp-packages (list emacs-vertico
                           emacs-orderless
                           emacs-embark
                           emacs-consult
                           emacs-marginalia
                           emacs-all-the-icons-completion
                           emacs-kind-icon
                           emacs-capf-autosuggest
                           emacs-corfu
                           emacs-corfu-doc))))

(define editing-service
  (elisp-configuration-service
   `((custom-set-variables
      '(tab-width 2)
      '(indent-tabs-mode nil))
     ,#~""
     (with-eval-after-load 'indent
       (custom-set-variables
        '(tab-always-indent 'complete)))
     ,#~""
     (with-eval-after-load 'yasnippet
       (yas-reload-all))
     ,#~""
     (global-so-long-mode)
     ,#~""
     (electric-pair-mode -1))
   #:elisp-packages (list emacs-yasnippet)))

(define prose-service
  (list
   (home-generic-service 'prose-packages
                         #:packages (list aspell aspell-dict-en))
   (elisp-configuration-service
    `((define-key mode-specific-map "ds" 'dictionary-search)
      (with-eval-after-load 'dictionary
        (custom-set-variables
         '(dictionary-server "dict.org")))
      ,#~""
      (add-hook 'emacs-mode-hook 'flyspell-prog-mode)
      (add-hook 'lisp-mode-hook 'flyspell-prog-mode)
      (add-hook 'clojure-mode-hook 'flyspell-prog-mode)
      (with-eval-after-load 'flyspell
        (custom-set-variables
         '(ispell-program-name "aspell")
         '(ispell-dictionary "en_US")
         '(flyspell-issue-welcome-flag nil)
         '(flyspell-issue-message-flag nil)))))))

(define image-service
  (list
   (home-generic-service 'image-packages
                         #:packages (list imagemagick))
   (elisp-configuration-service
    `((with-eval-after-load 'image-mode
        (define-key image-mode-map "q" 'image-kill-buffer)
        (custom-set-variables
         '(image-user-external-converter t)))))))

(define pdf-service
  (elisp-configuration-service
   `((pdf-loader-install)
     (add-hook 'pdf-view-mode-hook 'pdf-view-themed-minor-mode)
     (with-eval-after-load 'pdf-tools
       (custom-set-variables
        '(pdf-view-display-size 'fit-page)
        '(pdf-view-resize-factor 1.025))
       (require 'saveplace-pdf-view)
       (save-place-mode)))
   #:elisp-packages (list emacs-pdf-tools emacs-saveplace-pdf-view)))

(define files-service
  (elisp-configuration-service
   `((with-eval-after-load 'files
       (custom-set-variables
        '(backup-directory-alist
          `((".*" . "/tmp")))
        '(auto-save-file-name-transforms
          `((".*" "/tmp" t)))
        '(auto-save-no-message t)
        '(create-lockfiles nil)
        '(delete-old-versions t)
        '(kept-new-versions 3)
        '(kept-old-versions 2)
        '(version-control t)
        '(remote-file-name-inhibit-cache nil)
        '(safe-local-variable-values '((after-save-hook . (org-babel-tangle))
                                       (ispell-dictionary . "en_GB")))))
     ,#~""
     (setq custom-file
           (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory))
     (with-eval-after-load 'custom
       (custom-set-variables
        '(custom-safe-themes t))
       (when (file-exists-p custom-file)
         (load custom-file)))
     ,#~""
     (recentf-mode)
     ,#~""
     (global-auto-revert-mode)
     (with-eval-after-load 'autorevert
       (custom-set-variables
        '(auto-revert-remote-files nil))))))

(define tramp-service
  (elisp-configuration-service
   `((let ((map mode-specific-map))
      (define-key map "Ff" 'eb-files-find-file)
      (define-key map "Fd" 'eb-files-dired)
      (define-key map "Fs" 'eb-files-tramp-shell))
     (with-eval-after-load 'tramp
       (custom-set-variables
        '(tramp-verbose 1)
        '(tramp-default-method "ssh"))
       (setq tramp-remote-path
             (append tramp-remote-path
                     '(tramp-own-remote-path)))))))

(define calendar-service
  (elisp-configuration-service
   `((define-key mode-specific-map "C" 'calendar)
     (with-eval-after-load 'calendar
       (with-eval-after-load 'ebdb
         (custom-set-variables
          '(diary-file (expand-file-name "diary" (xdg-user-dir "DOCUMENTS")))))
       (custom-set-variables
        '(calendar-week-start-day 1)
        '(calendar-view-diary-initially-flag t)
        '(calendar-date-style 'european)
        '(calendar-mark-diary-entries-flag t)))
     ,#~""
     (appt-activate 1)
     (let ((map mode-specific-map))
       (define-key map "aa" 'appt-add)
       (define-key map "ad" 'appt-delete))
     (with-eval-after-load 'appt
       (custom-set-variables
        '(appt-display-format 'echo)
        '(appt-audible nil)
        '(appt-message-warning-time 10)
        '(appt-display-interval 2)
        '(appt-display-diary nil))))))

(define time-service
  (elisp-configuration-service
   `((define-key mode-specific-map "wc" 'world-clock)
     (custom-set-variables
      '(display-time-default-load-average nil)
      '(display-time-load-average-threshold 0)
      '(display-time-day-and-date t)
      '(display-time-24hr-format t)
      '(display-time-world-list '(("Europe/London" "London")
                                  ("Europe/Madrid" "Madrid")
                                  ("America/New_York" "New York")))
      '(display-time-world-time-format "%R %Z"))
     (display-time-mode))))

(define finance-service
  (elisp-configuration-service
   `((add-hook 'calc-start-hook 'calc-currency-load)
     (autoload 'calc-currency-load "calc-currency")
     (with-eval-after-load 'calc-currency
       (custom-set-variables
        '(calc-currency-base-currency 'EUR)
        '(calc-currency-update-interval 7))))
   #:elisp-packages (list emacs-calc-currency)))

(define bookmark-service
  (list
   (nyxt-configuration-service
    `((define-command save-as-emacs-bookmark ()
        "Save current page in Nyxt as a bookmark record in Emacs."
        (eval-in-emacs
         `(let ((bookmark-make-record-function
                 (lambda ()
                   (eb-web--bookmark-make-record
                    ,(quri:render-uri (url (current-buffer)))
                    ,(title (current-buffer))))))
            (bookmark-set)))
        (echo "Org Roam node stored"))
      (define-key *custom-keymap*
        "M-c r" 'save-as-emacs-bookmark)))
   (elisp-configuration-service
    `((with-eval-after-load 'bookmark
        (custom-set-variables
         '(bookmark-save-flag 1)
         '(bookmark-set-fringe-mark nil)
         '(bookmark-default-file
           (expand-file-name "bookmarks" (xdg-user-dir "DOCUMENTS")))))))))

(define appearance-service
  (elisp-configuration-service
   `((with-eval-after-load 'eb-look
       (custom-set-variables
        '(eb-look-light-theme "modus-operandi")
        '(eb-look-dark-theme "modus-vivendi")
        '(eb-look-dark-theme-threshold "21:30")
        '(eb-look-fixed-font "Iosevka")
        '(eb-look-variable-font "IBM Plex Sans")
        '(eb-look-headless-font-size 105)
        '(eb-look-docked-font-size 105)))
     (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
     (set-fontset-font "fontset-default" nil (font-spec :name "Noto Color Emoji"))
     (set-fontset-font t 'unicode "Noto Color Emoji" nil 'append)
     (setq use-default-font-for-symbols nil)
     (with-eval-after-load 'modus-themes
       (define-key mode-specific-map "Mt" 'modus-themes-toggle)
       (custom-set-variables
        '(modus-themes-italic-constructs t)
        '(modus-themes-bold-constructs t)
        '(modus-themes-org-blocks 'gray-background)
        '(modus-themes-region '(bg-only no-extend))
        '(modus-themes-markup '(intense))
        '(modus-themes-mixed-fonts t)))
     (with-eval-after-load 'all-the-icons
       (custom-set-variables
        '(all-the-icons-scale-factor 1)
        '(all-the-icons-default-adjust 0)))
     ,#~""
     (define-key mode-specific-map "do" 'eb-look-dashboard-open)
     (with-eval-after-load 'dashboard
       (custom-set-variables
        '(dashboard-banner-logo-title "Welcome to Emacs")
        '(dashboard-startup-banner 'logo)
        '(dashboard-center-content t)
        '(dashboard-show-shortcuts nil)
        '(dashboard-set-heading-icons t)
        '(dashboard-set-file-icons t)
        '(dashboard-set-footer nil)
        '(dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
        '(dashboard-week-agenda t)
        '(dashboard-page-separator "\n\n")
        '(dashboard-agenda-release-buffers t)
        '(dashboard-banner-logo-png ,(file-append gnu-meditate-logo "/meditate.png"))
        '(dashboard-item-generators '((recents . dashboard-insert-recents)
                                      (bookmarks . dashboard-insert-bookmarks)
                                      (agenda . dashboard-insert-agenda)
                                      (registers . dashboard-insert-registers)))
        '(dashboard-items '((recents . 5)
                            (bookmarks . 5)
                            (agenda . 15)))))
     ,#~""
     (minions-mode)
     (with-eval-after-load 'minions
       (custom-set-variables
        '(minions-mode-line-lighter ";")))
     (define-key mode-specific-map "ei" 'eb-look-emoji-insert)
     (with-eval-after-load 'fringe
       (set-fringe-mode 10))
     ,#~""
     (add-hook 'before-save-hook 'delete-trailing-whitespace)
     (global-whitespace-mode)
     (custom-set-variables
      '(whitespace-style '(face tabs tab-mark))
      '(whitespace-global-modes '(not org-mode
                                      org-agenda-mode
                                      org-agenda-follow-mode
                                      org-capture-mode
                                      dired-mode
                                      eshell-mode
                                      magit-status-mode
                                      diary-mode
                                      magit-diff-mode
                                      text-mode
                                      pass-view-mode
                                      erc-mode)))
     ,#~""
     (define-key global-map (kbd "C-=") 'text-scale-increase)
     (define-key global-map (kbd "C--") 'text-scale-decrease)
     ,#~""
     (custom-set-variables
      '(echo-keystrokes 0)
      '(ring-bell-function 'ignore)
      '(visible-bell nil))
     ,#~""
     (custom-set-variables
      '(mode-line-compact 'long))
     (setq mode-line-misc-info '(("" so-long-mode-line-info)))
     ,#~""
     (fset 'yes-or-no-p 'y-or-n-p)
     (transient-mark-mode)
     (delete-selection-mode)
     ,#~""
     (with-eval-after-load 'prog-mode
       (custom-set-variables
        '(prettify-symbols-unprettify-at-point 'right-edge))
       (setq-default prettify-symbols-alist
                     '(("#+BEGIN_SRC" . "Λ")
                       ("#+END_SRC" . "λ")
                       ("#+begin_src" . "λ")
                       ("#+end_src" . "λ")
                       ("#+begin_quote" . "")
                       ("#+BEGIN_QUOTE" . "")
                       ("#+end_quote" . "")
                       ("#+END_QUOTE" . "")
                       (":LOGBOOK:" . "")
                       (":PROPERTIES:" . "")
                       ("#+filetags:" . "")
                       ("#+FILETAGS:" . "")
                       ("#+title:" . "")
                       ("#+TITLE:" . "")
                       ("#+author:" . "")
                       ("#+AUTHOR:" . "")
                       ("# -*-" . "")
                       ("-*-" . "")
                       ("- [ ]" . "")
                       ("+ [ ]" . "")
                       ("- [x]" . "")
                       ("+ [x]" . "")
                       ("- [X]" . "")
                       ("+ [X]" . "")
                       ("- [-]" . "")
                       ("+ [-]" . ""))))
     ,#~""
     (add-hook 'text-mode-hook 'display-line-numbers-mode)
     (add-hook 'conf-mode-hook 'display-line-numbers-mode)
     (add-hook 'prog-mode-hook 'display-line-numbers-mode)
     (with-eval-after-load 'display-line-numbers
       (custom-set-variables
        '(display-line-numbers-type 'relative))))
   #:elisp-packages (list emacs-minions
                          emacs-modus-themes
                          emacs-all-the-icons
                          emacs-all-the-icons-dired
                          emacs-dashboard)
   #:early-init `((setq inhibit-splash-screen t
                        inhibit-startup-message t
                        initial-scratch-message nil)
                  (push '(menu-bar-lines . 0) default-frame-alist)
                  (push '(tool-bar-lines . 0) default-frame-alist)
                  (push '(vertical-scroll-bars) default-frame-alist))))

(define tab-service
  (elisp-configuration-service
   '((tab-bar-mode)
     (with-eval-after-load 'tab-bar
       (custom-set-variables
        '(tab-bar-format '(eb-tab-format-left
                           eb-tab-format-center
                           eb-tab-format-align-right
                           eb-tab-format-right))
        '(tab-bar-close-button-show nil)
        '(tab-bar-show t))))))

(define project-service
  (elisp-configuration-service
   `((add-hook 'project-find-functions 'project-try-vc)
     (add-hook 'project-find-functions 'eb-prog-project-custom-root)
     (advice-add 'project-compile :override 'eb-prog-project-compile)
     (with-eval-after-load 'eb-prog
       (custom-set-variables
        '(eb-prog-configuration-project (expand-file-name "~/src/guixrc"))))
     (with-eval-after-load 'project
       (eb-prog--add-project-commands
        `((?c "Compile Project" project-compile)
          (?m "Show Magit Status" magit-status)
          (?s "Start an inferior shell" project-shell)
          (?F "Find file consult" consult-find)
          (?R "Search for regexp with rg" eb-prog-project-ripgrep)
          (?C "Capture with Org" eb-prog-org-capture)))))))

(define structures-service
  (elisp-configuration-service
   `((with-eval-after-load 'window
       (nconc display-buffer-alist
              '(("\\*help.*\\*"
                 (display-buffer-reuse-window display-buffer-same-window)
                 (reusable-frames . t))))
       (custom-set-variables
        '(even-window-sizes nil)
        '(split-window-keep-point t)))

     (define-key ctl-x-map (kbd "C-b") 'ibuffer)
     (with-eval-after-load 'ibuffer
       (custom-set-variables
        '(ibuffer-expert t)))
     ,#~""
     (with-eval-after-load 'winner
       (winner-mode))
     ,#~""
     (define-key global-map (kbd "M-o") 'ace-window)
     (with-eval-after-load 'ace-window
       (set-face-attribute 'aw-leading-char-face nil
                           :foreground "#51afef"
                           :weight 'bold
                           :height 1.0)
       (custom-set-variables
        '(aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
        '(aw-background nil)
        '(aw-scope 'frame)
        '(aw-ignore-current nil)
        '(aw-display-mode-overlay nil)))
     ,#~""
     (setq-default cursor-in-non-selected-windows nil
                   frame-title-format '("%b - Emacs"))
     ,#~""
     (with-eval-after-load 'frame
       (add-to-list 'initial-frame-alist '(fullscreen . maximized))
       (blink-cursor-mode 0)))
   #:elisp-packages (list emacs-ace-window)))

(define (emacs-service)
  `(,@dired-service
    ,@org-service
    ,editing-service
    ,@prose-service
    ,@completion-service
    ,@image-service
    ,pdf-service
    ,files-service
    ,tramp-service
    ,time-service
    ,calendar-service
    ,finance-service
    ,@bookmark-service
    ,appearance-service
    ,tab-service
    ,project-service
    ,structures-service
    ,(service home-emacs-service-type
             (home-emacs-configuration
              (package
                (if (or (string= (getenv "XDG_SESSION_TYPE") "x11")
                        (string= (getenv "XDG_SESSION_TYPE") "tty"))
                    emacs-native-comp
                    emacs-next-pgtk))
              (elisp-packages (list emacs-elibs))
              (early-init-el
               `((require 'xdg)
                 (add-hook 'emacs-startup-hook (lambda ()
                                                 (setq gc-cons-threshold 800000)))
                 ,#~""
                 (setq gc-cons-threshold most-positive-fixnum
                       package-enable-at-startup nil
                       package-native-compile t
                       package-user-dir (expand-file-name "emacs/elpa" (xdg-data-home))
                       auto-save-list-file-prefix (expand-file-name "emacs/auto-save-list/.saves-"
                                                                    (xdg-data-home)))))
              (init-el
               `(,#~";; -*- lexical-binding: t; -*-"
                 (require 'xdg)
                 (require 'eb-util)
                 ,#~""
                 (with-eval-after-load 'comp
                   (custom-set-variables
                    '(native-comp-async-report-warnings-errors nil)))
                 ,#~""
                 (require 'warnings)
                 (custom-set-variables
                  '(warning-suppress-types '((diary) (auto-save) (org-babel)))
                  '(warning-suppress-log-types '((comp org-babel)))
                  '(warning-minimum-level :error))
                 ,#~""
                 (server-start)))))
    ,(simple-service 'add-emacs-envs
                     home-environment-variables-service-type
                     `(("VISUAL" . ,(file-append emacs-native-comp "/bin/emacsclient"))
                       ("EDITOR" . ,(file-append emacs-native-comp "/bin/emacsclient"))))
    ,(simple-service 'home-emacs-xdg
                     home-xdg-mime-applications-service-type
                     (home-xdg-mime-applications-configuration
                      (default
                       '((x-scheme-handler/unknown . emacsclient.desktop)
                         (image/png . emacsclient.desktop)
                         (image/jpg . emacsclient.desktop)
                         (image/jpeg . emacsclient.desktop)
                         (application/pdf . emacsclient.desktop)
                         (application/desktop . emacsclient.desktop)
                         (application/json . emacsclient.desktop)
                         (text/javascript . emacsclient.desktop)
                         (text/html . emacsclient.desktop)
                         (text/css . emacsclient.desktop)))))))
