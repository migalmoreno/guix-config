(define-module (migalmoreno users vega markup)
  #:use-module (rde features emacs-xyz)
  #:use-module (rde features markup)
  #:use-module (rde packages))

(define-public features
  (list
   (feature-emacs-org
    #:org-directory "~/documents"
    #:org-priority-faces
    '((?A . (:foreground "#FF665C" :weight bold))
      (?B . (:foreground "#51AFEF"))
      (?C . (:foreground "#4CA171")))
    #:org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d!)"))
    #:org-todo-keyword-faces
    '(("TODO" . "#ff665c")
      ("NEXT" . "#FCCE7B")
      ("HOLD" . "#a991f1")
      ("DONE" . "#7bc275"))
    #:org-tag-alist
    '((:startgroup)
      (:endgroup)
      ("work" . ?w)
      ("project" . ?p)
      ("education" . ?d)
      ("finance" . ?f)
      ("chore" . ?c)))
   (feature-emacs-org-recur)
   (feature-emacs-org-roam
    #:org-roam-directory "~/notes"
    #:org-roam-todo? #t
    #:org-roam-dailies-directory "./"
    #:org-roam-capture-templates
    `(("w" "work" plain "%?"
       :if-new (file+head "work/%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+title: ${title}\n#+filetags: :${Topic}:\n")
       :unnarrowed t)
      ("p" "personal" plain "%?"
       :if-new (file+head "personal/%<%Y%m%d%H%M%S>-${slug}.org"
                          "#+title: ${title}\n#+filetags: :${Topic}:\n")
       :unnarrowed t))
    #:org-roam-dailies-capture-templates
    '(("w" "work" entry
       "* %?"
       :if-new (file+head "work/daily/%<%Y-%m-%d>.org"
                          "#+title: %<%Y-%m-%d>\n"))
      ("p" "personal" entry
       "* %?"
       :if-new (file+head "personal/daily/%<%Y-%m-%d>.org"
                          "#+title: %<%Y-%m-%d>\n"))))
   (feature-emacs-org-agenda)
   (feature-emacs-monocle)
   (feature-markdown
    #:headings-scaling? #t)
   (feature-tex
    #:listings-options
    '(("basicstyle" "\\ttfamily")
      ("stringstyle" "\\color{blue}\\ttfamily")
      ("numbers" "left")
      ("numberstyle" "\\tiny")
      ("breaklines" "true")
      ("showstringspaces" "false")
      ("showtabs" "false")
      ("keywordstyle" "\\color{violet}")
      ("commentstyle" "\\color{gray}")
      ("label" "{Figure}")))
   (feature-emacs-citation
    #:global-bibliography (list "~/documents/references.bib"))))
