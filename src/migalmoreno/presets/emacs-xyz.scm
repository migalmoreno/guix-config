(define-module (migalmoreno presets emacs-xyz)
  #:use-module (rde features emacs-xyz)
  #:use-module (guix gexp))

(define %extra-tempel-templates
  `(text-mode
    ,#~""
    (cut "--8<---------------cut here---------------start------------->8---"
         n r n
         "--8<---------------cut here---------------end--------------->8---"
         n)
    (asciibox "+-" (make-string (length str) ?-) "-+" n
              "| " (s str) " |" n
              "+-" (make-string (length str) ?-) "-+" n)))

(define-public extra-init-el
  `((eval-when-compile
     (require 'cl-lib))
    (setq frame-title-format
          '(multiple-frames
            "%b"
            ("" "%b - GNU Emacs at " system-name
             " [" (:eval (frame-parameter (selected-frame) 'window-id)) "]")))
    (require 'warnings)
    (setq warning-suppress-types '((diary) (auto-save) (org-babel)))
    (setq warning-suppress-log-types '((comp org-babel)))
    (setq warning-minimum-level :error)
    (setq mode-line-misc-info
          (remove '(global-mode-string ("" global-mode-string))
                  mode-line-misc-info))
    (setq ring-bell-function 'ignore)
    (setq visible-bell nil)
    (fset 'yes-or-no-p 'y-or-n-p)
    (transient-mark-mode)
    (delete-selection-mode)
    (tooltip-mode -1)
    (with-eval-after-load 'comp
      (setq native-comp-async-report-warnings-errors nil))
    (global-so-long-mode)
    (column-number-mode 0)
    (with-eval-after-load 'autorevert
      (setq auto-revert-remote-files nil))
    (setq auto-save-no-message t)
    (setq create-lockfiles nil)
    (setq delete-old-versions t)
    (setq kept-new-versions 3)
    (setq kept-old-versions 2)
    (setq version-control t)
    (setq remote-file-name-inhibit-cache nil)))

(define-public extra-early-init-el
  '((require 'xdg)
    (setq echo-keystrokes 0)
    (setq package-native-compile t)
    (setq package-user-dir
          (expand-file-name "emacs/elpa" (xdg-data-home)))
    (setq auto-save-list-file-prefix
          (expand-file-name "emacs/auto-save-list/.saves-"
                            (xdg-data-home)))))

(define-public emacs-features
  (list
   (feature-emacs-completion)
   (feature-emacs-vertico)
   (feature-emacs-corfu #:corfu-doc-auto #t)
   (feature-emacs-all-the-icons)
   (feature-emacs-pdf-tools)
   (feature-emacs-tempel
    #:templates %extra-tempel-templates)
   (feature-emacs-graphviz)
   (feature-emacs-calendar
    #:calendar-date-style 'european
    #:diary-file "~/documents/diary")
   (feature-emacs-spelling
    #:flyspell-hooks '(org-mode-hook message-mode-hook bibtex-mode-hook)
    #:ispell-standard-dictionary "en_US")
   (feature-emacs-info)
   (feature-emacs-which-key)
   (feature-emacs-help)
   (feature-emacs-time
    #:display-time? #t
    #:display-time-24hr? #t
    #:display-time-date? #t
    #:world-clock-time-format "%R %Z"
    #:world-clock-timezones
    '(("Europe/London" "London")
      ("Europe/Madrid" "Madrid")
      ("Europe/Moscow" "Moscow")
      ("America/New_York" "New York")
      ("Australia/Sydney" "Sydney")
      ("Asia/Tokyo" "Tokyo")))
   (feature-emacs-dashboard
    #:show-on-startup? #f
    #:item-generators
    '((recents . dashboard-insert-recents)
      (bookmarks . dashboard-insert-bookmarks)
      (agenda . dashboard-insert-agenda)
      (registers . dashboard-insert-registers))
    #:items
    '((agenda . 7)
      (bookmarks . 7)
      (recents . 7))
    #:dashboard-agenda-prefix-format "%?-12:c"
    #:path-max-length 50)
   (feature-emacs-dired
    #:kill-when-opening-new-buffer? #t
    #:group-directories-first? #t)
   (feature-emacs-calc)
   (feature-emacs-tramp)
   (feature-emacs-re-builder)
   (feature-emacs-ace-window)
   (feature-emacs-project)
   (feature-emacs-keycast)
   (feature-emacs-input-methods
    #:default-input-method "spanish-keyboard")
   (feature-emacs-browse-url)
   (feature-emacs-webpaste)))
