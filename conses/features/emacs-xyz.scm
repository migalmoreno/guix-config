(define-module (conses features emacs-xyz)
  #:use-module (conses features web-browsers)
  #:use-module (conses utils)
  #:use-module (conses packages emacs-xyz)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (rde serializers elisp)
  #:use-module (rde packages)
  #:use-module (rde home services emacs-xyz)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 match)
  #:export (feature-emacs-appearance
            feature-emacs-whitespace
            feature-emacs-cursor
            feature-emacs-modus-themes
            feature-emacs-all-the-icons
            feature-emacs-completion
            feature-emacs-corfu
            feature-emacs-vertico
            feature-emacs-tempel
            feature-emacs-project
            feature-emacs-files
            feature-emacs-window
            feature-emacs-ibuffer
            feature-emacs-emms
            feature-emacs-pulseaudio-control
            feature-emacs-image
            feature-emacs-graphviz
            feature-emacs-ednc
            feature-emacs-calendar
            feature-emacs-bookmark
            feature-emacs-dashboard
            feature-emacs-markdown
            feature-emacs-org
            feature-emacs-org-roam
            feature-emacs-org-agenda
            feature-emacs-citar
            feature-emacs-pdf-tools
            feature-emacs-info
            feature-emacs-which-key
            feature-emacs-helpful
            feature-emacs-browse-url
            feature-emacs-eww
            feature-emacs-webpaste
            feature-emacs-time
            feature-emacs-dired
            feature-emacs-calc
            feature-emacs-tramp
            feature-emacs-battery
            feature-emacs-display-wttr
            feature-emacs-tab-bar
            feature-emacs-json
            feature-emacs-comint
            feature-emacs-shell
            feature-emacs-eshell
            feature-emacs-telega
            feature-emacs-smartparens
            feature-emacs-xref
            feature-emacs-re-builder
            feature-emacs-elisp
            feature-emacs-rainbow-delimiters
            feature-emacs-yaml
            feature-emacs-lang-web
            feature-emacs-polymode))


;;;
;;; UI
;;;

(define* (feature-emacs-appearance
          #:key
          (margin 8)
          (auto-theme? #t)
          (fringes #f)
          (mode-line-padding 4)
          (header-line-padding 4)
          (tab-bar-padding 4)
          (header-line-as-mode-line? #t))
  "Configure Emacs's appearance."
  (ensure-pred any-package? emacs-modus-themes)
  (ensure-pred number? margin)
  (ensure-pred boolean? auto-theme?)
  (ensure-pred maybe-number? fringes)
  (ensure-pred number? mode-line-padding)
  (ensure-pred number? header-line-padding)
  (ensure-pred number? tab-bar-padding)
  (ensure-pred boolean? header-line-as-mode-line?)

  (define emacs-f-name 'appearance)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to the appearance of Emacs."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((setq-default frame-title-format '("%b - Emacs"))
        (with-eval-after-load 'frame
          (add-to-list 'initial-frame-alist '(fullscreen . maximized))
          (set-frame-parameter (selected-frame) 'internal-border-width ,margin))
        (with-eval-after-load 'minions-autoloads
          (minions-mode))
        (with-eval-after-load 'minions
          (setq minions-mode-line-lighter ";"))
        (setq mode-line-misc-info
              (remove '(global-mode-string ("" global-mode-string)) mode-line-misc-info))
        ,@(if header-line-as-mode-line?
              '((setq-default header-line-format mode-line-format)
                (setq-default mode-line-format nil))
              '())
        (setq use-file-dialog nil)
        (setq use-dialog-box nil)
        (with-eval-after-load 'fringe
          (fringe-mode ,(or fringes 0)))
        (setq echo-keystrokes 0)
        (setq ring-bell-function 'ignore)
        (setq visible-bell nil)
        (setq mode-line-compact 'long)
        (fset 'yes-or-no-p 'y-or-n-p)
        (transient-mark-mode)
        (delete-selection-mode)
        (with-eval-after-load 'prog-mode
          (setq prettify-symbols-unprettify-at-point 'right-edge)
          (setq-default prettify-symbols-alist
                        '((":LOGBOOK:" . "")
                          (":PROPERTIES:" . "")
                          ("# -*-" . "")
                          ("-*-" . ""))))
        (add-hook 'text-mode-hook 'display-line-numbers-mode)
        (add-hook 'conf-mode-hook 'display-line-numbers-mode)
        (add-hook 'prog-mode-hook 'display-line-numbers-mode)
        (with-eval-after-load 'display-line-numbers
          (setq display-line-numbers-type t))
        (tooltip-mode -1))
      #:early-init
      `((setq inhibit-splash-screen t)
        (setq inhibit-startup-message t)
        (setq initial-scratch-message nil)
        (setq x-gtk-use-system-tooltips nil)
        (push '(menu-bar-lines . 0) default-frame-alist)
        (push '(tool-bar-lines . 0) default-frame-alist)
        (push (cons 'left-fringe ,(or fringes 0)) default-frame-alist)
        (push (cons 'right-fringe ,(or fringes 0)) default-frame-alist)
        (push '(vertical-scroll-bars) default-frame-alist)
        (push '(no-special-glyphs) default-frame-alist)
        (push '(undecorated) default-frame-alist)
        (push '(horizontal-scroll-bars) default-frame-alist)
        (push '(internal-border-width . ,margin) default-frame-alist))
      #:elisp-packages (list emacs-minions))))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             (emacs-auto-theme? . ,auto-theme?)
             (emacs-mode-line-padding . ,mode-line-padding)
             (emacs-header-line-padding . ,header-line-padding)
             (emacs-tab-bar-padding . ,tab-bar-padding)
             (emacs-margin . ,margin)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-whitespace
          #:key
          (global-modes '()))
  "Configure whitespace, a minor mode to visualize whitespace characters."
  (ensure-pred list? global-modes)

  (define emacs-f-name 'whitespace)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to whitespace-mode."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((add-hook 'before-save-hook 'delete-trailing-whitespace)
        (global-whitespace-mode)
        (with-eval-after-load 'whitespace
          (setq whitespace-style '(face tabs tab-mark))
          (setq whitespace-global-modes ',global-modes))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-cursor)
  "Configure the Emacs graphical cursor."

  (define emacs-f-name 'cursor)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to the cursor."
    (list
     (rde-elisp-configuration-service
      f-name
      config
      `((pixel-scroll-mode)
        (with-eval-after-load 'mouse
          (setq mouse-yank-at-point nil))
        (with-eval-after-load 'mwheel
          (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)
                                              ((control) . 1)))
          (setq mouse-wheel-progressive-speed nil)
          (setq mouse-wheel-follow-mouse t)
          (setq scroll-conservatively 100)
          (setq mouse-autoselect-window nil)
          (setq what-cursor-show-names t)
          (setq focus-follows-mouse t))
        (with-eval-after-load 'frame
          (setq-default cursor-in-non-selected-windows nil)
          (blink-cursor-mode 0))))))

  (feature
   (name f-name)
   (values `((emacs-cursor . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-modus-themes
          #:key
          (emacs-modus-themes emacs-modus-themes)
          (extra-after-load-theme-hooks '())
          (dark? #f))
  "Configure modus-themes, a pair of elegant and highly accessible
themes for Emacs."
  (ensure-pred any-package? emacs-modus-themes)
  (ensure-pred list? extra-after-load-theme-hooks)
  (ensure-pred boolean? dark?)

  (define emacs-f-name 'modus-themes)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to modus-themes."
    (define auto? (get-value 'emacs-auto-theme? config))
    (define mode-line-padding (get-value 'emacs-mode-line-padding config))
    (define header-line-padding (get-value 'emacs-header-line-padding config))
    (define tab-bar-padding (get-value 'emacs-tab-bar-padding config))

    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'modus-themes)
        (require 'configure-rde-keymaps)

        (defgroup configure-modus-themes nil
          "Minor nits related to `modus-themes'."
          :group 'configure)

        (defcustom configure-modus-themes-tab-bar-padding 1
          "The padding of the tab bar."
          :type 'number
          :group 'configure-modus-themes)

        (defcustom configure-modus-themes-header-line-padding 1
          "The padding of the header line."
          :type 'number
          :group 'configure-modus-themes)

        (defun configure-modus-themes-set-custom-faces (&optional _theme)
          "Set faces based on the current theme or THEME."
          (interactive)
          (modus-themes-with-colors
            (custom-set-faces
             `(tab-bar ((,class :background ,bg-header
                                :box (:line-width ,configure-modus-themes-tab-bar-padding
                                                  :color ,bg-header :style unspecified))))
             `(header-line ((,class :box (:line-width ,configure-modus-themes-header-line-padding
                                                      :color ,bg-header))))
             `(aw-leading-char-face ((,class :height 1.0 :foreground ,blue-alt-other))))))

        (defun configure-modus-themes--dark-theme-p (&optional theme)
          "Indicate if there is a curently-active dark THEME."
          (if theme
              (eq theme 'modus-vivendi)
            (eq (car custom-enabled-themes) 'modus-vivendi)))

        (defun configure-modus-themes-set-info-faces ()
          "Apply some extra appearance settings to `Info-mode' and `Info+-mode'."
          (interactive)
          (face-remap-add-relative 'default :inherit 'variable-pitch)
          (modus-themes-with-colors
            (custom-set-faces
             `(info-reference-item ((,class :background unspecified :foreground "#00d3d0")))
             `(info-function-ref-item ((,class :background unspecified :foreground "#b6a0ff")))
             `(info-quoted-name ((,class :foreground "#b0d6f5")))
             `(info-double-quoted-name ((,class :foreground "#b0d6f5")))
             `(info-xref ((,class :foreground "#00bcff" :underline t)))
             `(info-command-ref-item ((,class :background unspecified)))
             `(info-macro-ref-item ((,class :background unspecified)))
             `(info-variable-ref-item ((,class :background unspecified)))
             `(info-string ((,class :foreground "#79a8ff"))))))

        ,@(if (get-value 'mpv config)
              '((require 'mpv)
                (defun configure-modus-themes-change-mpv-theme (&optional theme)
                  "Set theme in current mpv process according to current system theme or THEME."
                  (interactive)
                  (if (or (and theme (configure-modus-themes--dark-theme-p theme))
                          (configure-modus-themes--dark-theme-p))
                      (progn
                        (mpv-set-property "background" "#000000")
                        (mpv-set-property "osd-color" "#ffffff"))
                    (mpv-set-property "background" "#ffffff")
                    (mpv-set-property "osd-color" "#323232")))
                (add-hook 'mpv-started-hook 'configure-modus-themes-change-mpv-theme)
                (add-hook 'modus-themes-after-load-theme-hook 'configure-modus-themes-change-mpv-theme))
              '())

        ,@(if (get-value 'nyxt-emacs config)
              '((require 'nyxt)
                (defun configure-modus-themes-load-nyxt-theme (&optional theme)
                  "Load theme in Nyxt according to current system theme or THEME."
                  (interactive)
                  (when nyxt-process
                    (if (or (and theme (configure-modus-themes--dark-theme-p theme))
                            (configure-modus-themes--dark-theme-p))
                        (nyxt-load-theme 'modus-vivendi)
                        (nyxt-load-theme 'modus-operandi))))
                (add-hook 'modus-themes-after-load-theme-hook 'configure-modus-themes-load-nyxt-theme))
              '())


        (setq configure-modus-themes-header-line-padding ,header-line-padding)
        (setq configure-modus-themes-tab-bar-padding ,tab-bar-padding)
        ,@(map (lambda (hook)
                 `(add-hook 'modus-themes-after-load-theme-hook ',hook))
               (append
                '(configure-modus-themes-set-custom-faces)
                 extra-after-load-theme-hooks))
        (modus-themes-load-themes)
        (with-eval-after-load 'modus-themes
          (define-key rde-toggle-map "t" 'modus-themes-toggle)
          (setq modus-themes-mode-line '(borderless))
          (setq modus-themes-operandi-color-overrides
                '((fg-window-divider-inner . "#ffffff")
                  (fg-window-divider-outer . "#ffffff")
                  (vertical-border . "#ffffff")))
          (setq modus-themes-vivendi-color-overrides
                '((fg-window-divider-inner . "#000000")
                  (fg-window-divider-outer . "#000000")
                  (vertical-border . "#000000")))
          (setq modus-themes-italic-constructs t)
          (setq modus-themes-italic-constructs t)
          (setq modus-themes-bold-constructs t)
          (setq modus-themes-mode-line '(borderless (padding ,mode-line-padding)))
          (setq modus-themes-org-blocks 'gray-background)
          (setq modus-themes-region '(bg-only no-extend))
          (setq modus-themes-markup '(intense))
          (setq modus-themes-mixed-fonts t)
          (setq modus-themes-subtle-line-numbers t)
          (setq modus-themes-headings (quote ((1 . (1.15))
                                              (2 . (1.1))
                                              (3 . (1.1))
                                              (4 . (1.0))
                                              (5 . (1.0))
                                              (6 . (1.0))
                                              (7 . (0.9))
                                              (8 . (0.9))))))
        ,@(if dark?
              '((modus-themes-load-vivendi))
              '((modus-themes-load-operandi)))
        ,@(if auto?
              '((require 'circadian)
                (add-hook 'circadian-after-load-theme-hook 'configure-org-update-faces)
                (add-hook 'circadian-after-load-theme-hook 'configure-modus-themes-set-theme-dependent-faces)
                (setq circadian-themes '((:sunrise . modus-operandi)
                                         (:sunset . modus-vivendi)))
                (circadian-setup))
            '()))
      #:elisp-packages (append
                         (if auto?
                             (list emacs-circadian-next)
                           '())
                         (if (get-value 'emacs-nyxt config)
                             (list (get-value 'emacs-nyxt config))
                             '())
                         (if (get-value 'emacs-mpv config)
                             (list (get-value 'emacs-mpv config))
                             '())
                         (list emacs-modus-themes
                               (get-value 'emacs-configure-rde-keymaps config)))
      #:summary "Modus Themes extensions"
      #:commentary "Customizations to Modus Themes, the elegant, highly legible Emacs themes.")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-modus-themes)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-all-the-icons
          #:key
          (emacs-all-the-icons emacs-all-the-icons))
  "Configure all-the-icons, a collection of fonts for Emacs."
  (ensure-pred any-package? emacs-all-the-icons)

  (define emacs-f-name 'all-the-icons)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to all-the-icons."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((when (and (display-graphic-p)
                   (not (find-font (font-spec :name "all-the-icons"))))
          (all-the-icons-install-fonts t))
        (with-eval-after-load 'all-the-icons
          (setq all-the-icons-scale-factor 1.0)
          (setq all-the-icons-default-adjust 0)
          (setq all-the-icons-octicon-scale-factor 0.9)))
      #:elisp-packages (list emacs-all-the-icons))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-all-the-icons)))
   (home-services-getter get-home-services)))


;;;
;;; Completion
;;;

(define* (feature-emacs-completion
          #:key
          (emacs-orderless emacs-orderless)
          (emacs-consult emacs-consult)
          (emacs-embark emacs-embark)
          (emacs-marginalia emacs-marginalia)
          (consult-initial-narrowing? #f))
  "Configure a set of modular completion system components for Emacs."
  (ensure-pred file-like? emacs-orderless)
  (ensure-pred file-like? emacs-consult)
  (ensure-pred file-like? emacs-embark)
  (ensure-pred file-like? emacs-marginalia)
  (ensure-pred boolean? consult-initial-narrowing?)

  (define emacs-f-name 'completion)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Emacs's completion system."
    (define ripgrep (@ (gnu packages rust-apps) ripgrep))
    (define fd (@ (gnu packages rust-apps) fd))

    (list
     (simple-service
      'emacs-completion-profile-service
      home-profile-service-type
      (list ripgrep fd))
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'xdg)
        (require 'consult)
        (defgroup configure-completion nil
          "Tweaks to the built-in Emacs completion via `consult'."
          :group 'configure)
        (autoload 'savehist-mode "savehist")
        (add-hook 'after-init-hook 'savehist-mode)
        (with-eval-after-load 'savehist
          (setq savehist-file (expand-file-name "emacs/history" (or (xdg-cache-home) "~/.cache")))
          (add-hook 'after-init-hook 'savehist-save)
          (setq history-length 1000)
          (setq history-delete-duplicates t))
        (require 'orderless)
        (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode)
        (with-eval-after-load 'minibuffer
          (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
          (setq enable-recursive-minibuffers t)
          (setq completion-in-region-function (lambda (&rest args)
                                                (apply (if vertico-mode
                                                           'consult-completion-in-region
                                                           'completion--in-region)
                                                       args)))
          (setq completion-category-defaults nil)
          (setq read-file-name-completion-ignore-case t)
          (setq read-buffer-completion-ignore-case t)
          (setq read-extended-command-predicate 'command-completion-default-include-p)
          (setq completion-cycle-threshold t)
          (setq completion-styles '(orderless basic))
          (setq completion-category-overrides '((file (styles basic partial-completion)))))
        ,@(if (get-value 'emacs-all-the-icons config)
              '((with-eval-after-load 'all-the-icons-completion-autoloads
                  (all-the-icons-completion-mode))
                (with-eval-after-load 'all-the-icons
                  (add-hook 'marginalia-mode-hook 'all-the-icons-completion-marginalia-setup)))
              '())
        (advice-add 'completing-read-multiple :filter-args 'configure-completion-crm-indicator)
        (autoload 'embark-pp-eval-defun "embark")
        (define-key global-map (kbd "C-.") 'embark-act)
        (define-key global-map (kbd "M-.") 'embark-dwim)
        (define-key help-map "b" 'embark-bindings)
        (with-eval-after-load 'embark
          (setq prefix-help-command 'embark-prefix-help-command)
          (add-to-list 'display-buffer-alist
                       `(,(rx bos "*Embark Collect " (or "Live" "Completions") "*")
                         nil
                         (window-parameters (mode-line-format . none))))
          (setq embark-indicators '(embark-minimal-indicator))
          (setq embark-prompter 'embark-keymap-prompter))

        ,@(if consult-initial-narrowing?
              '((defcustom configure-completion-initial-narrow-alist '()
                  "Alist of MODE . KEY to present an initial completion narrowing via `consult'."
                  :group 'configure-completion
                  :type 'list)
                (defun configure-completion--mode-buffers (&rest modes)
                  "Return a list of buffers that are derived from MODES in `buffer-list'."
                  (cl-remove-if-not
                   (lambda (buffer)
                     (with-current-buffer buffer
                       (cl-some 'derived-mode-p modes)))
                   (buffer-list)))
                (defun configure-completion-initial-narrow ()
                  "Set initial narrow source for buffers under a specific mode."
                  (let* ((buffer-mode-assoc configure-completion-initial-narrow-alist)
                         (key (and (eq this-command 'consult-buffer)
                                   (or (alist-get (buffer-local-value
                                                   'major-mode (window-buffer (minibuffer-selected-window)))
                                                  buffer-mode-assoc)
                                       (cdr (cl-find-if (lambda (mode)
                                                          (with-current-buffer
                                                              (window-buffer (minibuffer-selected-window))
                                                            (derived-mode-p (car mode))))
                                                        buffer-mode-assoc))))))
                    (when key
                      (setq unread-command-events (append unread-command-events (list key 32))))))
                (add-hook 'minibuffer-setup-hook 'configure-completion-initial-narrow)
                (with-eval-after-load 'consult
                  (setq consult-narrow-key (kbd "C-="))
                  (setq consult-widen-key (kbd "C--"))))
              '())

        (defun configure-completion-crm-indicator (args)
          "Display a discernible indicator for `completing-read-multiple'."
          (cons (concat "[CRM] " (car args)) (cdr args)))

        ,@(if (get-value 'emacs-project config)
              '((require 'project)
                (let ((map project-prefix-map))
                  (define-key map "F" 'consult-find)
                  (define-key map "R" 'configure-project-ripgrep))
                (add-to-list 'project-switch-commands '(consult-find "Find file consult"))
                (add-to-list 'project-switch-commands '(configure-project-ripgrep "Search for regexp with rg")))
              '())
        (define-key global-map (kbd "M-y") 'consult-yank-pop)
        (define-key global-map (kbd "C-s") 'consult-line)
        (define-key ctl-x-4-map "b" 'consult-buffer-other-window)
        (define-key goto-map "a" 'consult-org-agenda)
        (define-key goto-map "i"  'consult-imenu)
        (define-key goto-map "g" 'consult-goto-line)
        (define-key search-map "r" 'consult-ripgrep)
        (define-key search-map "s" 'consult-find)
        (define-key search-map "l" 'consult-line)
        (define-key search-map "L" 'consult-line-multi)
        (define-key search-map "f" 'consult-recent-file)
        (define-key ctl-x-map "b" 'consult-buffer)
        (define-key help-map "a" 'consult-apropos)
        (define-key ctl-x-map (kbd "M-:") 'consult-complex-command)
        (define-key isearch-mode-map (kbd "M-s e") 'consult-isearch-history)
        (define-key minibuffer-mode-map (kbd "C-c C-r") 'consult-history)
        (with-eval-after-load 'consult
          (setq consult-find-args "fd . -H -F -t f -E .git node_modules .cache")
          (consult-customize
           consult--source-buffer consult-buffer
           consult-bookmark consult--source-bookmark
           consult-recent-file consult--source-recent-file
           consult--source-project-buffer
           consult--source-hidden-buffer
           :preview-key (kbd "M-.")))
        (autoload 'marginalia-mode "marginalia")
        (marginalia-mode))
      #:elisp-packages (append
                        (list emacs-orderless
                              emacs-embark
                              emacs-consult
                              emacs-marginalia)
                        (if (get-value 'emacs-all-the-icons config)
                            (list emacs-all-the-icons-completion)
                            '()))
      #:commentary "Adds consult buffer sources for various buffer types to be selected via narrowing keys.")))

  (feature
   (name f-name)
   (values (append
            `((,f-name . #t)
              (emacs-consult-initial-narrowing? . ,consult-initial-narrowing?))
             (make-feature-values emacs-orderless
                                  emacs-embark
                                  emacs-consult
                                  emacs-marginalia)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-corfu
          #:key
          (emacs-corfu emacs-corfu)
          (emacs-kind-icon emacs-kind-icon)
          (corfu-doc? #f))
  "Configure Corfu, an overlay for the Emacs's built-in CAPF."
  (ensure-pred file-like? emacs-corfu)
  (ensure-pred file-like? emacs-kind-icon)
  (ensure-pred boolean? corfu-doc?)

  (define emacs-f-name 'corfu)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Corfu."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((autoload 'global-corfu-mode "corfu")
        (global-corfu-mode)
        (autoload 'corfu-history-mode "corfu-history")
        (corfu-history-mode)
        ,@(if corfu-doc?
              '((add-hook 'corfu-mode-hook 'corfu-doc-mode))
              '())
        (with-eval-after-load 'corfu
          (setq corfu-auto t)
          (setq corfu-cycle t)
          (setq corfu-auto-prefix 2)
          (setq corfu-auto-delay 0.2)
          (setq corfu-echo-documentation '(1.0 . 0.2))
          (setq corfu-quit-at-boundary t)
          (setq corfu-preselect-first nil)
          (let ((map corfu-map))
            (define-key map "\t" 'corfu-next)
            (define-key map (kbd "<tab>") 'corfu-next)
            (define-key map (kbd "<backtab>") 'corfu-previous)
            (define-key map (kbd "S-TAB") 'corfu-previous)
            ,@(if corfu-doc?
                  '((define-key map (kbd "M-p") 'corfu-doc-scroll-down)
                    (define-key map (kbd "M-n") 'corfu-doc-scroll-up)
                    (define-key map (kbd "M-d") 'corfu-doc-toggle))
                  '()))
          (require 'kind-icon)
          (setq kind-icon-default-face 'corfu-default)
          (add-to-list 'corfu-margin-formatters 'kind-icon-margin-formatter)
          (set-face-attribute 'corfu-default nil :inherit 'fixed-pitch)))
      #:elisp-packages (append
                         (list emacs-corfu
                               emacs-kind-icon)
                         (if corfu-doc?
                          (list emacs-corfu-doc)
                          '())))))

  (feature
   (name f-name)
   (values (append
             `((,f-name . ,emacs-corfu))
             (if corfu-doc?
                 `((emacs-corfu-doc . ,emacs-corfu-doc))
                 '())))
   (home-services-getter get-home-services)))

(define* (feature-emacs-vertico
          #:key
          (emacs-vertico emacs-vertico))
  "Configure Vertico, a performant and minimalistic vertical
completion UI based on the default Emacs completion system."
  (ensure-pred file-like? emacs-vertico)

  (define emacs-f-name 'vertico)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Vertico."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((autoload 'vertico-mode "vertico")
        (vertico-mode)
        (with-eval-after-load 'vertico
          (let ((map vertico-map))
            (define-key map "?" 'minibuffer-completion-help)
            (define-key map (kbd "M-RET") 'minibuffer-force-complete-and-exit)
            (define-key map (kbd "M-TAB") 'minibuffer-complete))
          (setq vertico-cycle t)
          (setq vertico-resize 'grow-only)))
      #:elisp-packages (list emacs-vertico))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-vertico)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-tempel
          #:key
          (emacs-tempel emacs-tempel)
          (tempel-capf-hooks '(prog-mode-hook
                               text-mode-hook
                               conf-mode-hook
                               fundamental-mode-hook))
          (default-templates? #t)
          (templates '()))
  "Configure TempEL, a simple-template mechanism for Emacs."
  (ensure-pred any-package? emacs-tempel)
  (ensure-pred boolean? default-templates?)

  (define emacs-f-name 'tempel)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to TempEL."
    (append
     (if default-templates?
         (list
          (service
           home-emacs-tempel-service-type
           (home-emacs-tempel-configuration
            (templates
             `(,#~"fundamental-mode"
                  (today (format-time-string "%Y-%m-%d"))
                  (copyright
                   (if (derived-mode-p 'lisp-data-mode 'clojure-data-mode 'scheme-mode)
                       ";;"
                       comment-start)
                   (if (string-suffix-p " " comment-start) "" " ")
                   "Copyright ©" (format-time-string "%Y") " "
                   (format "%s <%s>" user-full-name user-mail-address)
                   comment-end))))))
         '())
     (list
      (simple-service
       'emacs-tempel-service
       home-emacs-tempel-service-type
       templates)
      (rde-elisp-configuration-service
       emacs-f-name
       config
       `((defun configure-tempel-setup-capf ()
                "Add the TempEL Capf `tempel-complete'."
                (setq-local completion-at-point-functions
                            (cons 'tempel-complete
                                  completion-at-point-functions)))

         (mapcar (lambda (hook)
                   (add-hook hook 'configure-tempel-setup-capf))
                 ',tempel-capf-hooks)
         (define-key global-map (kbd "M-+") 'tempel-complete)
         (define-key global-map (kbd "M-*") 'tempel-insert)
         (with-eval-after-load 'tempel
           (setq tempel-trigger-prefix "<")))
       #:elisp-packages (list emacs-tempel)
       #:summary "Extensions to TempEL"
       #:commentary "Provide extensions to TempEL, a simple templating system for Emacs."))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-tempel)))
   (home-services-getter get-home-services)))


;;;
;;; Environment
;;;

(define* (feature-emacs-project)
  "Configure project.el, a library to perform operations
on the current project."

  (define emacs-f-name 'project)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to the project API."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'cl-lib))
        (defgroup configure-project nil
          "Custom `project.el' enhancements."
          :group 'configure)

        (defcustom configure-project-dominating-files '()
          "List of root files that indicate a directory is project."
          :group 'configure-project
          :type '(repeat string))

        (cl-defmethod project-root ((project (head explicit)))
          "Determine the PROJECT root."
          (cdr project))

        (defun configure-project-custom-root (dir)
          "Search in project's DIR for a set of project dominating files."
          (let* ((files configure-project-dominating-files)
                 (root (cl-find-if (lambda (file)
                                     (locate-dominating-file dir file))
                                   files)))
            (when root
              (cons 'explicit (locate-dominating-file dir root)))))

        (defun configure-project-ripgrep ()
          "Run `consult-ripgrep' in the current project root."
          (interactive)
          (when-let ((default-dir (project-root (project-current t))))
            (consult-ripgrep default-dir)))

        (defun configure-project-org-capture ()
          "Run `org-capture' in the current project root."
          (interactive)
          (when-let ((default-dir (project-root (project-current t))))
            (dir-locals-read-from-dir default-dir)
            (org-capture)))

        (defun configure-project-compile (&optional comint)
          "Compile the current project, where if COMINT, the compilation buffer will be in Comint mode."
          (interactive "P")
          (let ((default-directory (project-root (project-current t)))
                (compilation-buffer-name-function
                 (or project-compilation-buffer-name-function
                     compilation-buffer-name-function)))
            (call-interactively 'compile nil (and comint (vector (list 4))))))

        (setq configure-project-dominating-files '(".project.el" ".dir-locals.el"))
        (add-hook 'project-find-functions 'project-try-vc)
        (add-hook 'project-find-functions 'configure-project-custom-root)
        (advice-add 'project-compile :override 'configure-project-compile)
        (with-eval-after-load 'project
          (setq project-switch-use-entire-map t)))
      #:elisp-packages (if (get-value 'emacs-consult config)
                           (list (get-value 'emacs-consult config))
                           '())
      #:summary "Custom project.el utilities."
      #:commentary "Enhancements over the project.el API.")))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


;;;
;;; Structures
;;;

(define* (feature-emacs-files)
  "Configure Emacs's file- and directory-handling facilities."

  (define emacs-f-name 'files)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to file handling in Emacs."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load 'files
          (setq backup-directory-alist
                `((".*" . "/tmp")))
          (setq auto-save-file-name-transforms
                `((".*" "/tmp" t)))
          (setq auto-save-no-message t)
          (setq create-lockfiles nil)
          (setq delete-old-versions t)
          (setq kept-new-versions 3)
          (setq kept-old-versions 2)
          (setq version-control t)
          (setq remote-file-name-inhibit-cache nil)
          (setq safe-local-variable-values '((after-save-hook . (org-babel-tangle))
                                             (ispell-dictionary . "en_GB"))))
        (setq custom-file (expand-file-name (format "emacs/custom-%s.el" (user-uid)) (xdg-cache-home)))
        (with-eval-after-load 'custom
          (setq custom-safe-themes t)
          (when (file-exists-p custom-file)
            (load custom-file)))
        (add-hook 'after-init-hook 'recentf-mode)
        (with-eval-after-load 'recentf
          (add-hook 'after-init-hook 'recentf-save-list)
          (setq recent-save-file (expand-file-name "emacs/recentf" (or (xdg-cache-home) "~/.cache"))))
        (global-auto-revert-mode)
        (with-eval-after-load 'autorevert
          (setq auto-revert-remote-files nil))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-window
          #:key
          (emacs-ace-window emacs-ace-window))
  "Configure tooling related to Emacs's window tree mechanism."
  (ensure-pred file-like? emacs-ace-window)

  (define emacs-f-name 'window)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Emacs's windows."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load 'window
          (setq even-window-sizes nil)
          (setq split-window-keep-point t))
        (setq window-divider-default-right-width ,(get-value 'emacs-margin config))
        (window-divider-mode)
        (winner-mode)
        ,@(if (get-value 'emacs-exwm config)
              '((with-eval-after-load 'exwm-autoloads
                  (exwm-input-set-key (kbd "M-o") 'ace-window)))
              '((define-key global-map (kbd "M-o") 'ace-window)))
        (with-eval-after-load 'ace-window
          (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
          (setq aw-background nil)
          (setq aw-scope 'frame)
          (setq aw-ignore-current nil)
          (setq aw-display-mode-overlay nil)))
      #:elisp-packages (list emacs-ace-window))))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             (emacs-ace-window . ,emacs-ace-window)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-ibuffer)
  "Configure IBuffer, an Emacs library that allows to
operate on buffers like Dired."

  (define emacs-f-name 'ibuffer)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to buffer handling in Emacs."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((define-key ctl-x-map (kbd "C-b") 'ibuffer)
        (with-eval-after-load 'ibuffer
          (setq ibuffer-expert t))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


;;;
;;; Multimedia
;;;

(define* (feature-emacs-emms
          #:key
          (emacs-emms emacs-emms)
          (emms-info-method 'emms-info-libtag)
          (emms-media-dir "~/music"))
  "Configure the Emacs MultiMedia System."
  (ensure-pred file-like? emacs-emms)
  (ensure-pred symbol? emms-info-method)
  (ensure-pred path? emms-media-dir)

  (define emacs-f-name 'emms)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to EMMS."
    (define atomicparsley (@ (gnu packages video) atomicparsley))

    (append
     (list
      (simple-service
       'home-audio-packages
       home-profile-service-type
       (list atomicparsley))
      (rde-elisp-configuration-service
       emacs-f-name
       config
       `((eval-when-compile
           (require 'emms)
           (require 'emms-browser))
         (require 'ytdl)

         (defun configure-emms-download-track ()
           "Download EMMS track at point using `ytdl'."
           (interactive)
           (emms-playlist-ensure-playlist-buffer)
           (with-current-emms-playlist
             (let* ((dl-type (ytdl--get-download-type))
                    (track (emms-playlist-track-at))
                    (title (emms-track-get track 'info-title))
                    (source (emms-track-get track 'name)))
               (if (equal (emms-track-get track 'type) 'url)
                   (ytdl--download-async
                    source
                    (expand-file-name title (ytdl--eval-field (nth 1 dl-type)))
                    (ytdl--eval-list (ytdl--eval-field (nth 2 dl-type)))
                    'ignore
                    (car dl-type))
                 (error "Track `%s' is not a remote track to download" title)))))

         (defun configure-emms-library-load ()
           "Load an EMMS playlist with local music files and move to it."
           (interactive)
           (ignore-errors
             (if (not emms-playlist-buffer)
                 (progn
                   (emms-add-directory-tree ,emms-media-dir)
                   (emms-playlist-mode-go))
               (with-current-emms-playlist
                 (unless (emms-playlist-selected-track)
                   (emms-add-directory-tree ,emms-media-dir))
                 (emms-playlist-mode-go)))))

         (defun configure-emms-source-track (url title &optional length play)
           "Append and/or PLAY track with URL, TITLE, and LENGTH to the current EMMS playlist."
           (interactive "sURL: \nsTitle: \nP")
           (if length
               (emms-add-configure-emms-track url title length)
             (emms-add-configure-emms-track url title nil))
           (when play
             (emms-stop)
             (emms-playlist-current-select-last)
             (emms-start)))

         (defun configure-emms-toggle-random-repeat ()
           "Toggle both the random and repeat state for the current EMMS playlist."
           (interactive)
           (emms-toggle-random-playlist)
           (if (and emms-repeat-track emms-random-playlist)
               (progn
                 (setq emms-repeat-track nil)
                 (message "Will play the tracks randomly and repeat the current track"))
             (setq emms-repeat-track t)
             (message "Will play the tracks sequentially and repeat the current track")))

         (defun configure-emms-seek-to-beginning ()
           "Seek to beginning of current EMMS track."
           (interactive)
           (emms-seek-to 0))

         (defun configure-emms-next ()
           "Move to the next track depending on the current playlist state."
           (interactive)
           (if emms-random-playlist
               (emms-random)
             (emms-next)))

         (defun configure-emms-previous ()
           "Move to the previous track depending on the current playlist state."
           (interactive)
           (if emms-random-playlist
               (emms-random)
             (emms-next)))

         (define-emms-source configure-emms-track (url title &optional length)
           (let ((emms-track (emms-track 'url url)))
             (emms-track-set emms-track 'info-title title)
             (when length
               (emms-track-set emms-track 'info-playing-time length))
             (emms-playlist-insert-track emms-track)))

         (let ((map mode-specific-map))
           (define-key map "eb" 'emms-browser)
           (define-key map "eh" 'emms-history-save)
           (define-key map "eq" 'emms-stop)
           (define-key map "es" 'emms-toggle-random-playlist)
           (define-key map "eS" 'emms-seek-to)
           (define-key map (kbd "e SPC") 'emms-pause)
           (define-key map "er" 'emms-toggle-repeat-track)
           (define-key map "eR" 'configure-emms-toggle-random-repeat)
           (define-key map "el" 'configure-emms-library-load)
           (define-key map "en" 'configure-emms-next)
           (define-key map "ep" 'configure-emms-previous)
           (define-key map "ea" 'configure-emms-seek-to-beginning))
         (with-eval-after-load 'emms
           (require 'emms-setup)
           (require ',emms-info-method)
           (emms-all)
           (emms-default-players)
           (emms-toggle-random-playlist)
           (emms-smart-browse)
           (define-key emms-playlist-mode-map "m" 'configure-emms-download-track)
           (define-key dired-mode-map "e" 'emms-play-dired)
           (emms-browser-make-filter
            "all-files" (emms-browser-filter-only-type 'file))
           (emms-browser-make-filter
            "last-week" (emms-browser-filter-only-recent 7))
           (let ((mp3-function (assoc "mp3" emms-tag-editor-tagfile-functions)))
             (add-to-list
              'emms-tag-editor-tagfile-functions
              `("aac" ,(cadr mp3-function) ,(caddr mp3-function)))
             (add-to-list
              'emms-tag-editor-tagfile-functions
              `("m4a" ,(executable-find "AtomicParsley")
                ((info-artist . "--artist")
                 (info-title . "--title")
                 (info-album . "--album")
                 (info-tracknumber . "--tracknum")
                 (info-year . "--year")
                 (info-genre . "--genre")
                 (info-note . "--comment")
                 (info-albumartist . "--albumArtist")
                 (info-composer . "--composer")))))
           (setq emms-playlist-buffer-name "*EMMS Playlist*")
           (setq emms-history-file (expand-file-name "emacs/emms-history" (or (xdg-cache-home) "~/.cache")))
           (setq emms-seek-seconds 15)
           (setq emms-source-file-default-directory ,emms-media-dir)
           (setq emms-playlist-mode-center-when-go t)
           (setq emms-repeat-playlist t)
           (setq emms-info-functions '(,emms-info-method))
           (setq emms-browser-covers 'emms-browser-cache-thumbnail-async)
           (setq emms-mode-line-format "%s")
           (setq emms-browser-thumbnail-small-size 64)
           (setq emms-browser-thumbnail-medium-size 128)
           (setq emms-mode-line-icon-enabled-p nil)
           ,@(if (get-value 'mpv config)
                 '((require 'emms-player-mpv)
                   (setq emms-player-list '(emms-player-mpv))
                   (add-to-list 'emms-player-mpv-parameters "--ytdl-format=best")
                   (add-to-list 'emms-player-mpv-parameters "--force-window=no"))
                 '())))
       #:elisp-packages (append
                         (if (get-value 'emacs-ytdl config)
                             (list (get-value 'emacs-ytdl config))
                             '())
                         (list emacs-emms
                               (get-value 'emacs-configure-rde-keymaps config)))
       #:summary "Extensions for EMMS"
       #:commentary "Extensions for EMMS, the Emacs MultiMedia System."))
     (if (get-value 'nyxt config)
         (list
          (rde-nyxt-configuration-service
           emacs-f-name
           config
           `((define-command emms-source-track ()
               "Source the current buffer as an EMMS track."
               (let ((url (quri:render-uri (url (current-buffer)))))
                 (eval-in-emacs
                  `(configure-emms-source-track
                    ,url
                    ,(title (current-buffer))
                    t))
                 (echo "Starting to play ~a in EMMS" url)))
             (define-key *rde-keymap* "C-c e" 'emms-source-track))))
         '())))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-emms)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-pulseaudio-control
          #:key
          (emacs-pulseaudio-control emacs-pulseaudio-control)
          (volume-step "5%"))
  "Configure pulseaudio-control, PulseAudio integration for Emacs."
  (ensure-pred file-like? emacs-pulseaudio-control)
  (ensure-pred string? volume-step)

  (define emacs-f-name 'pulseaudio-control)
  (define f-name (symbol-append 'emacs emacs-f-name))

  (define (get-home-services config)
    "Return home services related to PulseAudio Control."
    (define pactl (file-append (@ (gnu packages pulseaudio) pulseaudio) "/bin/pactl"))

    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load 'pulseaudio-control-autoloads
          (pulseaudio-control-default-keybindings))
        ,@(if (get-value 'emacs-exwm config)
              '((with-eval-after-load 'exwm
                  (exwm-input-set-key (kbd "s-<next>") 'pulseaudio-control-decrease-sink-volume)
                  (exwm-input-set-key (kbd "s-<prior>") 'pulseaudio-control-increase-sink-volume)))
              '())
        (with-eval-after-load 'pulseaudio-control
          (define-key pulseaudio-control-map "L" 'pulseaudio-control-toggle-sink-input-mute-by-index)
          ,@(if (get-value 'emacs-all-the-icons config)
                '((eval-when-compile
                   (require 'all-the-icons))
                  (with-eval-after-load 'all-the-icons
                    (let ((all-the-icons-default-adjust -0.15))
                      (setq pulseaudio-control-sink-mute-string
                            (all-the-icons-material "volume_off" :height 1))
                      (setq pulseaudio-control-sink-volume-strings
                            (list
                             (all-the-icons-material "volume_mute" :height 1)
                             (all-the-icons-material "volume_down" :height 1)
                             (all-the-icons-material "volume_up" :height 1)))
                      (setq pulseaudio-control-source-mute-string
                            (all-the-icons-material "mic_off" :height 1))
                      (setq pulseaudio-control-source-volume-strings
                            (list
                             (all-the-icons-material "mic_none" :height 1)
                             (all-the-icons-material "mic" :height 1))))))
                '())
          (setq pulseaudio-control-pactl-path ,pactl)
          (setq pulseaudio-control--volume-maximum '(("percent" . 100)
                                                     ("decibels" . 10)
                                                     ("raw" . 98000)))
          (setq pulseaudio-control-volume-step ,volume-step)
          (setq pulseaudio-control-volume-verbose nil)
          (pulseaudio-control-default-sink-mode)
          (pulseaudio-control-default-source-mode)
          (pulseaudio-control-display-mode)))
      #:elisp-packages (append (list emacs-pulseaudio-control)
                               (if (get-value 'emacs-all-the-icons config)
                                   (list (get-value 'emacs-all-the-icons config))
                                   '())))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-pulseaudio-control)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-image)
  "Configure Image mode, Emacs's library to view image files."

  (define emacs-f-name 'image)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to image-mode."
    (define imagemagick (@ (gnu packages imagemagick) imagemagick))

    (list
     (simple-service
      'image-packages
      home-profile-service-type
      (list imagemagick))
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load 'image-mode
          (define-key image-mode-map "q" 'image-kill-buffer)
          (setq image-user-external-converter t))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-graphviz
          #:key
          (emacs-graphviz-dot-mode emacs-graphviz-dot-mode))
  "Configure Graphviz, the open source graph visualization software."
  (ensure-pred any-package? emacs-graphviz-dot-mode)

  (define emacs-f-name 'graphviz)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Graphviz."
    (define graphviz (@ (gnu packages graphviz) graphviz))

    (list
     (simple-service
      'emacs-graphviz-profile-service
      home-profile-service-type
      (list graphviz))
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((defun configure-graphviz-fix-inline-images ()
          "Display inline images automatically."
          (interactive)
          (when org-inline-image-overlays
            (org-redisplay-inline-images)))

        (add-hook 'org-babel-after-execute-hook 'configure-graphviz-fix-inline-images)
        (require 'graphviz-dot-mode)
        (with-eval-after-load 'org
          (require 'ob-dot))
        (with-eval-after-load 'ob-dot
          (add-to-list 'org-babel-default-header-args:dot '(:cmdline . "-Kdot -Tpng"))))
      #:elisp-packages (list emacs-graphviz-dot-mode))))

  (feature
   (name f-name)
   (values `((,f-name . ,f-name)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-ednc
          #:key
          (emacs-ednc emacs-ednc)
          (notifications-icon #f)
          (ednc-key "n"))
  "Configure the Emacs Desktop Notification Center (EDNC)."
  (ensure-pred any-package? emacs-ednc)
  (ensure-pred maybe-path? notifications-icon)
  (ensure-pred string? ednc-key)

  (define emacs-f-name 'ednc)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to EDNC."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((defvar rde-ednc-map nil
           "Map to bind `ednc' commands under.")
        (define-prefix-command 'rde-ednc-map)
        (defun configure-ednc--notify ()
          "Display the latest EDNC notification."
          (when (ednc-notifications)
            (ednc-format-notification (car (ednc-notifications)))))

        (defun configure-ednc-show-notification-log ()
          "Switch to the EDNC log buffer."
          (interactive)
          (when (bufferp (get-buffer ednc-log-name))
            (switch-to-buffer ednc-log-name)))

        (defun configure-ednc-close-last-notification ()
          "Close the latest notification provided by the notification daemon."
          (interactive)
          (when-let* ((notification (car (ednc-notifications))))
            (ednc--close-notification notification 2)))

        (defun configure-ednc-close-all-notifications ()
          "Dismiss all EDNC notifications."
          (interactive)
          (mapc 'ednc-dismiss-notification (cdr ednc--state)))

        (defun configure-ednc-update-notifications (&rest _)
          "Update the display of EDNC notifications."
          (interactive)
          (force-mode-line-update t))

        (with-eval-after-load 'ednc-autoloads
          (add-hook 'after-init-hook 'ednc-mode))
        (add-hook 'ednc-notification-presentation-functions 'configure-ednc-update-notifications)
        (add-hook 'ednc-notification-presentation-functions 'ednc--update-log-buffer)
        (with-eval-after-load 'notifications
          ,@(if notifications-icon
                `((setq notifications-application-icon ,notifications-icon))
                '()))
        (define-key rde-app-map (kbd ,ednc-key) 'rde-ednc-map)
        (let ((map rde-ednc-map))
          (define-key map "c" 'configure-ednc-close-last-notification)
          (define-key map "l" 'configure-ednc-show-notification-log)
          (define-key map "d" 'configure-ednc-close-all-notifications)))
      #:elisp-packages (list emacs-ednc))))

  (feature
   (name f-name)
   (values `((,f-name . ,f-name)))
   (home-services-getter get-home-services)))


;;;
;;; Task Management
;;;

(define* (feature-emacs-calendar
          #:key
          (diary-file "~/.cache/emacs/diary")
          (calendar-date-style 'european)
          (week-numbers? #f))
  "Configure the calendar and diary facilities in Emacs."
  (ensure-pred path? diary-file)
  (ensure-pred symbol? calendar-date-style)
  (ensure-pred boolean? week-numbers?)

  (define emacs-f-name 'calendar)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to the Emacs Calendar/Diary."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'configure-rde-keymaps)
        (define-key rde-app-map "c" 'calendar)
        (with-eval-after-load 'calendar
          (setq diary-file ,diary-file)
          (setq calendar-week-start-day 1)
          (setq calendar-view-diary-initially-flag t)
          (setq calendar-date-style ',calendar-date-style)
          (setq calendar-mark-diary-entries-flag t)
          ,@(if week-numbers?
                '((setq calendar-intermonth-header
                        (propertize "WK" 'font-lock-face
                                    'font-lock-function-name-face))
                  (setq calendar-intermonth-text
                        '(propertize
                          (format "%2d"
                                  (car
                                   (calendar-iso-from-absolute
                                    (calendar-absolute-from-gregorian
                                     (list month day year)))))
                          'font-lock-face 'font-lock-function-name-face)))
                '()))
        (appt-activate 1)
        (let ((map rde-app-map))
          (define-key map "Aa" 'appt-add)
          (define-key map "Ad" 'appt-delete))
        (with-eval-after-load 'appt
          (setq appt-display-format 'echo)
          (setq appt-audible nil)
          (setq appt-message-warning-time 10)
          (setq appt-display-interval 2)
          (setq appt-display-diary nil))
        ,@(if (get-value 'emacs-auto-theme? config)
              '((defun configure-calendar--get-geolocation ()
                  "Fetch the current location's coordinates through Mozilla's Geolocation API."
                  (with-current-buffer
                      (ignore-errors
                        (url-retrieve-synchronously
                         "https://location.services.mozilla.com/v1/geolocate?key=geoclue" t))
                    (goto-char (point-min))
                    (re-search-forward (rx bol "\n") nil t)
                    (delete-region (point) (point-min))
                    (let* ((location (car (cdr (json-parse-string (buffer-string) :object-type 'plist))))
                           (latitude (plist-get location :lat))
                           (longitude (plist-get location :lng)))
                      (cons longitude latitude))))

                (with-eval-after-load 'solar
                  (setq calendar-longitude (car (configure-calendar--get-geolocation)))
                  (setq calendar-latitude (cdr (configure-calendar--get-geolocation)))))
              '()))
      #:elisp-packages (list (get-value 'emacs-configure-rde-keymaps config)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-bookmark
          #:key
          (bookmark-file "~/.cache/emacs/bookmarks"))
  "Configure the built-in Emacs bookmark mechanism."
  (ensure-pred path? bookmark-file)

  (define emacs-f-name 'bookmark)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to bookmark.el."
    (append
     (list
      (rde-elisp-configuration-service
       emacs-f-name
       config
       `((with-eval-after-load 'bookmark
           (setq bookmark-save-flag 1)
           (setq bookmark-set-fringe-mark nil)
           (setq bookmark-default-file ,bookmark-file))
         ,@(if (get-value 'emacs-embark config)
               '((with-eval-after-load 'embark
                   (define-key embark-bookmark-map "c" 'configure-browse-url-alt-bookmark-jump)))
               '()))))
     (if (get-value 'nyxt config)
         (list
          (rde-nyxt-configuration-service
           f-name
           config
           `((define-command save-as-emacs-bookmark ()
               "Save current page in Nyxt as a bookmark record in Emacs."
               (eval-in-emacs
                `(let ((bookmark-make-record-function
                        (lambda ()
                          (configure-browse-url-bookmark-make-record
                           ,(quri:render-uri (url (current-buffer)))
                           ,(title (current-buffer))))))
                   (bookmark-set)))
               (echo "Bookmark stored"))
             (define-key *rde-keymap* "C-c r" 'save-as-emacs-bookmark))))
         '())))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define (file-like-or-path-or-symbol-or-boolean? x)
  (or (boolean? x) (symbol? x) (file-like-or-path? x)))

(define* (feature-emacs-dashboard
          #:key
          (emacs-dashboard emacs-dashboard)
          (page-separator "\n\n")
          (logo-title "Welcome to Emacs!")
          (center-content? #t)
          (set-init-info? #t)
          (init-info #f)
          (items #f)
          (item-generators #f)
          (item-shortcuts #f)
          (item-names #f)
          (set-heading-icons? #f)
          (heading-icons #f)
          (set-file-icons? #f)
          (navigator-buttons #f)
          (banner 'logo)
          (banner-max-height 0)
          (banner-max-width 0)
          (org-agenda-weekly? #t)
          (org-agenda-prefix-format #f)
          (set-footer? #t)
          (footer #f)
          (footer-messages #f)
          (footer-icon #f)
          (bookmarks-show-base? #f)
          (recentf-show-base? #f)
          (projects-backend 'project-el)
          (projects-show-base? #f)
          (path-style #f)
          (path-max-length 70)
          (path-shorten-string "...")
          (dashboard-key "h"))
  "Configure Emacs Dashboard, an extensible startup screen.
Set up the visible sections via ITEMS, where each entry is
of the form (LIST-TYPE . LIST-SIZE).  For the aforementioned to work,
you also need to configure ITEM-GENERATORS, where each entry is of
the form (LIST-TYPE . LIST-GENERATOR-FUNCTION). You can quickly navigate
to each section with ITEM-SHORTCUTS and set a custom name for each one via
ITEM-NAMES.

In terms of visuals, you can choose whether you want icons to be displayed in
the section headings via SET-HEADING-ICONS? and select what icon to show for each
section with HEADING-ICONS, where each entry is of the form (LIST-TYPE . ICON-NAME-STRING).
Choose if you want section entries to be shown alongside icons via SET-FILE-ICONS?.

NAVIGATOR-BUTTONS are custom buttons that you can display below the BANNER, to include
quick shortcuts to things like web bookmarks. BANNER can be either `official' for the
official Emacs logo, #f to hide the banner, `logo' for an alternative Emacs logo, or a custom
file path to an image whose dimensions you can constrain with BANNER-MAX-HEIGHT and BANNER-MAX-WIDTH.

Remind yourself of tasks by setting ORG-AGENDA-WEEKLY? to #t and customize the format of
each task entry with ORG-AGENDA-PREFIX-FORMAT (see the Emacs variable with the same name for
information on the format strings).

Choose whether to show the bookmark name, file name, and project name, respectively, for the
entries in each section with BOOKMARKS-SHOW-BASE?, RECENTF-SHOW-BASE?, and PROJECTS-SHOW-BASE?.

You can truncate paths whose character length is greater than PATH-MAX-LENGTH by setting
PATH-STYLE to either `truncate-beginning', `truncate-middle', or `truncate-end'."
  (ensure-pred file-like? emacs-dashboard)
  (ensure-pred string? page-separator)
  (ensure-pred string? logo-title)
  (ensure-pred boolean? center-content?)
  (ensure-pred boolean? set-init-info?)
  (ensure-pred maybe-string? init-info)
  (ensure-pred maybe-list? items)
  (ensure-pred maybe-list? item-generators)
  (ensure-pred maybe-list? item-shortcuts)
  (ensure-pred maybe-list? item-names)
  (ensure-pred boolean? set-heading-icons?)
  (ensure-pred maybe-list? heading-icons)
  (ensure-pred boolean? set-file-icons?)
  (ensure-pred maybe-list? navigator-buttons)
  (ensure-pred file-like-or-path-or-symbol-or-boolean? banner)
  (ensure-pred number? banner-max-height)
  (ensure-pred number? banner-max-width)
  (ensure-pred boolean? org-agenda-weekly?)
  (ensure-pred maybe-string? org-agenda-prefix-format)
  (ensure-pred boolean? set-footer?)
  (ensure-pred maybe-string? footer)
  (ensure-pred maybe-list? footer-messages)
  (ensure-pred maybe-string? footer-icon)
  (ensure-pred boolean? bookmarks-show-base?)
  (ensure-pred boolean? recentf-show-base?)
  (ensure-pred symbol? projects-backend)
  (ensure-pred boolean? projects-show-base?)
  (ensure-pred maybe-symbol? path-style)
  (ensure-pred integer? path-max-length)
  (ensure-pred string? path-shorten-string)
  (ensure-pred string? dashboard-key)

  (define emacs-f-name 'dashboard)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'configure-rde-keymaps)
        (require 'dashboard)
        (defun configure-dashboard-open ()
          "Jump to a dashboard buffer, creating one if it doesn't exist."
          (interactive)
          (when (get-buffer-create dashboard-buffer-name)
            (switch-to-buffer dashboard-buffer-name)
            (dashboard-mode)
            (dashboard-insert-startupify-lists)
            (dashboard-refresh-buffer)))

        (define-key rde-app-map (kbd ,dashboard-key) 'configure-dashboard-open)
        (with-eval-after-load 'dashboard
          (setq dashboard-set-file-icons ,(if set-file-icons? 't 'nil))
          (setq dashboard-projects-backend ',projects-backend)
          (setq dashboard-projects-show-base ,(if projects-show-base? 't 'nil))
          (setq dashboard-bookmarks-show-base ,(if bookmarks-show-base? 't 'nil))
          (setq dashboard-recentf-show-base ,(if recentf-show-base? 't 'nil))
          (setq dashboard-center-content ,(if center-content? 't 'nil))
          ,@(if set-init-info?
                (if init-info
                    `((setq dashboard-init-info ,init-info))
                    '())
                '((setq dashboard-set-init-info nil)))
          ,@(if (get-value 'emacs-advanced-user? config)
                '((setq dashboard-show-shortcuts nil))
                '())
          (setq dashboard-page-separator ,page-separator)
          ,@(if (and (get-value 'emacs-org-agenda config)
                     org-agenda-weekly?)
                '((setq dashboard-week-agenda t))
                '())
          (setq dashboard-banner-logo-title ,logo-title)
          ,@(if (symbol? banner)
                `((setq dashboard-startup-banner ',banner))
                `((setq dashboard-startup-banner ,(match banner
                                                    (#f 'nil)
                                                    (e e)))))
          (setq dashboard-image-banner-max-height ,banner-max-height)
          (setq dashboard-image-banner-max-width ,banner-max-width)
          '()
          ,@(if items
                `((setq dashboard-items ',items))
                '())
          ,@(if item-generators
                `((setq dashboard-item-generators ',item-generators))
                '())
          ,@(if item-shortcuts
                `((setq dashboard-item-generators ',item-shortcuts))
                '())
          ,@(if item-names
                `((setq dashboard-item-generators ',item-names))
                '())
          (setq dashboard-set-heading-icons ,(if set-heading-icons? 't 'nil))
          ,@(if heading-icons
                `((setq dashboard-heading-icons ',heading-icons))
                '())
          ,@(if navigator-buttons
                `((setq dashboard-set-navigator t)
                  (setq dashboard-navigator-buttons ',navigator-buttons))
                '())
          (setq dashboard-agenda-release-buffers t)
          ,@(if org-agenda-prefix-format
                `((setq dashboard-agenda-prefix-format ,org-agenda-prefix-format))
                '())
          ,@(if set-footer?
                (append
                 (if footer
                     `((setq dashboard-footer ,footer))
                     '())
                 (if footer-messages
                     `((setq dashboard-footer-messages ',footer-messages))
                     '())
                 (if footer-icon
                     `((setq dashboard-footer-icon ,footer-icon))
                     '()))
                `((setq dashboard-set-footer nil)))
          (setq dashboard-path-max-length ,path-max-length)
          (setq dashboard-path-shorten-string ,path-shorten-string)
          ,@(if path-style
                `((setq dashboard-path-style ',path-style))
                '())))
      #:elisp-packages (list
                        emacs-dashboard
                        (get-value 'emacs-configure-rde-keymaps config)))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-dashboard)))
   (home-services-getter get-home-services)))


;;;
;;; Note-Taking
;;;

(define* (feature-emacs-markdown
          #:key
          (emacs-markdown-mode emacs-markdown-mode))
  "Configure markdown-mode to edit Markdown-formatted text in Emacs."
  (ensure-pred any-package? emacs-markdown-mode)

  (define emacs-f-name 'markdown)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to markdown-mode"
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((defgroup configure-markdown nil
          "Markdown mode customizations."
          :group 'configure)

        (define-minor-mode configure-markdown-minimal-mode
          "A more minimal interface to Markdown mode."
          :global t :group 'configure-markdown
          (if configure-markdown-minimal-mode
              (progn
                (display-line-numbers-mode -1)
                (visual-line-mode 1))
            (display-line-numbers-mode 1)
            (visual-line-mode -1)))

        (add-hook 'markdown-mode-hook 'configure-markdown-minimal-mode)
        (with-eval-after-load 'markdown-mode
          (setq markdown-header-scaling t)
          (setq markdown-header-scaling-values '(1.2 1.1 1.1 1.0 1.0 0.9))
          (setq markdown-hide-urls t)
          (setq markdown-hide-markup t)))
      #:elisp-packages (list emacs-markdown-mode)
      #:summary "markdown-mode extensions"
      #:commentary "Provide extensions to Markdown mode.")))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-markdown-mode)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-org
          #:key
          (org-directory "~/documents")
          (org-default-notes-file (format #f "~a/notes.org" org-directory))
          (org-tag-alist #f)
          (org-todo-keywords #f)
          (org-todo-keyword-faces #f)
          (org-priority-faces #f)
          (org-capture-templates #f)
          (org-archive-location (format #f "~a/archive.org::* From %s" org-directory))
          (emacs-org-modern emacs-org-modern-next)
          (org-modern? #t)
          (org-indent? #t)
          (document-converters? #f))
  "Configure Org Mode, the outline-based note management tool
and organizer for Emacs."
  (ensure-pred path? org-directory)
  (ensure-pred path? org-default-notes-file)
  (ensure-pred maybe-list? org-tag-alist)
  (ensure-pred maybe-list? org-todo-keywords)
  (ensure-pred maybe-list? org-priority-faces)
  (ensure-pred maybe-list? org-capture-templates)
  (ensure-pred path? org-archive-location)
  (ensure-pred any-package? emacs-org-modern)
  (ensure-pred boolean? org-modern?)
  (ensure-pred boolean? org-indent?)
  (ensure-pred boolean? document-converters?)

  (define emacs-f-name 'org)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Org Mode."
    (define unoconv (@ (gnu packages libreoffice) unoconv))

    (append
     (if document-converters?
         (list
          (simple-service
           'emacs-org-profile-service
           home-profile-service-type
           (list unoconv)))
         '())
     (list
      (rde-elisp-configuration-service
       emacs-f-name
       config
       `((eval-when-compile
          (require 'cl-macs))
         (defgroup configure-org nil
           "Extensions for basic Org mode features."
           :group 'configure)

         ,@(if (get-value 'emacs-consult-initial-narrowing? config)
               '((defvar configure-org-buffer-source
                   `(:name "Org"
                           :narrow ?o
                           :category buffer
                           :preview-key ,(kbd "M-.")
                           :state ,'consult--buffer-state
                           :items ,(lambda () (mapcar 'buffer-name (org-buffer-list))))
                   "Source for Org buffers to be set in `consult-buffer-sources'.")
                 (add-to-list 'consult-buffer-sources configure-org-buffer-source)
                 (add-to-list 'configure-completion-initial-narrow-alist '(org-mode . ?o)))
             '())

         (defun configure-org-timer-reset ()
           "Set `org-timer-mode-line-string' to nil."
           (interactive)
           (setq org-timer-mode-line-string nil))

         (defun configure-org-timer-update-mode-line ()
           "Update the timer in the mode line without adding surrounding angle brackets."
           (if org-timer-pause-time
               nil
             (setq org-timer-mode-line-string (substring (org-timer-value-string) 0 -1))
             (force-mode-line-update)))

         (defun configure-org-set-custom-faces ()
           "Set Org mode's faces for `configure-org-minimal-mode'."
           (interactive)
           ,@(if (get-value 'emacs-modus-themes config)
                 '((eval-when-compile
                     (require 'modus-themes))
                   (modus-themes-with-colors
                     (custom-set-faces
                      `(org-block ((,class :inherit modus-themes-fixed-pitch :weight normal)))
                      `(org-verbatim ((,class :inherit (fixed-pitch modus-themes-markup-verbatim) :weight normal)))
                      `(org-code ((,class :inherit (shadow fixed-pitch modus-themes-markup-code) :weight normal)))
                      `(org-document-info ((,class :weight bold)))
                      `(org-document-info ((,class :weight bold)))
                      `(org-document-info-keyword ((,class :inherit (shadow fixed-pitch))))
                      `(org-ellipsis ((,class :inherit (font-lock-comment-face)
                                              :weight normal
                                              :height ,(or (plist-get
                                                            (alist-get fontaine--current-preset fontaine-presets)
                                                            :default-height)
                                                           1.0))))
                      `(org-link ((,class :underline t)))
                      `(org-meta-line ((,class :inherit (font-lock-comment-face fixed-pitch))))
                      `(org-special-keyword ((,class :inherit (font-lock-comment-face))))
                      `(org-headline-done ((,class :strike-through t)))
                      `(org-table ((,class :inherit fixed-pitch)))
                      `(org-indent ((,class :inherit (org-hide fixed-pitch)))))))
               '()))

         (defun configure-org-update-faces (&optional _theme)
           "Apply appropriate faces to currently-active Org mode buffers."
           (cl-loop for buffer in (org-buffer-list)
                    do (with-current-buffer buffer
                         (configure-org-set-custom-faces))))

         (cl-defun configure-org-do-promote (&optional (levels 1))
           "Allow promoting the current heading a number of LEVELS high up the tree."
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

         (define-minor-mode configure-org-minimal-mode
           "Provide a minimal interface to Org mode."
           :group 'configure-org
           (if configure-org-minimal-mode
               (progn
                 (corfu-mode -1)
                 (electric-pair-local-mode -1)
                 (org-indent-mode)
                 (visual-line-mode)
                 (variable-pitch-mode 1)
                 (prettify-symbols-mode)
                 (display-line-numbers-mode -1)
                 (org-appear-mode)
                 (org-make-toc-mode)
                 (setq-local fill-prefix "")
                 (configure-org-set-custom-faces))
             (corfu-mode 1)
             (electric-pair-local-mode)
             (org-indent-mode -1)
             (visual-line-mode -1)
             (variable-pitch-mode -1)
             (prettify-symbols-mode -1)
             (display-line-numbers-mode 1)
             (org-appear-mode -1)
             (org-make-toc-mode -1)
             (setq-local fill-prefix nil)))

         (advice-add 'org-do-promote :override 'configure-org-do-promote)
         (add-hook 'org-mode-hook 'org-fragtog-mode)
         (add-hook 'org-mode-hook 'configure-org-minimal-mode)
         (autoload 'org-buffer-list "org")
         (let ((map mode-specific-map))
           (define-key map "l" 'org-store-link)
           (define-key map "c" 'org-capture))
         (with-eval-after-load 'org
           (dolist (module '(org-indent
                             org-tempo
                             org-habit org-crypt
                             org-protocol org-timer))
             (add-to-list 'org-modules module))
           (add-to-list
            'display-buffer-alist
            `(,(rx "*Org Links*")
              display-buffer-no-window
              (allow-no-window . t)))
           (let ((map org-mode-map))
             (define-key map (kbd "M-n") 'org-metaright)
             (define-key map (kbd "M-p") 'org-metaleft)
             (define-key map (kbd "C-c ob") 'org-switchb)
             (define-key map (kbd "C-c de") 'org-decrypt-entries))
           ,@(if (get-value 'emacs-consult config)
                 '((with-eval-after-load 'consult
                     (define-key org-mode-map (kbd "M-g h") 'consult-org-heading)))
               '())
           (setq org-directory ,org-directory)
           (setq org-return-follows-link t)
           (setq org-startup-folded 'content)
           (setq org-startup-indented t)
           (setq org-startup-with-inline-images t)
           (setq org-startup-with-latex-preview t)
           (setq org-extend-today-until 0)
           (setq org-use-fast-todo-selection 'expert)
           (setq org-highest-priority ?A)
           (setq org-lowest-priority ?C)
           (setq org-log-done 'time)
           (setq org-log-into-drawer t)
           (setq org-special-ctrl-a/e t)
           (setq org-insert-heading-respect-content t)
           (setq org-auto-align-tags t)
           (setq org-tags-column -77)
           (setq org-tags-exclude-from-inheritance '("project" "crypt"))
           (setq org-default-priority ?B)
           (setq org-default-notes-file ,org-default-notes-file)
           (setq org-enforce-todo-dependencies t)
           (setq org-enforce-todo-checkbox-dependencies t)
           (setq org-archive-location ,org-archive-location)
           (setq org-outline-path-complete-in-steps nil)
           (setq org-fast-tag-selection-single-key 'expert)
           (setq org-display-remote-inline-images 'cache)
           (setq org-image-actual-width t)
           (setq org-pretty-entities t)
           (setq org-pretty-entities-include-sub-superscripts nil)
           (setq org-M-RET-may-split-line nil)
           (setq org-ellipsis " ⤵")
           (setq org-hide-emphasis-markers t)
           (setq org-fontify-done-headline t))
         ,@(if org-priority-faces
               '((setq org-priority-faces ',org-priority-faces))
               '())
         ,@(if org-todo-keywords
               `((setq org-todo-keywords ',org-todo-keywords))
               '())
         ,@(if org-todo-keyword-faces
               `((setq org-todo-keyword-faces ',org-todo-keyword-faces))
               '())
         ,@(if org-tag-alist
               `((setq org-tag-alist ',org-tag-alist))
               '())
         ,@(if org-modern?
               `((add-hook 'org-mode-hook 'org-modern-mode)
                 (with-eval-after-load 'org-modern
                   (setq org-modern-star nil)
                   (setq org-modern-hide-stars t)
                   (setq org-modern-priority nil)))
               '())
         (advice-add 'org-refile :after 'org-save-all-org-buffers)
         (with-eval-after-load 'org-refile
           (setq org-refile-targets '((org-agenda-files . (:maxlevel . 1)))))
         ,@(if (get-value 'emacs-project config)
               '((define-key project-prefix-map "o" 'configure-project-org-capture)
                 (add-to-list 'project-switch-commands '(configure-project-org-capture "Capture with Org")))
               '())
         (with-eval-after-load 'org-capture
           (setq org-capture-bookmark nil)
           ,@(if org-capture-templates
                 `((setq org-capture-templates ',org-capture-templates))
                 '()))
         (with-eval-after-load 'org-habit
           (setq org-habit-graph-column 60))
         (with-eval-after-load 'ob-core
           (setq org-confirm-babel-evaluate nil))
         (with-eval-after-load 'org-src
           (setq org-src-tab-acts-natively t)
           (setq org-edit-src-content-indentation 0)
           (setq org-src-window-setup 'current-window)
           (setq org-catch-invisible-edits 'show-and-error)
           (setq org-src-fontify-natively t))
         (with-eval-after-load 'org-list
           (setq org-list-demote-modify-bullet '(("+" . "-")
                                                 ("-" . "+")
                                                 ("*" . "-"))))
         (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
         (with-eval-after-load 'visual-fill-column
           (setq-default visual-fill-column-center-text t)
           (setq visual-fill-column-enable-sensible-window-split t)
           (setq visual-fill-column-width 90))
         (org-crypt-use-before-save-magic)
         (with-eval-after-load 'org-crypt
           (setq org-crypt-key nil))
         (add-hook 'org-timer-stop-hook 'configure-org-timer-reset)
         (advice-add 'org-timer-update-mode-line :override 'configure-org-timer-update-mode-line)
         (with-eval-after-load 'org-timer
           (let ((map mode-specific-map))
             (define-key map "ots" 'org-timer-start)
             (define-key map "otq" 'org-timer-stop)
             (define-key map "otp" 'org-timer-pause-or-continue))
           ,@(if (get-value 'emacs-all-the-icons config)
                 '((eval-when-compile
                    (require 'all-the-icons))
                   (with-eval-after-load 'all-the-icons
                     (setq org-timer-format (concat (all-the-icons-material "timer" :v-adjust -0.1) " %s  "))))
                 '()))
         (with-eval-after-load 'ol
           (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file))
         ,@(if (and (get-value 'emacs-exwm config) (get-value 'nyxt config))
               '((require 'ol)
                 (org-link-set-parameters
                  "nyxt"
                  :store 'nyxt-store-link))
               '())
         (with-eval-after-load 'ox
           (setq org-export-with-date nil)
           (setq org-export-with-author nil)
           (setq org-export-preserve-breaks t)
           (setq org-html-postamble nil)))
       #:elisp-packages (append
                         (list
                          emacs-org-contrib
                          emacs-org-make-toc
                          emacs-org-appear
                          emacs-org-modern
                          emacs-visual-fill-column
                          emacs-org-fragtog)
                         (if (get-value 'emacs-all-the-icons config)
                             (list (get-value 'emacs-all-the-icons config))
                             '())
                         (if (get-value 'nyxt config)
                             (list (get-value 'emacs-nyxt config))
                             '())
                         (if (get-value 'emacs-modus-themes config)
                             (list (get-value 'emacs-modus-themes config))
                             '()))))
     (if (get-value 'nyxt config)
         (list
          (rde-nyxt-configuration-service
           f-name
           config
           `((define-command org-capture ()
               "Store and capture the current page link via Org mode."
               (eval-in-emacs
                '(nyxt-capture "tb"))
               (echo "Org link successfully stored and captured"))
             (define-key *rde-keymap* "C-c c" 'org-capture))))
         '())))

  (feature
   (name f-name)
   (values `((,f-name . #t)
             (emacs-org-modern? . org-modern?)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-org-roam
          #:key
          (emacs-org-roam emacs-org-roam)
          (org-roam-directory #f)
          (org-roam-dailies-directory #f)
          (org-roam-capture-templates #f)
          (org-roam-dailies-capture-templates #f)
          (org-roam-todo? #t))
  "Configure Org-roam, Roam Research for Emacs."
  (ensure-pred file-like? emacs-org-roam)
  (ensure-pred maybe-path? org-roam-directory)
  (ensure-pred maybe-path? org-roam-dailies-directory)
  (ensure-pred maybe-list? org-roam-capture-templates)
  (ensure-pred maybe-list? org-roam-dailies-capture-templates)
  (ensure-pred boolean? org-roam-todo?)

  (define emacs-f-name 'org-roam)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Org-roam."
    (append
     (list
      (rde-elisp-configuration-service
       emacs-f-name
       config
       `((eval-when-compile
          (let ((org-roam-v2-ack t))
            (require 'org-roam)))
         (defun configure-org-roam-open-ref ()
           "List all ROAM_REFS in the current buffer and allow you to open them."
           (interactive)
           (when (derived-mode-p 'org-mode)
             (if-let* ((refs (org-property-values "ROAM_REFS"))
                       (choices (mapcar (lambda (x)
                                          (org-unbracket-string "[[" "]]" x))
                                        (split-string (car (org-property-values "ROAM_REFS")) " ")))
                       (node-ref (completing-read "Refs: "
                                                  (lambda (string pred action)
                                                    (if (eq action 'metadata)
                                                        `(metadata
                                                          (category . org-roam-ref)
                                                          ,(cons 'display-sort-function 'identity))
                                                      (complete-with-action action choices string pred)))
                                                  nil 'require-match)))
                 node-ref
               (error "No roam refs in this node"))))

         (defun configure-org-roam-get-filetags ()
           "Return the top-level tags for the current org-roam node."
           (split-string
            (or (cadr (assoc "FILETAGS"
                             (org-collect-keywords '("filetags"))))
                "")
            ":" 'omit-nulls))

         (defun configure-org-roam-todo-p ()
           "Return non-nil if the current buffer has any to-do entry."
           (org-element-map
               (org-element-parse-buffer 'headline)
               'headline
             (lambda (h)
               (eq (org-element-property :todo-type h) 'todo))
             nil 'first-match))

         (defun configure-org-roam-update-todo-tag ()
           "Update the \"todo\" tag in the current buffer."
           (when (and (not (active-minibuffer-window))
                      (org-roam-file-p))
             (org-with-point-at 1
               (let* ((tags (configure-org-roam-get-filetags))
                      (is-todo (configure-org-roam-todo-p)))
                 (cond ((and is-todo (not (member "todo" tags)))
                        (org-roam-tag-add '("todo")))
                       ((and (not is-todo) (member "todo" tags))
                        (org-roam-tag-remove '("todo"))))))))

         (defun configure-org-roam-list-todo-files ()
           "Return a list of org-roam files containing the \"todo\" tag."
           (org-roam-db-sync)
           (let ((todo-nodes (cl-remove-if-not
                              (lambda (n)
                                (member "todo" (org-roam-node-tags n)))
                              (org-roam-node-list))))
             (delete-dups (mapcar #'org-roam-node-file todo-nodes))))

         (defun configure-org-roam-update-todo-files (&rest _)
           "Update the value of `org-agenda-files'."
           (setq org-agenda-files (configure-org-roam-list-todo-files)))

         (defun configure-org-roam-node-insert-immediate (arg &rest args)
           "Immediately insert new Org Roam node with ARG and ARGS in the buffer."
           (interactive "P")
           (let ((args (cons arg args))
                 (org-roam-capture-templates (list
                                              (append
                                               (car org-roam-capture-templates)
                                               '(:immediate-finish)))))
             (apply 'org-roam-node-insert args)))

         (setq org-roam-v2-ack t)
         ,@(if org-roam-directory
               `((setq org-roam-directory ,org-roam-directory))
               '())
         (autoload 'org-roam-db-autosync-enable "org-roam")
         (add-to-list 'display-buffer-alist
                      `(,(rx "*org-roam*")
                        display-buffer-same-window))
         ,@(if org-roam-todo?
               '((add-hook 'find-file-hook 'configure-org-roam-update-todo-tag)
                 (add-hook 'before-save-hook 'configure-org-roam-update-todo-tag)
                 (advice-add 'org-agenda :before 'configure-org-roam-update-todo-files))
               '())
         (let ((map mode-specific-map))
           (define-key map "nb" 'org-roam-buffer-toggle)
           (define-key map "nf" 'org-roam-node-find)
           (define-key map "nr" 'org-roam-ref-find)
           (define-key map "nc" 'org-roam-capture)
           (define-key map "ni" 'org-roam-node-insert)
           (define-key map "nI" 'configure-org-roam-node-insert-immediate)
           (define-key map "ndn" 'org-roam-dailies-capture-today)
           (define-key map "ndd" 'org-roam-dailies-goto-today)
           (define-key map "ndY" 'org-roam-dailies-capture-yesterday)
           (define-key map "ndT" 'org-roam-dailies-capture-tomorrow)
           (define-key map "ndy" 'org-roam-dailies-goto-yesterday)
           (define-key map "ndt" 'org-roam-dailies-goto-tomorrow)
           (define-key map "ndC" 'org-roam-dailies-capture-date)
           (define-key map "ndc" 'org-roam-dailies-goto-date))
         (with-eval-after-load 'org-roam
           (org-roam-db-autosync-enable)
           (let ((map org-mode-map))
             (define-key map (kbd "C-TAB") 'completion-at-point)
             (define-key map (kbd "C-c r r") 'org-roam-ref-add)
             (define-key map (kbd "C-c r R") 'org-roam-ref-remove)
             (define-key map (kbd "C-c r f") 'org-roam-ref-find)
             (define-key map (kbd "C-c r t") 'org-roam-tag-add)
             (define-key map (kbd "C-c r T") 'org-roam-tag-remove)
             (define-key map (kbd "C-c r a") 'org-roam-alias-add)
             (define-key map (kbd "C-c r A") 'org-roam-alias-remove)
             (define-key map (kbd "C-c r O") 'configure-org-roam-open-ref)
             (define-key map (kbd "C-c n df") 'org-roam-dailies-goto-next-note)
             (define-key map (kbd "C-c n db") 'org-roam-dailies-goto-previous-note))
           ,@(if org-roam-capture-templates
                 `((setq org-roam-capture-templates ',org-roam-capture-templates))
                 '())
           (with-eval-after-load 'org-roam-dailies
             ,@(if org-roam-dailies-capture-templates
                   `((setq org-roam-dailies-capture-templates ',org-roam-dailies-capture-templates))
                   '())
             ,@(if org-roam-dailies-directory
                   `((setq org-roam-dailies-directory ,org-roam-dailies-directory))
                   '()))
           (with-eval-after-load 'org-roam-node
             (setq org-roam-completion-everywhere t)
             (setq org-roam-node-display-template
                   (concat "${title:80}" (propertize "${tags:35}" 'face 'org-tag)))
             (setq org-roam-node-annotation-function
                   (lambda (node) (marginalia--time (org-roam-node-file-mtime node)))))
           ,@(if (get-value 'emacs-embark config)
                 '((eval-when-compile
                    (require 'embark))
                   (with-eval-after-load 'embark
                     (embark-define-keymap embark-roam-ref-map
                       "Keymap for actions to be triggered on org-roam refs."
                       :parent embark-url-map
                       ("RET" browse-url-generic)
                       ("c" browse-url-chromium)
                       ("r" org-roam-ref-remove)
                       ("v" 'configure-mpv-play-url)
                       ("V" 'configure-mpv-play-url-other-window))
                     (add-to-list 'embark-keymap-alist '(org-roam-ref . embark-roam-ref-map))
                     (advice-add 'org-roam-ref-add :around 'configure-browse-url-trace-url)))
                 '())))
       #:elisp-packages (append
                         (list emacs-org-roam)
                         (if (get-value 'emacs-embark config)
                             (list (get-value 'emacs-embark config))
                             '()))))
     (if (get-value 'nyxt config)
         (list
          (rde-nyxt-configuration-service
           f-name
           config
           `((define-command org-roam-capture ()
               "Store and capture the current page as an Org Roam node."
               (eval-in-emacs
                '(nyxt-capture "w" :roam-p t)))
             (define-key *rde-keymap* "C-c n" 'org-roam-capture))))
         '())))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-org-roam)
             (org-roam-todo? . ,org-roam-todo?)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-org-agenda
          #:key
          (org-agenda-files #f)
          (org-agenda-custom-commands (@@ (rde features emacs-xyz) %rde-org-agenda-custom-commands))
          (org-agenda-prefix-format '()))
  "Configure the Org Agenda planner."
  (ensure-pred maybe-list? org-agenda-files)
  (ensure-pred list? org-agenda-custom-commands)
  (ensure-pred maybe-list? org-agenda-prefix-format)

  (define emacs-f-name 'org-agenda)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to The Agenda."
    (require-value 'emacs-org config)

    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((defgroup configure-org-agenda nil
          "Custom enhancements to the Org Agenda."
          :group 'configure)

        (defvar configure-org-agenda-appt-timer nil
          "Timer to update `appt-time-msg-list' from Agenda entries.")

        (defun configure-org-agenda-to-appt ()
          "Reset the `appt-mode' list and initialize it from Agenda entries."
          (interactive)
          (setq appt-time-msg-list nil)
          (org-agenda-to-appt))

        (defun configure-org-agenda-appt-reset ()
          "Initialize the `appt-mode' list for today and reset the timer to run tomorrow."
          (interactive)
          (configure-org-agenda-to-appt)
          (setq configure-org-agenda-appt-timer
                (run-at-time "24:01" nil 'configure-org-agenda-appt-reset)))

        (defun configure-org-agenda-daily ()
          "Invoke a custom Org agenda dispatcher for the daily agenda view."
          (interactive)
          (org-agenda nil (kbd "C-d")))

        (defun configure-org-agenda-overview ()
          "Invoke a custom Org agenda dispatcher for the overview."
          (interactive)
          (org-agenda nil (kbd "C-o")))

        (defun configure-org-agenda-category (&optional len)
          "Get category of the Org Agenda item at point.
The category is defined by one of the following:
- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension
When LEN is a number, the resulting string is right-padded with
white space and then truncated with an ellipsis on the right if the
result is longer than LEN."
          (let* ((filename (when buffer-file-name
                             (file-name-sans-extension
                              (file-name-nondirectory buffer-file-name))))
                 (title (cadr (assoc "TITLE" (org-collect-keywords '("title")))))
                 (project-title (if (and title (string-match (rx (group (+ any)) ":" (+ any)) title))
                                    (match-string 1 title)
                                  title))
                 (category (org-get-category))
                 (agenda-category (if (and title (string= category filename))
                                      project-title
                                    category))
                 (result
                  (or (if (numberp len)
                          (s-truncate len (s-pad-right len " " agenda-category))
                        agenda-category)
                      "")))
            (if (and (not project-title) (numberp len))
                (s-truncate len (s-pad-right len " " result))
              result)))

        (define-minor-mode configure-org-agenda-appt-mode
          "Set up `appt-mode' integration for Agenda items."
          :global t :group 'configure-org-agenda
          (if configure-org-agenda-appt-mode
              (progn
                (setq configure-org-agenda-appt-timer
                      (configure-org-agenda-appt-reset))
                (add-hook 'org-agenda-finalize-hook 'configure-org-agenda-to-appt))
            (progn
              (remove-hook 'org-agenda-finalize-hook 'configure-org-agenda-to-appt)
              (cancel-timer configure-org-agenda-appt-timer))))

        (define-key global-map (kbd "C-x C-a") 'org-agenda)
        (define-key rde-app-map (kbd "ad") 'configure-org-agenda-daily)
        (define-key rde-app-map (kbd "ao") 'configure-org-agenda-overview)
        (add-hook 'org-agenda-mode-hook 'hack-dir-local-variables-non-file-buffer)
        (configure-org-agenda-appt-mode)
        (with-eval-after-load 'org-agenda
          ,@(if org-agenda-files
                `((setq org-agenda-files ',org-agenda-files))
                '())
          ,@(if org-agenda-prefix-format
                (if (get-value 'org-roam-todo? config)
                    `((setq org-agenda-prefix-format
                            '((agenda . " %i %(configure-org-agenda-category 12)%?-12t% s")
                              (todo . " %i %(configure-org-agenda-category 12) ")
                              (tags . " %i %(configure-org-agenda-category 12) ")
                              (search . " %i %(configure-org-agenda-category 12) "))))
                    `((setq org-agenda-prefix-format ',org-agenda-prefix-format)))
                '())
          (setq org-agenda-sticky t)
          (setq org-agenda-tags-column 0)
          (setq org-agenda-block-separator ?-)
          (setq org-agenda-time-grid '((daily today require-timed)
                                       (800 1000 1200 1400 1600 1800 2000)
                                       " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
          (setq org-agenda-current-time-string
                "⭠ now ─────────────────────────────────────────────────")
          (setq org-agenda-start-with-log-mode t)
          (setq org-agenda-todo-ignore-scheduled t)
          (setq org-agenda-todo-ignore-deadlines t)
          (setq org-agenda-todo-ignore-timestamp t)
          (setq org-agenda-window-setup 'current-window)
          (setq org-agenda-dim-blocked-tasks t)
          (setq org-agenda-skip-scheduled-if-done t)
          (setq org-agenda-skip-deadline-if-done t)
          (setq org-agenda-compact-blocks nil)
          (setq org-agenda-include-diary t)
          (setq org-agenda-custom-commands ',org-agenda-custom-commands)
          (setq org-agenda-bulk-custom-functions
                '((?P (lambda nil
                        (org-agenda-priority 'set))))))
        (advice-add 'org-redo :after 'configure-org-agenda-to-appt)
        (add-hook 'org-capture-after-finalize-hook 'configure-org-agenda-to-appt)
        ,@(if (get-value 'emacs-org-modern? config)
              '((add-hook 'org-agenda-finalize-hook 'org-modern-agenda))
              '())))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-citar
          #:key
          (emacs-citar emacs-citar)
          (global-bibliography (list "~/documents/references.bib")))
  "Configure built-in citation support in Org and the Citar package."
  (ensure-pred any-package? emacs-citar)

  (define emacs-f-name 'citar)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Citar."
    (define biblatex? (get-value 'tex-biblatex config))

    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load 'oc
          ,@(if biblatex?
                '((require 'oc-biblatex)
                  (setq org-cite-export-processors '((latex biblatex)
                                                     (t basic))))
                '())
          (setq org-cite-global-bibliography (list ,@global-bibliography)))
        (define-key mode-specific-map "i" 'citar-insert-citation)
        (with-eval-after-load 'citar
          ,@(if (get-value 'emacs-embark config)
                `((require 'citar-embark)
                  (citar-embark-mode))
                '())
          ,@(if (get-value 'emacs-all-the-icons config)
                '((eval-when-compile
                   (require 'all-the-icons))
                  (setq citar-symbols
                        `((file ,(all-the-icons-faicon
                                  "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
                          (note ,(all-the-icons-material
                                  "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
                          (link ,(all-the-icons-octicon
                                  "link" :face 'all-the-icons-orange :v-adjust 0.01) . " "))))
                '())
          (setq citar-symbol-separator "  ")
          (setq org-cite-insert-processor 'citar)
          (setq org-cite-follow-processor 'citar)
          (setq org-cite-activate-processor 'citar)
          (setq citar-bibliography org-cite-global-bibliography)))
      #:elisp-packages (append (list emacs-citar)
                               (if (get-value 'emacs-all-the-icons config)
                                   (list (get-value 'emacs-all-the-icons config))
                                   '())))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


;;;
;;; Reading
;;;

(define* (feature-emacs-pdf-tools
          #:key
          (emacs-pdf-tools emacs-pdf-tools)
          (emacs-saveplace-pdf-view emacs-saveplace-pdf-view))
  "Configure pdf-tools, the Emacs support library for PDF files."
  (ensure-pred any-package? emacs-pdf-tools)
  (ensure-pred any-package? emacs-saveplace-pdf-view)

  (define emacs-f-name 'pdf-tools)
  (define f-name (symbol-append 'emacs emacs-f-name))

  (define (get-home-services config)
    "Return home services related to pdf-tools."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'cl-lib)
        (defgroup configure-pdf-tools nil
          "Custom tweaks for PDF Tools."
          :group 'configure)

        (defun configure-pdf-tools--list-buffers ()
          "List all currently-opened `pdf-view' mode buffers."
          (cl-remove-if-not
           (lambda (buffer)
             (with-current-buffer buffer
               (derived-mode-p 'pdf-view-mode)))
           (buffer-list)))

        (defun configure-pdf-tools-update-buffers (&optional _theme)
          "Apply `configure-pdf-tools-mode' to currently opened `pdf-view' mode buffers."
          (cl-loop for buffer in (configure-pdf-tools--list-buffers)
                   do (with-current-buffer buffer
                        (configure-pdf-tools-mode 1))))

        (defun configure-pdf-tools-run (fun &rest args)
          "Run FUN with ARGS in the context of the first `pdf-view' mode buffer."
          (if (cl-find (current-buffer) (configure-pdf-tools--list-buffers))
              (apply fun args)
            (let ((buffer (car (configure-pdf-tools--list-buffers))))
              (save-window-excursion
                (with-selected-window (get-buffer-window buffer)
                  (apply fun args))))))

        (define-minor-mode configure-pdf-tools-mode
          "Apply `pdf-tools' settings based on the current theme."
          :global t :group 'configure-pdf-tools
          (if configure-pdf-tools-mode
              (if (configure-modus-themes--dark-theme-p)
                  (pdf-view-themed-minor-mode 1)
                (pdf-view-themed-minor-mode -1))
            (pdf-view-themed-minor-mode -1)))

        (autoload 'pdf-loader-install "pdf-loader")
        (pdf-loader-install)
        (add-hook 'pdf-view-mode-hook 'configure-pdf-tools-mode)
        (advice-add 'pdf-view-next-page-command :around 'configure-pdf-tools-run)
        (advice-add 'pdf-view-previous-page-command :around 'configure-pdf-tools-run)
        (let ((map mode-specific-map))
          (define-key map (kbd "pn") 'pdf-view-next-page-command)
          (define-key map (kbd "pp") 'pdf-view-previous-page-command))
        (with-eval-after-load 'pdf-tools
          (setq pdf-view-display-size 'fit-page)
          (setq pdf-view-resize-factor 1.025)
          (require 'saveplace-pdf-view)
          (save-place-mode)))
      #:elisp-packages (append
                        (list emacs-pdf-tools
                              emacs-saveplace-pdf-view)
                        (if (get-value 'emacs-modus-themes config)
                            (list (get-value 'emacs-modus-themes config))
                          '()))
      #:summary "Custom extensions to PDF Tools."
      #:commentary "Add custom helpers to control a PDF Tool buffer from outside the window and
custom themeing.")))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


;;;
;;; Documentation
;;;

(define* (feature-emacs-info
          #:key
          (emacs-info-plus emacs-info-plus))
  "Configure Info and Info+ for reading Info documents in Emacs."
  (ensure-pred file-like? emacs-info-plus)

  (define emacs-f-name 'info)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Info."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load 'info
          (define-key Info-mode-map "q" 'kill-this-buffer)
          (setq Info-use-header-line nil)
          (require 'info+)
          (add-hook 'Info-mode-hook 'visual-line-mode)
          (add-hook 'Info-mode-hook 'configure-modus-themes-set-info-faces)
          (setq info-manual+node-buffer-name-mode t)
          (setq Info-persist-history-mode t)
          (setq Info-fontify-isolated-quote-flag nil)
          (setq Info-fontify-reference-items-flag t)
          (setq Info-fontify-quotations t)
          (setq Info-breadcrumbs-in-mode-line-mode nil)))
      #:elisp-packages (list emacs-info-plus))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-info-plus)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-which-key
          #:key
          (idle-delay 1)
          (min-height 1))
  "Configure the which-key Emacs package.
MIN-HEIGHT can be used to adjust the height of the popup.
IDLE-DELAY controls the delay after which the popup is shown."
  (ensure-pred integer? idle-delay)
  (ensure-pred integer? min-height)

  (define emacs-f-name 'which-key)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to which-key."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((autoload 'which-key-mode "which-key")
        (which-key-mode)
        (with-eval-after-load 'which-key
          (setq which-key-idle-delay ,idle-delay)
          (setq which-key-min-display-lines ,min-height)))
      #:elisp-packages (list emacs-which-key))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-helpful
          #:key
          (emacs-helpful emacs-helpful))
  "Configure the Helpful package, an alternative to the Emacs
built-in help that provides much more contextual information."
  (ensure-pred file-like? emacs-helpful)

  (define emacs-f-name 'helpful)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Helpful."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((let ((map help-map))
          (define-key map "f" 'helpful-callable)
          (define-key map "v" 'helpful-variable)
          (define-key map "o" 'helpful-at-point)
          (define-key map "k" 'helpful-key)
          (define-key map "F" 'helpful-function)
          (define-key map "C" 'helpful-command)
          (define-key map (kbd "C-l") 'describe-face))
        (add-hook 'helpful-mode-hook 'visual-line-mode)
        (with-eval-after-load 'helpful
          (define-key helpful-mode-map "q" 'kill-this-buffer))
        (with-eval-after-load 'window
          (add-to-list 'display-buffer-alist
                       `(,(rx "*help" (* any) "*")
                         (display-buffer-reuse-window
                          display-buffer-same-window)
                         (reusable-frames . t))))
        (with-eval-after-load 'help-mode
          (define-key help-mode-map "q" 'kill-current-buffer)
          (setq help-window-select t)))
      #:elisp-packages (list emacs-helpful))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-helpful)))
   (home-services-getter get-home-services)))


;;;
;;; Web
;;;

(define* (feature-emacs-browse-url
          #:key
          (extra-url-mappings '()))
  "Configure the browse-url library in Emacs to load URLs in browsers."
  (ensure-pred elisp-config? extra-url-mappings)

  (define emacs-f-name 'browse-url)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define* (get-home-services config)
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'cl-lib)
        (defgroup configure-browse-url nil
          "Generic utilities to enhance `browse-url'."
          :group 'configure)

        (defcustom configure-browse-url-mappings '()
          "URL mapping alist.
It has the form (SERVICE . PRIVATE-MAPPING) where SERVICE is the hostname of
a media service and PRIVATE-MAPPING is a cons pair of (REGEX . PRIVATE-HOST)
where REGEX is what should match the alternative service URL and PRIVATE-HOST
is the preferred host of the alternative service to rewrite urls to."
          :type 'list
          :group 'configure-browse-url)

        (cl-defun configure-browse-url--transform-host (url &key (alt t))
          "Transform URL to its currently set proxy in `configure-browse-url-mappings'.
If ALT is non-nil, URL is a proxy URL, so try to find the original service url."
          (string-match (rx (group (+ any) "://" (+ (not "/"))) (+ any)) url)
          (if-let* ((service-url (match-string 1 url))
                    (mapping (if alt
                                 (cl-rassoc service-url configure-browse-url-mappings
                                            :test (lambda (url privacy-map)
                                                    (string-match-p (car privacy-map) url)))
                               (assoc-string service-url configure-browse-url-mappings))))
              (if alt
                  (replace-regexp-in-string
                   service-url
                   (car mapping)
                   url)
                (replace-regexp-in-string
                 service-url
                 (cddr mapping)
                 url))
            url))

        (defun configure-browse-url-bookmark-make-record (url title)
          "Create a bookmark record from a browser buffer with URL and TITLE."
          (let* ((defaults (delq nil (list title url)))
                 (bookmark
                  `(,title
                    ,@(bookmark-make-record-default 'no-file)
                    ,(cons 'browser-url url)
                    ,(cons 'filename url)
                    ,(cons 'handler 'configure-browse-url-bookmark-jump)
                    ,(cons 'defaults defaults))))
            bookmark))

        (defun configure-browse-url-bookmark-jump (bookmark)
          "Jump to BOOKMARK in the default browser."
          (let ((location (bookmark-prop-get bookmark 'browser-url)))
            (browse-url-default-browser location)))

        (defun configure-browse-url-alt-bookmark-jump (bookmark)
          "Jump to BOOKMARK in an alternative browser."
          (cl-letf (((symbol-function 'browse-url-can-use-xdg-open) 'ignore))
            (configure-browse-url-bookmark-jump bookmark)))

        (defun configure-browse-url-with-cookies (cookies &optional url)
          "Fetch and open URL with corresponding external application and COOKIES."
          (interactive "\nsURL: ")
          (let ((url-request-extra-headers
                 `(("Cookie"
                    ,(cl-loop for (field cookie) in cookies
                              collect (format " %s=%s;" field cookie) into headers
                              finally (return (string-join headers))))))
                (filename (concat temporary-file-directory
                                  (car (last (split-string url "/"))))))
            (unless (file-exists-p filename)
              (with-current-buffer
                  (url-retrieve-synchronously url t)
                (goto-char (point-min))
                (re-search-forward "^$")
                (forward-line 1)
                (delete-region (point) (point-min))
                (write-region (point-min) (point-max) filename)))
            (consult-file-externally filename)))

        (defun configure-browse-url-add-scheme (fun url &rest args)
          "Add an HTTPS scheme to URL if missing and invoke FUN and ARGS with it."
          (let ((link (if (string-match (rx bol (+ (in (?A . ?Z))) ":") url)
                          url
                        (concat "https:" url))))
            (apply fun link args)))

        (defun configure-browse-url-trace-url (fun url &rest args)
          "Transform mapped URL to its original host and invoke FUN and ARGS with it."
          (let ((link (configure-browse-url--transform-host url)))
            (apply fun link args)))

        (setq configure-browse-url-mappings
              (append
               (list
                ,@(if (get-value 'youtube-proxy config)
                      `((cons "https://www.youtube.com" (cons "^invidio.*" ,(get-value 'youtube-proxy config))))
                      '())
                ,@(if (get-value 'reddit-proxy config)
                      `((cons "https://www.reddit.com" (cons ".*teddit.*" ,(get-value 'reddit-proxy config))))
                      '())
                ,@(if (get-value 'quora-proxy config)
                      `((cons "https://quora.com" (cons ".*quora.*" ,(get-value 'quora-proxy config))))
                      '())
                ,@(if (get-value 'twitter-proxy config)
                      `((cons "https://twitter.com" (cons ".*nitter.*" ,(get-value 'twitter-proxy config))))
                      '())
                ,@(if (get-value 'imgur-proxy config)
                      `((cons "https://imgur.com" (cons ".*imgin.*" ,(get-value 'imgur-proxy config))))
                      '())
                ,@(if (get-value 'google-proxy config)
                      `((cons "https://www.google.com" (cons ".*whoogle.*" ,(get-value 'google-proxy config))))
                      '())
                ,@(if (get-value 'medium-proxy config)
                      `((cons "https://medium.com" (cons ".*scribe.*" ,(get-value 'medium-proxy config))))
                      '()))
               ',extra-url-mappings))
        (advice-add 'browse-url-xdg-open :around 'configure-browse-url-add-scheme)
        (with-eval-after-load 'browse-url
          (setq browse-url-browser-function 'browse-url-xdg-open))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-eww)
  "Configure EWW, the Emacs Web Wowser."

  (define emacs-f-name 'eww)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to EWW."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((add-hook 'eww-mode-hook 'eww-toggle-images)
        (with-eval-after-load 'eww
          (setq eww-search-prefix "https://farside.link/whoogle/search?q="))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-webpaste
          #:key
          (emacs-webpaste emacs-webpaste)
          (webpaste-providers '())
          (webpaste-key "P"))
  "Configure Webpaste.el, a mode to paste whole buffers or
parts of buffers to pastebin-like services.
WEBPASTE-PROVIDERS denotes the list of providers that will
be used in descending order of priority."
  (ensure-pred file-like? emacs-webpaste)
  (ensure-pred list? webpaste-providers)
  (ensure-pred string? webpaste-key)

  (define emacs-f-name 'webpaste)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Webpaste.el."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'configure-rde-keymaps)
        (defvar rde-webpaste-map nil
          "Map to bind `webpaste' commands under.")
        (define-prefix-command 'rde-webpaste-map)
        (define-key rde-app-map (kbd ,webpaste-key) 'rde-webpaste-map)
        (let ((map rde-webpaste-map))
          (define-key map "b" 'webpaste-paste-buffer)
          (define-key map "r" 'webpaste-paste-region)
          (define-key map "p" 'webpaste-paste-buffer-or-region))
        (with-eval-after-load 'webpaste
          (setq webpaste-provider-priority ',webpaste-providers)
          (setq webpaste-paste-confirmation t)))
      #:elisp-packages (list emacs-webpaste
                             (get-value 'emacs-configure-rde-keymaps config)))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-webpaste)))
   (home-services-getter get-home-services)))


;;;
;;; Generic
;;;

(define* (feature-emacs-time
          #:key
          (world-clock-timezones #f)
          (world-clock-key "C")
          (world-clock-time-format "%A %d %B %R %Z")
          (display-time? #f)
          (display-time-24hr? #f)
          (display-time-date? #f))
  "Configure time.el, an Emacs library to display the time.
Choose the timezones you'll be prompted with upon calling `world-clock'
with WORLD-CLOCK-TIMEZONES and change its format with WORLD-CLOCK-TIME-FORMAT
(see `format-time-string' in Emacs for information on the format strings).
If you want to display time on the mode line, set DISPLAY-TIME? to #t, and
accordingly set its appearance with DISPLAY-TIME-24HR? and DISPLAY-TIME-DATE?."
  (ensure-pred maybe-list? world-clock-timezones)
  (ensure-pred string? world-clock-key)
  (ensure-pred string? world-clock-time-format)
  (ensure-pred boolean? display-time?)
  (ensure-pred boolean? display-time-24hr?)
  (ensure-pred boolean? display-time-date?)

  (define emacs-f-name 'time)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to time.el."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
          (require 'time))
        (require 'configure-rde-keymaps)
        (define-key rde-app-map (kbd ,world-clock-key) 'world-clock)
        ,@(if world-clock-timezones
              `((setq world-clock-list ',world-clock-timezones))
              '())
        (setq display-time-world-time-format ,world-clock-time-format)
        (setq display-time-default-load-average nil)
        (setq display-time-load-average-threshold 0)
        ,@(if display-time-date?
              '((setq display-time-day-and-date t))
              '())
        ,@(if display-time-24hr?
              '((setq display-time-24hr-format t))
              '())
        ,@(if display-time?
              '((display-time-mode))
              '()))
      #:elisp-packages (list (get-value 'emacs-configure-rde-keymaps config)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-dired
          #:key
          (emacs-dired-rsync emacs-dired-rsync))
  "Configure the Emacs Directory Editor."
  (ensure-pred any-package? emacs-dired-rsync)

  (define emacs-f-name 'dired)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Dired."
    (define zip (@ (gnu packages compression) zip))
    (define rsync (@ (gnu packages rsync) rsync))

    (list
     (simple-service
      'home-dired-profile-service
      home-profile-service-type
      (list zip rsync))
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((defun configure-dired-open-externally ()
          "Open files in Dired through their corresponding external program."
          (interactive)
          (let ((files (dired-get-marked-files)))
            (mapc 'consult-file-externally files)))

        (define-key global-map (kbd "s-d") 'dired-jump)
        (with-eval-after-load 'dired
          (add-hook 'dired-mode-hook 'dired-hide-details-mode)
          (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
          (setq dired-dwim-target t)
          (setq dired-listing-switches "-lA")
          (setq delete-by-moving-to-trash nil)
          (setq dired-kill-when-opening-new-dired-buffer t)
          (setq dired-recursive-deletes 'always)
          (setq dired-clean-confirm-killing-deleted-buffers nil)
          (setq dired-recursive-copies 'always)
          (setq dired-deletion-confirmer 'y-or-n-p)
          (setq dired-rsync-options "--exclude .git/ --exclude .gitignore -az --info=progress2 --delete")
          (let ((map dired-mode-map))
            (define-key map "q" 'kill-current-buffer)
            (define-key map (kbd "C-c C-r") 'dired-rsync)
            (define-key map "V" 'configure-dired-open-externally)))
        ,@(if (get-value 'emacs-all-the-icons config)
              '((add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
                (with-eval-after-load 'all-the-icons-dired
                  (setq all-the-icons-dired-monochrome nil)))
            '())
        (with-eval-after-load 'ls-lisp
          (setq ls-lisp-dirs-first t)
          (setq ls-lisp-use-insert-directory-program nil)))
      #:elisp-packages (append
                        (if (get-value 'emacs-all-the-icons config)
                            (list emacs-all-the-icons-dired)
                            '())
                        (list emacs-dired-rsync))
      #:summary "Custom Dired extensions"
      #:commentary "Provide extensions to the DIRectory EDitor.")))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-calc
          #:key
          (emacs-calc-currency emacs-calc-currency)
          (currency 'EUR)
          (exchange-update-interval 7))
  "Configure Calc, an advanced desk calculator and mathematical tool for Emacs.
Additionally, you can compute the current exchange rate for your preferred
CURRENCY and update it every EXCHANGE-UPDATE-INTERVAL days."
  (ensure-pred any-package? emacs-calc-currency)
  (ensure-pred symbol? currency)
  (ensure-pred number? exchange-update-interval)

  (define emacs-f-name 'calc)
  (define f-name (symbol-append 'emacs emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Calc."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'xdg)
        (add-hook 'calc-start-hook 'calc-currency-load)
        (autoload 'calc-currency-load "calc-currency")
        (with-eval-after-load 'calc-currency
          (setq calc-currency-exchange-rates-file
                (expand-file-name "calc-currency-rates.el" (or (xdg-cache-home) "~/.cache")))
          (setq calc-currency-base-currency ',currency)
          (setq calc-currency-update-interval ,exchange-update-interval)))
      #:elisp-packages (list emacs-calc-currency))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-tramp
          #:key
          (default-method "ssh"))
  "Configure TRAMP, a remote file editing tool for Emacs."
  (ensure-pred string? default-method)

  (define emacs-f-name 'tramp)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to TRAMP."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((defun configure-tramp--parse-sconfig-hosts ()
          "Parse SSH configuration file and return a list of host definitions."
          (when-let ((file (expand-file-name "~/.ssh/config")))
            (with-temp-buffer
              (insert-file-contents-literally file)
              (goto-char (point-min))
              (delete
               nil
               (cl-loop
                while (not (eobp))
                when (re-search-forward
                      (rx bol (* space) "Host" space
                          (group (+ (any "a-z" "A-Z" "0-9" "_.%*" "-"))))
                      (point-at-eol) t)
                collect (match-string 1)
                unless (> (skip-chars-forward "\t") 0)
                do (forward-line 1))))))

        (defun configure-tramp-run (read command &rest args)
          "Execute COMMAND with ARGS in TRAMP session and READ remote directory/file."
          (let* ((host (completing-read "SSH host: " (configure-tramp--parse-sconfig-hosts)))
                 (action
                  (pcase read
                    ('dir (read-directory-name
                           (format "Directory (%s): " host)
                           (format "/-:%s:" host)))
                    ('file (read-file-name (format "File (%s): " host) (format "/-:%s:" host)))
                    (_ (format "/-:%s:" host))))
                 (default-directory action))
            (if args
                (apply command args)
              (if read
                  (funcall command action)
                (funcall command)))))

        ,@(if (get-value 'emacs-consult-initial-narrowing? config)
              '((defvar configure-tramp-buffer-source
                  `(:name "Tramp"
                          :narrow ?r
                          :category buffer
                          :preview-key ,(kbd "M-.")
                          :state ,'consult--buffer-state
                          :items ,(lambda () (mapcar 'buffer-name (tramp-list-remote-buffers))))
                  "Source for TRAMP buffers to be set in `consult-buffer-sources'.")
                (add-to-list 'consult-buffer-sources configure-tramp-buffer-source))
            '())

        (defun configure-tramp-shell (&optional arg)
          "Open a shell buffer inside a TRAMP host with ARG."
          (interactive "P")
          (configure-tramp-run nil 'shell arg))

        (defun configure-tramp-eshell (&optional arg)
          "Open an eshell buffer inside a TRAMP host with ARG."
          (interactive "P")
          (configure-tramp-run nil 'eshell arg))

        (defun configure-tramp-dired ()
          "Open a Dired buffer inside a TRAMP host."
          (interactive)
          (configure-tramp-run 'dir 'dired))

        (defun configure-tramp-find-file ()
          "Open a file inside a TRAMP host."
          (interactive)
          (configure-tramp-run 'file 'find-file))

        (let ((map mode-specific-map))
          (define-key map "rf" 'configure-tramp-find-file)
          (define-key map "rd" 'configure-tramp-dired)
          (define-key map "rs" 'configure-tramp-shell))
        (with-eval-after-load 'tramp
          (setq tramp-verbose 1)
          (setq tramp-default-method ,default-method)
          (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
          (set-default 'tramp-default-proxies-alist
                       '((".*" "\\`root\\'" "/ssh:%h:")))))
      #:summary "Helpers for TRAMP"
      #:commentary "Provide helpers for TRAMP, the remote file editing mode for Emacs.")))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-battery
          #:key
          (battery-icons '()))
  "Configure the display of battery status in Emacs."

  (define emacs-f-name 'battery)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Battery mode."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((setq battery-mode-line-format "%b%p%% ")
        (display-battery-mode)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-display-wttr
          #:key
          (emacs-display-wttr emacs-display-wttr))
  "Configure the display of weather in Emacs."
  (ensure-pred file-like? emacs-display-wttr)

  (define emacs-f-name 'display-wttr)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to display-wttr."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((setq display-wttr-format "%C %t")
        (with-eval-after-load 'display-wttr-autoloads
          (display-wttr-mode)))
      #:elisp-packages (list emacs-display-wttr))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-display-wttr)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-tab-bar
          #:key
          (modules-left '())
          (modules-center '())
          (modules-right '()))
  "Configure the Emacs Tab Bar."
  (ensure-pred elisp-config? modules-left)
  (ensure-pred elisp-config? modules-center)
  (ensure-pred elisp-config? modules-right)

  (define emacs-f-name 'tab-bar)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to the Emacs Tab Bar."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((defgroup configure-tab-bar nil
          "Configure the tab bar via menu items."
          :group 'configure)

        (defcustom configure-tab-bar-modules-left '()
          "List of modules on the left-hand side of the tab bar."
          :type '(repeat configure-tab-bar-module)
          :group 'configure-tab-bar)

        (defcustom configure-tab-bar-modules-center '()
          "List of modules in the center of the tab bar."
          :type '(repeat configure-tab-bar-module)
          :group 'configure-tab-bar)

        (defcustom configure-tab-bar-modules-right '()
          "List of modules on the right-hand side of the tab bar."
          :type '(repeat configure-tab-bar-module)
          :group 'configure-tab-bar)

        (cl-defstruct configure-tab-bar-module
          "Menu item module to be displayed in the tab bar."
          id label help action)

        (defun configure-tab-bar-build-formatter (modules)
          "Build a tab bar formatter with MODULES."
          (mapcar (lambda (item)
                    (let ((label (configure-tab-bar-module-label item)))
                      `(,(configure-tab-bar-module-id item) menu-item
                        ,(cond
                          ((symbolp label)
                           `(when (boundp ',label)
                              ,label))
                          ((and (listp label) (equal (car label) :eval))
                           `(format-mode-line ',label))
                          (t label))
                        ,(or (configure-tab-bar-module-action item)
                             'nil)
                        ,@(when (configure-tab-bar-module-help item)
                            `(:help ,(configure-tab-bar-module-help item))))))
                  modules))

        (defun configure-tab-bar-format-left ()
          "Return modules for the left-hand side of the tab bar."
          (configure-tab-bar-build-formatter configure-tab-bar-modules-left))

        (defun configure-tab-bar-format-center ()
          "Return modules for the center of the tab bar."
          (let* ((modules (mapconcat
                           (lambda (module)
                             (let ((label (configure-tab-bar-module-label module)))
                               (if (symbolp label)
                                   (symbol-value label)
                                 label)))
                           configure-tab-bar-modules-center ""))
                 (str (concat (propertize
                               " " 'display
                               `(space :align-to (- center ,(/ (length modules) 2.0)))))))
            (cons
             `(align-center menu-item ,str nil)
             (configure-tab-bar-build-formatter configure-tab-bar-modules-center))))

        (defun configure-tab-bar-format-right ()
          "Return modules for the right-hand side of the tab bar."
          (let* ((labels (mapconcat
                          (lambda (module)
                            (let ((label (configure-tab-bar-module-label module)))
                              (if (symbolp label)
                                  (symbol-value label)
                                label)))
                          configure-tab-bar-modules-right ""))
                 (n-icons (cl-loop with nprops = 0
                                   for i from 0 to (- (length labels) 1)
                                   when (get-text-property i 'rear-nonsticky labels)
                                   do (cl-incf nprops)
                                   finally (cl-return nprops)))
                 (hpos (+ (length labels) n-icons))
                 (str (propertize " " 'display `(space :align-to (- right ,hpos)))))
            (cons
             `(align-right menu-item ,str nil)
             (configure-tab-bar-build-formatter configure-tab-bar-modules-right))))

        (tab-bar-mode)
        ,@(if (get-value 'emacs-all-the-icons config)
              `((eval-when-compile
                 (require 'all-the-icons))
                (setq configure-tab-bar-modules-left (list ,@modules-left))
                (setq configure-tab-bar-modules-center (list ,@modules-center))
                (setq configure-tab-bar-modules-right (list ,@modules-right)))
              '())
        (with-eval-after-load 'tab-bar
          (setq tab-bar-format '(configure-tab-bar-format-left
                                 configure-tab-bar-format-center
                                 configure-tab-bar-format-right))
          (setq tab-bar-close-button-show nil)
          (setq tab-bar-show t)))
      #:elisp-packages (if (get-value 'emacs-all-the-icons config)
                           (list (get-value 'emacs-all-the-icons config))
                           '())
      #:summary "Extensions to Emacs's Tab Bar"
      #:commentary "Provide extensions to the Emacs Tab Bar, supplying custom menu items
in the form of modules to be displayed.")))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-comint)
  "Configure the general command-intepreter-in-a-buffer (comint) for
process-in-a-buffer derived packages like shell, REPLs, etc."

  (define emacs-f-name 'comint)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Comint."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `(,@(if (get-value 'emacs-consult-initial-narrowing? config)
              '((defvar configure-comint-buffer-source
                  `(:name "Comint"
                          :narrow ?c
                          :category buffer
                          :preview-key ,(kbd "M-.")
                          :state ,'consult--buffer-state
                          :items ,(lambda ()
                                    (mapcar 'buffer-name (configure-completion--mode-buffers
                                                          'comint-mode 'cider-repl-mode))))
                  "Source for `comint-mode' buffers to be set in `consult-buffer-sources'.")
                (add-to-list 'consult-buffer-sources configure-comint-buffer-source)
                (add-to-list 'configure-completion-initial-narrow-alist '(comint-mode . ?c)))
              '())
        (add-hook 'comint-preoutput-filter-functions 'ansi-color-apply nil t)
        (add-hook 'comint-mode 'capf-autosuggest-mode)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-shell)
  "Configure shell-scripting tooling for Emacs."

  (define emacs-f-name 'shell)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to shell-scripting in Emacs."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load 'sh-script
          (setq sh-basic-offset 2)
          (setq sh-indentation 2)
          (setq smie-indent-basic 2)
          (setq sh-indent-comment nil)
          (setq sh-first-lines-indent nil))
        (add-to-list 'display-buffer-alist
                     `(,(rx "*Async Shell Command" (* any) "*")
                       (display-buffer-no-window)))
        (with-eval-after-load 'ob-core
          (require 'ob-shell)
          (add-to-list 'org-structure-template-alist '("sh" . "src sh")))
        ,@(if (get-value 'emacs-project config)
              '((define-key project-prefix-map "s" 'project-shell)
                (add-to-list 'project-switch-commands '(project-shell "Start an inferior shell")))
              '())))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-eshell
          #:key
          (emacs-eshell-syntax-highlighting
           emacs-eshell-syntax-highlighting)
          (emacs-eshell-prompt-extras
           emacs-eshell-prompt-extras))
  "Configure Eshell, a shell-like command interpreter
implemented in Emacs Lisp."

  (define emacs-f-name 'eshell)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Eshell."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((defgroup configure-shell nil
          "Eshell customizations for a better integration in Emacs."
          :group 'configure)

        (defun configure-eshell--bookmark-make-record ()
          "Create a bookmark for an `eshell-mode' buffer."
          (let ((eshell-buffer-name (with-current-buffer (current-buffer)
                                      (substring-no-properties (buffer-name))))
                (bookmark `(,eshell-buffer-name
                            ,@(bookmark-make-record-default 'no-file)
                            ,(cons 'handler 'configure-eshell-bookmark-jump)
                            ,(cons 'filename default-directory))))
            bookmark))

        (defun configure-eshell-bookmark-jump (bookmark)
          "Jump to BOOKMARK in an Eshell buffer."
          (when-let ((eshell-buffer-name eshell-buffer-name))
            (eshell)
            (setq default-directory (alist-get 'filename bookmark))
            (eshell-reset)))

        (defun configure-eshell-bookmark-handler ()
          "Set up corresponding `bookmark-make-record-function' for `eshell-mode' buffers."
          (setq-local bookmark-make-record-function 'configure-eshell-bookmark-make-record))

        (define-minor-mode configure-eshell-mode-setup
          "Set up environment on `eshell-mode' invocation."
          :global t :group 'configure-shell
          (if configure-eshell-mode-setup
              (progn
                (setenv "PAGER" "")
                (define-key eshell-mode-map (kbd "C-c L") 'eshell/clear)
                (define-key eshell-hist-mode-map (kbd "M-r") 'consult-history))
            (local-unset-key 'eshell/clear)))

        (add-hook 'eshell-mode-hook 'configure-eshell-mode-setup)
        (add-hook 'eshell-mode-hook 'configure-eshell-bookmark-handler)
        (with-eval-after-load 'eshell
          (setq eshell-banner-message "\n\n")
          (autoload 'eshell-syntax-highlighting-global-mode "eshell-syntax-highlighting")
          (eshell-syntax-highlighting-global-mode))
        (with-eval-after-load 'em-prompt
          (autoload 'epe-theme-lambda "eshell-prompt-extras")
          (setq eshell-prompt-function 'epe-theme-lambda)
          (setq eshell-highlight-prompt nil)))
      #:elisp-packages (list emacs-eshell-syntax-highlighting
                             emacs-eshell-prompt-extras))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))


;;;
;;; Communication
;;;

(define* (feature-emacs-telega
         #:key
         (emacs-telega emacs-telega))
  "Configure the Telega.el Emacs client."
  (ensure-pred any-package? emacs-telega)

  (define emacs-f-name 'telega)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to the Telega.el client."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'configure-rde-keymaps)
        ,@(if (get-value 'emacs-consult-initial-narrowing? config)
              '((defvar configure-telega-buffer-source
                        `(:name "Telega"
                          :narrow ?t
                          :category buffer
                          :preview-key ,(kbd "M-.")
                          :state ,'consult--buffer-state
                          :items ,(lambda () (mapcar 'buffer-name
                                                     (configure-completion--mode-buffers
                                                      'telega-chat-mode 'telega-root-mode))))
                        "Source for Telega buffers to be set in `consult-buffer-sources'.")
                (add-to-list 'consult-buffer-sources configure-telega-buffer-source)
                (add-to-list 'configure-completion-initial-narrow-alist '(telega-root-mode . ?t))
                (add-to-list 'configure-completion-initial-narrow-alist '(telega-chat-mode . ?t)))
              '())
        (let ((map rde-app-map))
          (define-key map "t" 'telega))
        (add-hook 'telega-load-hook 'telega-notifications-mode)
        (autoload 'telega-root--buffer "telega")
        (setq telega-directory (expand-file-name "telega" user-emacs-directory))
        (with-eval-after-load 'telega
          (setq telega-completing-read-function 'completing-read)))
      #:elisp-packages (list emacs-telega
                             (get-value 'emacs-configure-rde-keymaps config)))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-telega)))
   (home-services-getter get-home-services)))


;;;
;;; Development
;;;

(define* (feature-emacs-smartparens
          #:key
          (emacs-smartparens emacs-smartparens)
          (extra-sp-lisp-modes '())
          (show-smartparens? #f)
          (paredit-bindings? #f))
  "Configure smartparens, a minor mode to deal with pairs in Emacs."
  (ensure-pred file-like? emacs-smartparens)
  (ensure-pred list? extra-sp-lisp-modes)
  (ensure-pred boolean? show-smartparens?)
  (ensure-pred boolean? paredit-bindings?)

  (define emacs-f-name 'smartparens)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to smartparens."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `(,@(if paredit-bindings?
              '((autoload 'sp-use-paredit-bindings "smartparens")
                (sp-use-paredit-bindings))
              '((autoload 'sp-use-smartparens-bindings "smartparens")
                (sp-use-smartparens-bindings)))
        (with-eval-after-load 'paren
          (setq show-paren-style 'mixed))
        ,@(if show-smartparens?
              '((show-paren-mode 0)
                (show-smartparens-global-mode))
              '((show-paren-mode)))
        (with-eval-after-load 'smartparens
          (define-key smartparens-mode-map (kbd "M-s") nil)
          (setq sp-highlight-pair-overlay nil)
          (mapcar (lambda (mode)
                    (add-to-list 'sp-lisp-modes mode))
                  ',extra-sp-lisp-modes)
          (dolist (command '(smartparens-mode smartparens-strict-mode))
            (mapc (lambda (hook) (add-hook (intern (format "%s-hook" hook)) command)) sp-lisp-modes))
          (require 'smartparens-config)))
      #:elisp-packages (list emacs-smartparens))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-smartparens)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-xref)
  "Configure Xref, an Emacs mechanism to find definitions
and references in your programs."
  (define emacs-f-name 'xref)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Xref."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load 'xref
          (setq xref-auto-jump-to-first-definition 'move)
          (setq xref-auto-jump-to-first-xref 'move)
          (setq xref-prompt-for-identifier '(not xref-find-definitions-other-window
                                                 xref-find-definitions-other-frame))
          ,@(if (get-value 'emacs-consult config)
                '((setq xref-show-xrefs-function 'consult-xref)
                  (setq xref-show-definitions-function 'consult-xref))
                '()))))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-re-builder
          #:key
          (re-syntax 'rx))
  "Configure re-builder, an Emacs mode to build
Regexps with visual feedback."
  (ensure-pred symbol? re-syntax)

  (define emacs-f-name 're-builder)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to re-builder."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'configure-rde-keymaps)
        (define-key rde-app-map "r" 're-builder)
        (with-eval-after-load 're-builder
          (setq reb-re-syntax ',re-syntax)
          (setq reb-blink-delay 0)
          (setq reb-auto-match-limit nil)))
      #:elisp-packages (list (get-value 'emacs-configure-rde-keymaps config)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-elisp)
  "Configure tooling for Emacs Lisp, the programming
language for GNU Emacs."

  (define emacs-f-name 'elisp)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Emacs Lisp."
    (list
     (simple-service
      'elisp-tempel-templates
      home-emacs-tempel-service-type
      `(,#~"emacs-lisp-mode"
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
           (command "(defun " p " (" p ")\n  \"" "\"" n> "(interactive" p ")" n> r> ")")
           (advice "(defun " (p "adv" name) " (&rest app)" n> p n> "(apply app))" n>
                   "(advice-add #'" (p "fun") " " (p ":around") " #'" (s name) ")")
           (provide "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
                    ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name))) " ends here" n)))
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((require 'configure-rde-keymaps)
        (with-eval-after-load 'elisp-mode
          (let ((map emacs-lisp-mode-map))
            (define-key map (kbd "C-x C-e") 'pp-eval-last-sexp)
            (define-key map (kbd "M-:") 'pp-eval-expression)
            (define-key map (kbd "C-c C-m") 'pp-macroexpand-last-sexp)
            (define-key map (kbd "C-c C-b") 'eval-buffer)
            ,@(if (get-value 'emacs-embark config)
                  '((define-key map (kbd "C-c C-c") 'embark-pp-eval-defun))
                  '())))
        (define-key rde-app-map "I" 'ielm)
        (with-eval-after-load 'ielm
          (setq ielm-header "")
          (setq ielm-noisy nil))
        (with-eval-after-load 'org
          (add-to-list 'org-structure-template-alist '("el" . "src elisp")))
        (with-eval-after-load 'ob-core
          (setq org-babel-default-header-args:elisp
                '((:lexical . "t")
                  (:results . "scalar")))))
      #:elisp-packages (list (get-value 'emacs-configure-rde-keymaps config)))))

  (feature
   (name f-name)
   (values `((,f-name . #t)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-rainbow-delimiters
          #:key
          (emacs-rainbow-delimiters emacs-rainbow-delimiters)
          (rainbow-delimiters-hooks '(prog-mode-hook)))
  "Configure rainbow-delimiters, an Emacs mode that highlights character pairs."
  (ensure-pred file-like? emacs-rainbow-delimiters)
  (ensure-pred list? rainbow-delimiters-hooks)

  (define emacs-f-name 'rainbow-delimiters)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to rainbow-delimiters."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((mapcar (lambda (hook)
                  (add-hook hook 'rainbow-delimiters-mode))
                ',rainbow-delimiters-hooks))
      #:elisp-packages (list emacs-rainbow-delimiters))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-rainbow-delimiters)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-yaml
          #:key
          (emacs-yaml-mode emacs-yaml-mode))
  "Configure yaml-mode, an Emacs major mode to edit files in the
YAML serialization format."
  (ensure-pred file-like? emacs-yaml-mode)

  (define emacs-f-name 'yaml)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to yaml-mode."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((add-to-list 'auto-mode-alist '("\\.y[a]?ml\\'" . yaml-mode))
        (with-eval-after-load 'yaml-mode
          (define-key yaml-mode-map (kbd "RET") 'newline-and-indent)))
      #:elisp-packages (list emacs-yaml-mode))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-yaml-mode)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-lang-web
          #:key
          (emacs-json-mode emacs-json-mode))
  "Configure various text modes in Emacs for editing
web-related languages."
  (ensure-pred file-like? emacs-json-mode)

  (define emacs-f-name 'web)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to web languages."
    (list
     (simple-service
      'home-npm-environment-variables
      home-environment-variables-service-type
      '(("npm_config_userconfig" . "$XDG_CONFIG_HOME/npm-config")
        ("npm_config_cache" . "$XDG_CACHE_HOME/npm")
        ("npm_config_prefix" . "$XDG_DATA_HOME/npm/bin")
        ("PATH" . "$XDG_DATA_HOME/npm/bin:$PATH")))
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((with-eval-after-load 'js
         (setq js-indent-level 2))
        (with-eval-after-load 'css-mode
          (setq css-indent-offset 2))
        (with-eval-after-load 'mhtml-mode
          (define-key html-mode-map (kbd "M-o") 'ace-window)))
      #:elisp-packages (list emacs-json-mode))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-json-mode)))
   (home-services-getter get-home-services)))

(define* (feature-emacs-polymode
          #:key
          (emacs-polymode emacs-polymode)
          (emacs-polymode-markdown emacs-polymode-markdown)
          (emacs-polymode-org emacs-polymode-org))
  "Configure Polymode, a framework for multiple major modes in Emacs."
  (ensure-pred file-like? emacs-polymode)
  (ensure-pred file-like? emacs-polymode-markdown)
  (ensure-pred file-like? emacs-polymode-org)

  (define emacs-f-name 'polymode)
  (define f-name (symbol-append 'emacs- emacs-f-name))

  (define (get-home-services config)
    "Return home services related to Polymode."
    (list
     (rde-elisp-configuration-service
      emacs-f-name
      config
      `((eval-when-compile
         (require 'eieio))
        (defun configure-polymode-set-poly-block-faces ()
          "Correctly set a fixed pitch face for polymode source blocks."
          (interactive)
          (let* ((fix-pitch (face-attribute 'fixed-pitch :family))
                 (fix-font (face-attribute 'fixed-pitch :font))
                 (fix-height (face-attribute 'fixed-pitch :height))
                 (props `(:extend t
                          :height ,fix-height
                          :family ,fix-pitch
                          :font ,fix-font)))
            (oset pm/chunkmode adjust-face props)))
        ;; (add-hook 'polymode-init-inner-hook 'configure-polymode-set-poly-block-faces)
        )
      #:elisp-packages (list emacs-polymode
                             ;; emacs-polymode-org
                             emacs-polymode-markdown))))

  (feature
   (name f-name)
   (values `((,f-name . ,emacs-polymode)))
   (home-services-getter get-home-services)))
