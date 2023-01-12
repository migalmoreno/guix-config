(define-module (conses features fontutils)
  #:use-module (conses home services fonts)
  #:use-module (conses packages emacs-xyz)
  #:use-module (rde features)
  #:use-module (rde features emacs)
  #:use-module (rde features predicates)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-9)
  #:export (font-specification
            feature-fonts
            font-name
            font-default-size
            font-docked-size
            font-headless-size
            font-weight
            font-package
            font?
            make-font
            font))

(define-record-type* <font> font
  make-font
  font?
  this-font
  (name font-name)
  (default-size font-default-size)
  (docked-size
   font-docked-size
   (thunked)
   (default
    (font-default-size this-font)))
  (headless-size
   font-headless-size
   (thunked)
   (default
    (font-default-size this-font)))
  (weight font-weight (default 'regular))
  (package font-package))

(define (font-specification font)
  "Convert <font> record to string."
  (string-join (list
                (font-name font)
                (string-capitalize (symbol->string (font-weight font)))
                (number->string (font-default-size font)))
               " "))

(define* (feature-fonts
          #:key
          (default-font-size 11)
          (font-sans
           (font
            (name "IBM Plex Sans")
            (default-size default-font-size)
            (package font-ibm-plex)
            (weight 'light)))
          (font-serif
           (font
            (name "IBM Plex Serif")
            (default-size default-font-size)
            (package font-ibm-plex)))
          (font-monospace
           (font
            (name "Iosevka")
            (default-size default-font-size)
            (docked-size 10.7)
            (headless-size 10.5)
            (package font-iosevka)))
          (font-unicode
           (font
            (name "Noto Color Emoji")
            (default-size default-font-size)
            (package font-google-noto)))
          (emacs-fontaine emacs-fontaine)
          (extra-fontaine-presets '())
          (default-fontaine-preset 'docked))
  "Configure user-space fonts."
  (ensure-pred integer? default-font-size)
  (ensure-pred font? font-sans)
  (ensure-pred font? font-serif)
  (ensure-pred font? font-monospace)
  (ensure-pred font? font-unicode)
  (ensure-pred file-like? emacs-fontaine)
  (ensure-pred list? extra-fontaine-presets)
  (ensure-pred symbol? default-fontaine-preset)

  (define f-name 'fonts)

  (define (get-home-services config)
    "Return home services related to fonts."
    (list
     (simple-service
      'home-fonts-profile-service
      home-profile-service-type
      (list (font-package font-unicode)))
     (rde-elisp-configuration-service
      f-name
      config
      `((require 'xdg)
        (eval-when-compile
         (require 'cl-macs))
        (defun rde-fonts--build-emojis ()
          "Create an emoji list by looping over the corresponding range of characters."
          (delete
           nil
           (cl-loop with range = '(#x1f000 . #x1f9ff)
                    for i upto (- (cdr range) (car range))
                    collect (when-let* ((codepoint (+ (car range) i))
                                        (name (get-char-code-property codepoint 'name)))
                              (thread-last
                                (replace-regexp-in-string " " "-" (downcase name))
                                (format ":%s:")
                                (format "%s %s" (char-to-string (char-from-name name))))))))
        (defvar rde-fonts-emoji-list nil
          "Cached list of emojis.")
        (defun rde-fonts-insert-emoji ()
          "Insert an emoji character to the current buffer."
          (interactive)
          (thread-first
            (completing-read
             "Select emoji: "
             (or rde-fonts-emoji-list
                 (setq rde-fonts-emoji-list (rde-fonts--build-emojis))))
            (substring 0 1)
            (insert)))

        (define-key search-map "e" 'rde-fonts-insert-emoji)
        (define-key minibuffer-mode-map (kbd "C-c C-e") 'rde-fonts-insert-emoji)
        (with-eval-after-load 'fontset
          (set-fontset-font t 'symbol ,(font-name font-unicode) nil 'append)
          (set-fontset-font t 'unicode ,(font-name font-unicode) nil 'append)
          (set-fontset-font "fontset-default" nil (font-spec :name ,(font-name font-unicode))))
        (setq use-default-font-for-symbols nil)
        (require 'fontaine)
        (setq fontaine-presets
              '((docked
                 :default-family ,(font-name font-monospace)
                 :default-height ,(inexact->exact (* (font-docked-size font-monospace) 10))
                 :fixed-pitch-family ,(font-name font-monospace)
                 :fixed-pitch-height 1.0
                 :variable-pitch-family ,(font-name font-sans)
                 :variable-pitch-height 1.0
                 :variable-pitch-weight ,(font-weight font-sans))
                (headless
                 :default-family ,(font-name font-monospace)
                 :default-height ,(inexact->exact (* (font-headless-size font-monospace) 10))
                 :fixed-pitch-family ,(font-name font-monospace)
                 :fixed-pitch-height 1.0
                 :variable-pitch-family ,(font-name font-sans)
                 :variable-pitch-height 1.0
                 :variable-pitch-weight ,(font-weight font-sans))
                ,@extra-fontaine-presets))
        (setq fontaine-latest-state-file (expand-file-name "emacs/fontaine-latest.state.eld" (xdg-cache-home)))
        (when (display-graphic-p)
          (fontaine-set-preset ',default-fontaine-preset)))
      #:elisp-packages (list emacs-fontaine))
     (service home-font-service-type
              (home-font-configuration
               (sans-serif (make-font-spec (font-package font-sans) (font-name font-sans)))
               (serif (make-font-spec (font-package font-serif) (font-name font-serif)))
               (monospace (make-font-spec (font-package font-monospace) (font-name font-monospace)))))))

  (feature
   (name f-name)
   (values
    (append
     `((,f-name . #t)
       (emacs-fontaine . ,emacs-fontaine))
     (make-feature-values font-sans font-monospace
                          font-serif font-unicode)))
   (home-services-getter get-home-services)))
