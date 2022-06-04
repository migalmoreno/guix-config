(define-module (quasar home services documentation)
  #:use-module (efimerspan home services emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages scheme)
  #:use-module (gnu services)
  #:use-module (gnu home-services base)
  #:use-module (guix gexp)
  #:export (documentation-service))

(define (documentation-service)
  (list
   (home-generic-service 'home-documentation-packages
                         #:packages (list sicp))
   (elisp-configuration-service
    `((with-eval-after-load 'info
        (define-key Info-mode-map "q" 'kill-this-buffer)
        (custom-set-variables
         '(Info-use-header-line nil))
        (require 'info+)
        (add-hook 'Info-mode-hook 'visual-line-mode)
        (add-hook 'Info-mode-hook 'eb-look-set-variable-pitched)
        (custom-set-variables
         '(info-manual+node-buffer-name-mode t)
         '(Info-persist-history-mode t)
         '(Info-fontify-isolated-quote-flag nil)
         '(Info-fontify-reference-items-flag t)
         '(Info-fontify-quotations t)))
      ,#~""
      (let ((map help-map))
        (define-key map "f" 'helpful-callable)
        (define-key map "v" 'helpful-variable)
        (define-key map "o" 'helpful-at-point)
        (define-key map "k" 'helpful-key)
        (define-key map "F" 'helpful-function)
        (define-key map "C" 'helpful-command)
        (define-key map "b" 'embark-bindings)
        (define-key map "a" 'consult-apropos)
        (define-key map "z" 'describe-face))
      (add-hook 'helpful-mode-hook 'visual-line-mode)
      (with-eval-after-load 'helpful
        (define-key helpful-mode-map "q" 'kill-this-buffer))
      ,#~""
      (with-eval-after-load 'help-mode
        (define-key help-mode-map "q" 'kill-current-buffer)
        (custom-set-variables
         '(help-window-select t)))
      ,#~""
      (which-key-mode)
      (with-eval-after-load 'which-key
        (custom-set-variables
         '(which-key-idle-delay 0.3)))
      ,#~""
      (define-key mode-specific-map "dw" 'woman)
      (add-hook 'woman-mode-hook 'toggle-truncate-lines)
      (define-key mode-specific-map "dm" 'man)
      (define-key mode-specific-map "dM" 'consult-man)
      ,#~""
      (define-key mode-specific-map "K" 'keycast-mode))
    #:elisp-packages (list emacs-info-plus
                           emacs-helpful
                           emacs-which-key
                           emacs-keycast))))
