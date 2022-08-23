;; -*- lexical-binding: t; -*-
(defgroup eb-modus-themes nil
  "Minor nits related to `modus-themes'."
  :group 'eb)

;;;###autoload
(defun eb-modus-themes-set-theme-dependent-faces ()
  "Set faces based on the current theme in `modus-themes'."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-vivendi)
      (progn
        (set-face-attribute 'tab-bar nil :box '(:line-width 1 :color "#a8a8a8" :style unspecified))
        (set-face-attribute 'vertical-border nil :foreground "#000000"))
    (set-face-attribute 'tab-bar nil :box '(:line-width 1 :color "#505050" :style unspecified))
    (set-face-attribute 'vertical-border nil :foreground "#ffffff")))

;;;###autoload
(defun eb-modus-themes-set-info-faces ()
  "Apply some extra appearance settings to `Info-mode' and `Info+-mode'."
  (interactive)
  (face-remap-add-relative 'default :inherit 'variable-pitch)
  (when (eq (car custom-enabled-themes) 'modus-vivendi)
    (set-face-attribute 'info-reference-item nil :background 'unspecified :foreground "#00d3d0")
    (set-face-attribute 'info-function-ref-item nil :background 'unspecified :foreground "#b6a0ff")
    (set-face-attribute 'info-quoted-name nil :foreground "#b0d6f5")
    (set-face-attribute 'info-double-quoted-name nil :foreground "#79a8ff")
    (set-face-attribute 'info-xref nil :foreground "#00bcff" :underline t)
    (set-face-attribute 'info-command-ref-item nil :background 'unspecified)
    (set-face-attribute 'info-macro-ref-item nil :background 'unspecified)
    (set-face-attribute 'info-variable-ref-item nil :background 'unspecified)
    (set-face-attribute 'info-string nil :foreground "#79a8ff")))

(provide 'eb-modus-themes)
