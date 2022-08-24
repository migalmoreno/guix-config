(defgroup eb-markdown nil
  "Markdown mode customizations."
  :group 'eb)

;;;###autoload
(define-minor-mode eb-markdown-minimal-mode
  "Set a more minimal interface to Markdown mode."
  :global t :group 'eb-markdown
  (if eb-markdown-minimal-mode
      (progn
        (display-line-numbers-mode -1)
        (visual-line-mode 1))
    (display-line-numbers-mode 1)
    (visual-line-mode -1)))

(provide 'eb-markdown)
