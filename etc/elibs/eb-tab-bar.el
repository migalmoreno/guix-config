;; -*- lexical-binding: t; -*-
(require 'eb-desktop)
(require 'eb-mpv)
(require 'tab-bar)
(require 'all-the-icons)

(defgroup eb-tab-bar nil
  "Tab bar customizations."
  :group 'eb)

(defvar-local eb-tab-bar-notifications
  (list
   '(:eval (eb-desktop--notify)))
  "Display desktop notifications in the tab bar.")

(defvar eb-tab-bar-format-separator '(separator menu-item " " nil)
  "Separator to be used inside menu item blocks.")

;;;###autoload
(defun eb-tab-bar-format-left ()
  "Produce the items for the tab bar to output on its left-hand side."
  `((menu-bar menu-item ,(format " %s " (all-the-icons-fileicon "emacs" :v-adjust -0.1))
              tab-bar-menu-bar :help "Menu")
    (mpv-string menu-item ,eb-mpv-mode-line-string nil)
    (mpv-prev menu-item ,eb-mpv-prev-button
              mpv-playlist-prev :help "Previous playlist entry")
    (mpv-toggle menu-item ,eb-mpv-toggle-button
                mpv-pause :help "Toggle playback")
    (mpv-next menu-item ,eb-mpv-next-button
              mpv-playlist-next :help "Next playlist entry")
    (mpv-playing-time menu-item ,eb-mpv-playing-time-string nil)
    (notifications menu-item ,(string-trim-right (format-mode-line eb-tab-bar-notifications)) nil)))

;;;###autoload
(defun eb-tab-bar-format-center ()
  "Produce menu items to display information in the center of the tab bar."
  (let ((str (concat
              (propertize " " 'display
                          `(space :align-to
                                  (- center
                                     ,(/ (length display-time-string)
                                         2.0)))))))
    `((align-center menu-item ,str nil)
      (time menu-item ,display-time-string nil))))

;;;###autoload
(defun eb-tab-bar-format-align-right ()
  "Align the rest of tab bar items to the right bearing in mind Unicode characters."
  (let* ((rest (cdr (memq 'eb-tab-bar-format-align-right tab-bar-format)))
         (rest (tab-bar-format-list rest))
         (rest (mapconcat (lambda (item) (nth 2 item)) rest ""))
         (hpos (+ 5 (length rest)))
         (str (propertize " " 'display `(space :align-to (- right ,hpos) :height 1.3))))
    `((align-right menu-item ,str nil))))

;;;###autoload
(defun eb-tab-bar-format-right ()
  "Produce menu items corresponding to the right side of the tab bar."
  `((org-timer menu-item ,(when (boundp 'org-timer-mode-line-string)
                            org-timer-mode-line-string)
               nil)
    (appointments menu-item ,(when (boundp 'appt-mode-string)
                               appt-mode-string)
                  nil)
    (weather menu-item ,display-wttr-string nil)
    (volume menu-item ,eb-desktop-display-volume-string nil)
    (battery menu-item ,(when (boundp 'battery-mode-line-string)
                          battery-mode-line-string)
             nil)))

(provide 'eb-tab-bar)
