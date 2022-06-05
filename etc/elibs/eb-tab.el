;; -*- lexical-binding: t; -*-
(require 'eb-desktop)
(require 'eb-media)
(require 'eb-look)
(require 'tab-bar)

(defgroup eb-tab nil
  "Tab bar customizations."
  :group 'eb)

(defvar-local eb-tab-notifications
  (list
   '(:eval (eb-desktop--notify)))
  "Displays desktop notifications in the tab bar.")

(defvar eb-tab-format-separator '(separator menu-item " " nil)
  "Separator to be used inside menu item blocks.")

;;;###autoload
(defun eb-tab-format-left ()
  "Produces the items for the tab bar to output on its left-hand side."
  `((menu-bar menu-item " λ " tab-bar-menu-bar :help "Menu")
    (mpv-string menu-item ,eb-media-mpv-mode-line-string nil)
    (mpv-prev menu-item ,(eb-look--position-item eb-media-mpv-prev-button)
              mpv-playlist-prev :help "Previous playlist entry")
    (mpv-toggle menu-item ,(eb-look--position-item eb-media-mpv-toggle-button)
                mpv-pause :help "Toggle playback")
    (mpv-prev menu-item ,(eb-look--position-item eb-media-mpv-next-button)
              mpv-playlist-next :help "Next playlist entry")
    ,eb-tab-format-separator
    (emms-string menu-item ,(when (boundp 'emms-mode-line-string)
                              emms-mode-line-string)
                 nil)
    (emms-playing-time menu-item ,(when (boundp 'emms-playing-time-string)
                                    emms-playing-time-string)
                       nil)
    (org-timer menu-item ,(when (boundp 'org-timer-mode-line-string)
                            org-timer-mode-line-string)
               nil)
    (notifications menu-item ,(string-trim-right (format-mode-line eb-tab-notifications)) nil)
    (appointments menu-item ,(when (boundp 'appt-mode-string)
                               appt-mode-string)
                  nil)))

;;;###autoload
(defun eb-tab-format-center ()
  "Produces menu items to display information in the center of the tab bar."
  (let ((str (concat
              (propertize " " 'display
                          `(space :align-to
                                  (- center
                                     ,(/ (length display-time-string)
                                         2.0)))))))
    `((align-center menu-item ,str nil)
      (time menu-item ,display-time-string nil))))

;;;###autoload
(defun eb-tab-format-align-right ()
  "Align the rest of tab bar items to the right bearing in mind Unicode characters."
  (let* ((rest (cdr (memq 'eb-tab-format-align-right tab-bar-format)))
         (rest (tab-bar-format-list rest))
         (rest (mapconcat (lambda (item) (nth 2 item)) rest ""))
         (hpos (+ 5 (length rest)))
         (str (propertize " " 'display `(space :align-to (- right ,hpos)))))
    `((align-right menu-item ,str nil))))

;;;###autoload
(defun eb-tab-format-right ()
  "Produces menu items corresponding to the right side of the tab bar."
  `((weather menu-item ,eb-desktop-display-weather-string nil)
    (volume menu-item ,eb-desktop-display-volume-string nil)
    (battery menu-item ,(when (boundp 'battery-mode-line-string)
                          battery-mode-line-string)
             nil)
    ;; TODO: instead of `keycast-tab-bar-mode'
    ;; (keycast menu-item ,(keycast-tab-bar) nil)
    ))

(provide 'eb-tab)
