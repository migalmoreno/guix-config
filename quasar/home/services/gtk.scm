(define-module (quasar home services gtk)
  #:use-module (conses home services gtk)
  #:use-module (gnu home-services base)
  #:use-module (gnu services)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages gnome)
  #:export (gtk-service))

(define (gtk-service)
  (list
   (home-generic-service 'home-gtk-packages #:packages (list gsettings-desktop-schemas
                                                             papirus-icon-theme))
   (service home-gtk-service-type
            (home-gtk-configuration
             (settings
              `((Settings
                 ((gtk-application-prefer-dark-theme . ,(not (gtk-theme-light-p)))
                  (gtk-cursor-blink . #f)
                  (gtk-cursor-theme-name . "Bibata Classic")
                  (gtk-cursor-theme-size . 16)
                  (gtk-decoration-layout . "")
                  (gtk-dialogs-use-header . #f)
                  (gtk-enable-animations . #f)
                  (gtk-enable-event-sounds . #f)
                  (gtk-enable-input-feedback-sounds . #f)
                  (gtk-error-bell . #f)
                  (gtk-font-name . "Iosevka, 10")
                  (gtk-icon-theme-name . "Papirus-Dark")
                  (gtk-overlay-scrolling . #t)
                  (gtk-recent-files-enabled . #f)
                  (gtk-shell-shows-app-menu . #f)
                  (gtk-shell-shows-desktop . #f)
                  (gtk-shell-shows-menubar . #f)
                  (gtk-xft-antialias . #t)
                  (gtk-xft-dpi . 92)
                  (gtk-xft-hinting . #t)
                  (gtk-xft-hintstyle . hintfull)
                  (gtk-xft-rgba . none)))))
             (theme
              (let ((bg (if (gtk-theme-light-p) 'white 'black))
                    (fg (if (gtk-theme-light-p) 'black 'white))
                    (accent-bg (string->symbol (if (gtk-theme-light-p) "#d0d6ff" "#afafef")))
                    (accent-fg (if (gtk-theme-light-p) 'black 'white))
                    (secondary-bg (string->symbol (if (gtk-theme-light-p) "#f8f8f8" "#323232")))
                    (secondary-fg (if (gtk-theme-light-p) 'black 'white))
                    (tertiary-bg (string->symbol (if (gtk-theme-light-p) "#f0f0f0" "#6272a4")))
                    (selection (string->symbol (if (gtk-theme-light-p) "#e8dfd1" "#afafef"))))
                `(((box button:hover)
                   ((background . ,secondary-bg)
                    (color . ,fg)
                    (outline . none)
                    (box-shadow . none)))
                  ((widget button)
                   ((background . none)
                    (border . none)
                    (color . ,fg)
                    (box-shadow . none)))
                  ((widget button:hover)
                   ((background . ,secondary-bg)))
                  (#((stack box > #{button:not(:checked)}#)
                     (.titlebar #{button:not(:checked)}#)
                     (row #{button:not(:checked)}#)
                     #{button.file:not(:checked)}#
                     #{button.lock:not(:checked)}#
                     #{button.image-button:not(:checked)}#
                     #{button.text-button:not(:checked)}#
                     #{button.toggle:not(:checked)}#
                     #{button.slider-button:not(:checked)}#)
                   ((-gtk-icon-shadow . none)
                    (background . none)
                    (color . ,fg)
                    (border . (1px solid ,secondary-bg))
                    (outline . none)
                    (box-shadow . none)
                    (text-shadow . none)))
                  (#((stack box #{button:hover:not(:checked)}#)
                     (.titlebar #{button:hover:not(:checked)}#)
                     (row #{button:hover:not(:checked)}#)
                     #{button.file:hover:not(:checked)}#
                     #{button.lock:hover:not(:checked)}#
                     #{button.image-button:hover:not(:checked)}#
                     #{button.slider-button:hover:not(:checked)}#)
                   ((background . none)))
                  (button:checked
                   ((background . ,tertiary-bg)
                    (color . ,fg)
                    (border . (1px solid ,tertiary-bg))))
                  (image
                   ((color . ,fg)))
                  (image:disabled
                   ((color . ,secondary-fg)))
                  ((row:selected image)
                   ((color . ,tertiary-bg)))
                  ((row:selected button:hover)
                   ((background . none)))
                  ((button image)
                   ((-gtk-icon-effect . none)
                    (-gtk-icon-shadow . none)
                    (color . ,fg)))
                  ((headerbar button.toggle)
                   ((border-radius . 20px)))
                  (#((headerbar button.toggle)
                     (stack stackswitcher button:checked)
                     radio:checked
                     (header button))
                   ((background . ,tertiary-bg)
                    (color . ,fg)
                    (text-shadow . none)
                    (border . (1px solid ,tertiary-bg))
                    (box-shadow . none)))
                  (#((button.color colorswatch)
                     (colorswatch overlay))
                   ((box-shadow . none)
                    (border . none)))
                  ((filechooser widget button)
                   ((border . (1px solid ,secondary-bg))))
                  (#((combobox entry)
                     (combobox box)
                     (combobox box button))
                   ((background . ,secondary-bg)
                    (color . ,fg)
                    (border . none)
                    (outline . none)
                    (box-shadow . none)))
                  ((combobox button)
                   ((border . none)))
                  ((combobox button:checked)
                   ((background . none)
                    (border . none)))
                  (button.emoji-section:checked
                   ((border . none)))
                  (radiobutton
                   ((outline . none)))
                  ((radiobutton radio)
                   ((color . ,bg)
                    (background . ,bg)
                    (border . none)))
                  ((radiobutton radio:checked)
                   ((background . ,accent-bg)
                    (color . ,secondary-fg)))
                  ((radiobutton box)
                   ((background . none)
                    (border-color . ,fg)))
                  (checkbutton
                   ((outline . none)))
                  (#(check
                     (modelbutton radio))
                   ((background . ,secondary-bg)
                    (color . ,fg)
                    (border . none)))
                  (#((checkbutton check:checked)
                     (modelbutton check:checked)
                     (treeview check:checked)
                     (modelbutton radio:checked))
                   ((background . ,accent-bg)
                    (color . ,accent-fg)))
                  (#((stackswitcher #{button:not(:checked)}#)
                     #{radio:not(:checked)}#)
                   ((background . ,secondary-bg)
                    (color . ,secondary-fg)
                    (border . (1px solid ,secondary-bg))
                    (box-shadow . none)
                    (text-shadow . none)))
                  (switch
                   ((background . ,secondary-bg)))
                  (#(switch switch-slider)
                   ((border-color . ,tertiary-bg)
                    (box-shadow . none)))
                  (switch:checked
                   ((background . ,accent-bg)))
                  ((switch:checked image)
                   ((color . ,accent-fg)))
                  (#((switch slider) (switch slider:disabled))
                   ((background . ,tertiary-bg)))
                  (label
                   ((background . none)
                    (text-shadow . none)))
                  (label.keycap
                   ((box-shadow . (0 -3px ,secondary-bg inset))
                    (border-color . ,secondary-bg)))
                  ((label link:link)
                   ((color . ,accent-bg)
                    (caret-color . ,accent-bg)
                    (text-decoration-color . ,accent-bg)
                    (text-decoration-line . none)))
                  (spinbutton
                   ((box-shadow . none)))
                  ((spinbutton button)
                   ((background . ,secondary-bg)
                    (-gtk-icon-shadow . none)
                    (border . none)))
                  (#{spinbutton:not(:disabled)}#
                   ((background . ,secondary-bg)
                    (border . none)
                    (color . ,fg)))
                  (#(expander (expander title:hover > arrow))
                   ((color . ,fg)))
                  (modelbutton
                   ((outline . none)))
                  (modelbutton.flat:hover
                   ((background . ,secondary-bg)))
                  (window.background
                   ((background . ,bg)
                    (color . ,fg)))
                  (decoration
                   ((background . ,bg)
                    (border-radius . (15px 15px 0 0))
                    (border . none)
                    (padding . 0)))
                  (.titlebar
                   ((background . ,bg)
                    (color . ,fg)
                    (border-color . ,secondary-bg)))
                  (box
                   ((background . ,bg)))
                  ((box label)
                   ((color . ,fg)))
                  ((box frame border)
                   ((border-color . ,secondary-bg)))
                  (#(stack separator (filechooser paned separator))
                   ((background . ,secondary-bg)))
                  ((stack box)
                   ((background . transparent)))
                  (#(viewport list (viewport grid))
                   ((background . ,bg)))
                  ((viewport list row)
                   ((outline . none)
                    (background . none)))
                  ((viewport row:selected)
                   ((color . ,accent-fg)
                    (background . ,accent-bg)
                    (outline . none)))
                  ((viewport row:selected label)
                   ((color . ,accent-fg)))
                  ((viewport #{row:hover:not(:selected)}#)
                   ((background . none)))
                  ((viewport row:selected > box label)
                   ((color . ,accent-fg)))
                  (treeview.view
                   ((background . ,secondary-bg)
                    (color . ,fg)
                    (border-color . ,secondary-bg)
                    (outline . none)))
                  ((treeview:selected treeview:active)
                   ((background . ,accent-bg)
                    (color . ,accent-fg)))
                  ((treeview header button)
                   ((background . ,bg)
                    (border . none)))
                  (scrolledwindow
                   ((border-color . ,secondary-bg)
                    (background . ,tertiary-bg)))
                  (#((scrolledwindow overshoot.top) (scrolledwindow overshoot.bottom))
                   ((outline . none)
                    (box-shadow . none)
                    (background . none)
                    (border . none)))
                  (#((stack scrolledwindow viewport) (window stack widget))
                   ((background . ,bg)
                    (color . ,fg)))
                  (box.view.vertical
                   ((border . none)
                    (box-shadow . none)
                    (background . ,bg)))
                  ((scrolledwindow textview text)
                   ((background . ,bg)
                    (color . ,fg)))
                  (header
                   ((background . ,bg)
                    (border-color . ,secondary-bg)))
                  ((header tabs label)
                   ((color . ,fg)))
                  ((header tabs tab)
                   ((background . ,bg)))
                  (#(notebook.frame
                     (frame.border-ridge border)
                     (frame.border-groove border)
                     (frame.border-outset border)
                     (frame.border-inset border))
                   ((border-color . ,secondary-bg)))
                  ((noteboox box)
                   ((background . ,bg)
                    (color . ,fg)))
                  ((notebook header tabs tab)
                   ((outline . none)))
                  ((header tabs #{tab:not(:checked):hover}#)
                   ((box-shadow . none)))
                  ((notebook header.left tabs tab:checked)
                   ((box-shadow . (-4px 0 ,accent-bg inset))))
                  ((notebook header.right tabs tab:checked)
                   ((box-shadow . (4px 0 ,accent-bg inset))))
                  ((notebook header.bottom tabs tab:checked)
                   ((box-shadow . (0 4px ,accent-bg inset))))
                  ((notebook header.top tabs tab:checked)
                   ((box-shadow . (0 -4px ,accent-bg inset))))
                  ((notebook header.left tabs tab)
                   ((border-right . (1px solid ,secondary-bg))))
                  ((notebook header.right tabs tab)
                   ((border-left . (1px solid ,secondary-bg))))
                  ((notebook header.bottom tabs tab)
                   ((border-top . (1px solid ,secondary-bg))))
                  ((notebook header.top tabs tab)
                   ((border-bottom . (1px solid ,secondary-bg))))
                  (#((searchbar revealer box)
                     (revealer frame.app-notification)
                     (actionbar revealer box))
                   ((background . ,tertiary-bg)
                    (border-color . ,secondary-bg)))
                  (#((searchbar revealer frame)
                     (revealer frame.app-notification))
                   ((background . ,tertiary-bg)
                    (border . (1px solid ,secondary-bg))))
                  (#((searchbar revealer box)
                     (revealer frame.app-notification)
                     (actionbar revealer box))
                   ((background . ,tertiary-bg)
                    (border-color . ,secondary-bg)))
                  (toolbar
                   ((background . ,bg)
                    (color . ,fg)))
                  (paned
                   ((background . ,bg)
                    (color . ,fg)))
                  (paned
                   ((background . ,bg)))
                  ((flowboxchild grid)
                   ((border-color . ,secondary-bg)))
                  (#(menu .menu .context-menu)
                   ((margin . 0)
                    (padding . 0)
                    (box-shadow . none)
                    (background-color . ,bg)
                    (border . (1px solid ,tertiary-bg))))
                  (#((.csd menu)
                     (.csd .menu)
                     (.csd .context-menu))
                   ((border . none)))
                  (#((menu menuitem)
                     (.menu menuitem)
                     (.context-menu menuitem))
                   ((transition . (background-color 75ms #{cubic-bezier(0, 0, 0.2, 1)}#))
                    (min-height . 20px)
                    (min-width . 40px)
                    (padding . (4px 8px))
                    (color . ,fg)
                    (font . initial)
                    (text-shadow . none)))
                  (#((.menu menuitem:hover)
                     (.menu menuitem:hover)
                     (.context-menu menuitem:hover))
                   ((transition . none)
                    (background-color . #{alpha(currentColor, 0.08)}#)))
                  (#((menu menuitem:disabled)
                     (.menu menuitem:disabled)
                     (.context-menu menuitem:disabled))
                   ((color . #{alpha(currentColor, 0.5)}#)))
                  (menubar
                   ((background . ,bg)
                    (color . ,fg)))
                  ((menubar submenu)
                   ((padding . 10px)))
                  (scrollbar
                   ((background . ,bg)
                    (border . none)))
                  ((scrollbar slider)
                   ((background . ,tertiary-bg)
                    (min-width . 6px)
                    (min-height . 6px)
                    (border . (1px solid ,fg))))
                  (calendar
                   ((background-color . ,bg)
                    (color . ,fg)
                    (margin . 0)
                    (border-color . ,secondary-bg)))
                  (calendar:selected
                   ((background . ,accent-bg)
                    (color . ,accent-fg)))
                  (#(calendar.header calendar.button)
                   ((background . ,accent-bg)))
                  (calendar.button:hover
                   ((color . ,accent-bg)))
                  (iconview
                   ((background . ,bg)
                    (color . ,fg)))
                  (iconview:selected
                   ((background . ,accent-bg)
                    (color . ,accent-fg)))
                  ((scale contents trough)
                   ((background . ,secondary-bg)
                    (border-color . ,secondary-bg)
                    (outline . none)))
                  ((#{scale:not(.marks-after):not(.marks-before):not(:disabled)}# contents trough slider)
                   ((background . ,secondary-bg)
                    (border-color . ,bg)
                    (box-shadow . none)))
                  ((scale contents trough highlight)
                   ((background . ,accent-bg)
                    (border-color . ,accent-bg)))
                  ((scale:disabled contents trough slider)
                   ((background . ,secondary-bg)
                    (border-color . ,bg)))
                  (#((scale.marks-after contents trough slider)
                     (scale.marks-before trough slider))
                   ((background . ,secondary-fg)
                    (border-radius . 50%)
                    (border . (1px solid ,bg))
                    (box-shadow . none)
                    (min-height . 20px)
                    (min-width . 20px)
                    (margin . -7px)))
                  (progressbar
                   ((color . ,fg)))
                  ((progressbar trough)
                   ((background . ,secondary-bg)
                    (border-color . ,secondary-bg)))
                  ((progressbar trough progress)
                   ((background . ,accent-bg)
                    (border-color . ,accent-bg)))
                  ((levelbar trough)
                   ((background . ,secondary-bg)
                    (border-color . ,secondary-fg)))
                  ((levelbar trough block)
                   ((border-color . ,secondary-fg)
                    (background . ,secondary-fg)))
                  ((levelbar trough block.filled)
                   ((background . ,accent-bg)
                    (border-color . ,accent-bg)))
                  (entry
                   ((background . ,secondary-bg)
                    (color . ,fg)
                    (border . none)))
                  (entry:focus
                   ((box-shadow . (0 0 0 1px ,accent-bg inset))))
                  ((entry progress)
                   ((border-color . ,accent-bg)))
                  ((entry image)
                   ((color . ,fg)))
                  ((colorswatch overlay)
                   ((border . none)))
                  (selection
                   ((background . ,selection)
                    (color . ,fg)))
                  (dialog
                   ((background . ,bg)
                    (color . ,fg)))
                  (popover
                   ((background . ,bg)
                    (color . ,fg)
                    (border . none)
                    (box-shadow . none)))
                  (.dialog-box
                   ((background . ,bg)))
                  ((.dialog-vbox button)
                   ((border-radius . 0))))))))))
