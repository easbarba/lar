;; ==================================================
;; WORKSPACE NAMES

(defparameter *group-names* '("mx" "read" "www" "media")
  "List of group names to be created.")

(when (consp *group-names*)
  (grename (first *group-names*))
  (loop for name in (rest *group-names*)
	do (add-group (current-screen) name))
  (gnext))

;; ===================================================
;; SOFTWARE PER GROUPS

(defparameter *frame-preferences*
  '(("mx"
     (0 t t :class "Emacs")
     (1 t t :class "Code"))
    ("read"
     (0 t t :class "okular")
     (1 t t :class "evince")
     (2 t t :class "libreoffice-writer"))
    ;; ("term"
    ;;  (0 t t :class "Alacritty")
    ;;  (1 t t :class "Konsole")
    ;;  (2 t t :class "Gnome-terminal"))
    ("www"
     (0 t t :class "Chromium")
     (1 t t :class "Firefox")
     (2 t t :class "Google-chrome")
     (3 t t :class "nyxt"))
    ("media"
     (0 t t :title "mpv")
     (1 t t :title "vlc")
     (2 t t :title "libreoffice")
     (4 t t :title "Thunderbird")
     (5 t t :title "wine.exe")
     (6 t t :title "wineboot.exe")
     (7 t t :title "winecfg.exe")
     (8 t t :title "kdetorrent")))
  "List of preferences to pass to define-frame-preference.")

(when (consp *frame-preferences*)
  (loop for (name . prefs) in *frame-preferences*
	do (eval `(define-frame-preference ,name ,@prefs))))
