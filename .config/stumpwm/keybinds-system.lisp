;; ===================================================
;; KEYBINDINGS

(dolist (current
	 '(("s-P"                    "s-play")
	   ("s-V"                    "s-video")
	   ("s-SPC"                  "mpc toggle")
	   ("M-s-d"                  "mpc next")
	   ("s-z"                    "mpv ~/Music/oosh.ogg")
	   ("Print"                  "s-shot --full")
	   ("s-Print"                "s-shot --partial")
	   ("s-a"                    "s-backlight up")
	   ("s-d"                    "s-backlight down")
	   ("s-w"                    "s-volume --up")
	   ("s-s"                    "s-volume --down")
	   ("s-e"                    "s-volume --toggle")
	   ("XF86AudioRaiseVolume"   "s-volume --up")
	   ("XF86AudioLowerVolume"   "s-volume --down")
	   ("XF86AudioMute"          "s-volume --toggle")))
  (define-key *top-map*
      (kbd (first current))
    (concatenate 'string "exec" " " (second current))))
