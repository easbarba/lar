;; ===================================================
;; KEYBINDINGS

(dolist (current
         '(("s-z"                    "s-mx")
           ("s-P"                    "s-play")
           ("s-V"                    "s-video")
           ("s-SPC"                  "mpc toggle")
           ("s-D"                    "mpc next")
           ("M-s-x"                  "rofi -combi-modi window,drun,ssh -show combi -show-icons")
           ("s-W"                    "mpc volume +3")
           ("s-S"                    "mpc volume -3")
           ("Print"                  "s-shot --full")
           ("s-Print"                "s-shot --partial")
           ("s-a"                    "s-backlight up")
           ("s-d"                    "s-backlight down")
           ("s-w"                    "s-volume --up")
           ("s-s"                    "s-volume --down")
           ("s-l"                    "slock")
           ("s-e"                    "s-volume --toggle")
           ("XF86AudioRaiseVolume"   "s-volume --up")
           ("XF86AudioLowerVolume"   "s-volume --down")
           ("XF86AudioMute"          "s-volume --toggle")))
  (define-key *top-map*
      (kbd (first current))
    (concatenate 'string "exec" " " (second current))))
