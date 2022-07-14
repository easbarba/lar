;; ===================================================
;; KEYBINDINGS

(dolist (current
         '(("s-z"                    "s-dev-mx")
           ("s-P"                    "s-media-play")
           ("s-V"                    "s-media-video")
           ("s-SPC"                  "mpc toggle")
           ("s-D"                    "mpc next")
           ("M-s-x"                  "rofi -combi-modi window,drun,ssh -show combi -show-icons")
           ("s-W"                    "mpc volume +3")
           ("s-S"                    "mpc volume -3")
           ("Print"                  "s-wm-shot --full")
           ("s-Print"                "s-wm-shot --partial")
           ("s-a"                    "s-wm-backlight up")
           ("s-d"                    "s-wm-backlight down")
           ("s-w"                    "s-wm-volume --up")
           ("s-s"                    "s-wm-volume --down")
           ("s-l"                    "s-wm-lock")
           ("s-v"                    "bash -c 'notify-send \"$(s-wm-sysinfo)\"'")
           ("s-e"                    "s-wm-volume --toggle")
           ("XF86AudioRaiseVolume"   "s-wm-volume --up")
           ("XF86AudioLowerVolume"   "s-wm-volume --down")
           ("XF86AudioMute"          "s-wm-volume --toggle")))
  (define-key *top-map*
      (kbd (first current))
    (concatenate 'string "exec" " " (second current))))
