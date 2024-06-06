;; ===================================================
;; KEYBINDINGS

(dolist (current
         '(("s-z"                    "s-dev-mx")
           ("s-P"                    "s-media-play")
           ("s-V"                    "s-media-video")
           ("s-SPC"                  "mpc toggle")
           ("s-D"                    "mpc next")
           ("s-A"                    "mpc previous")
           ("s-W"                    "mpc volume +3")
           ("s-S"                    "mpc volume -3")
           ("M-s-x"                  "rofi -combi-modi window,drun,ssh -show combi -show-icons")
           ("Print"                  "s_wm_shot --full")
           ("s-Print"                "s_wm_shot --partial")
           ("s-a"                    "s_wm_backlight up")
           ("s-d"                    "s_wm_backlight down")
           ("s-w"                    "s_wm_volume --up")
           ("s-s"                    "s_wm_volume --down")
           ("s-l"                    "s_wm_lock")
           ("s-v"                    "bash -c 'notify-send \"$(s_wm_sysinfo)\"'")
           ("s-e"                    "s_wm_volume --toggle")
           ("XF86AudioRaiseVolume"   "s_wm_volume --up")
           ("XF86AudioLowerVolume"   "s_wm_volume --down")
           ("XF86AudioMute"          "s_wm_volume --toggle")))
  (define-key *top-map*
      (kbd (first current))
    (concatenate 'string "exec" " " (second current))))
