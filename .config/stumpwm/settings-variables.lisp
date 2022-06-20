;; ----------------
;; STUMPWM VARIABLES
;; ----------------

(setf *colors*
      '("#2a2426"   ; 0 black
        "#dc322f"   ; 1 red
        "#859900"   ; 2 green
        "#b58900"   ; 3 yellow
        "#268bd2"   ; 4 blue
        "#d33682"   ; 5 magenta
        "#2aa198"   ; 6 cyan
        "#c6797e")) ; 7 white

(setf *default-package* :stumpwm
      *startup-message* "Never succumb to the temptation of bitterness. - Martin Luther King, Jr"
      *suppress-abort-messages* t
      *shell-program* "/bin/bash"
      *window-number-map* "1234567890"
      *window-format* " %c " ;; (%m%n%s)
      *window-border-style* :thin
      *mouse-focus-policy*  :ignore
      *message-window-gravity* :top-right
      *input-window-gravity* :top-right
      *window-name-source* :title
      *timeout-wait* 5
      *normal-border-width* 0
      *maxsize-border-width* 0
      *transient-border-width* 0)

;; Debug

(setf *debug-level* 10)
(redirect-all-output (data-dir-file "debug" "log"))
