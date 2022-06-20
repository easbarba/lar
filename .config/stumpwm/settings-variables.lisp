;; ----------------
;; STUMPWM VARIABLES
;; ----------------

(setf *default-package* :stumpwm
      *startup-message* "Praise the Sun!"
      *suppress-abort-messages* t
      *shell-program* (getenv "SHELL")
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
