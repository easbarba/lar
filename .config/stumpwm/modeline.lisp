
;; ----------------
;; DEFAULT MODELINE
;; ----------------

(setf *screen-mode-line-format* "^B%n^b:  %W ^> %d"
      *mode-line-timeout* 2
      *mode-line-foreground-color* "#c6797e"
      *mode-line-background-color* "#2a2426"
      *mode-line-position* :bottom)

(enable-mode-line (current-screen) (current-head) t)
