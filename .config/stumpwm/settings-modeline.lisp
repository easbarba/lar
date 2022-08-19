;; ----------------
;; DEFAULT MODELINE
;; ----------------

(setf *screen-mode-line-format* "^B%n^b:  %W ^> %d"
      *mode-line-timeout* 10 ;; secs
      *mode-line-foreground-color* "white"
      *mode-line-background-color* "black"
      *mode-line-position* :top
      *window-format* "%m%n%s%c")

(enable-mode-line (current-screen) (current-head) t)

;; s-sysinfo modeline
(setf *screen-mode-line-format*
      (list "^B%n^b:  %W ^> "
            " "
            "%d"
            "     "))
            ;; '(:eval (string-trim (string #\newline) (stumpwm:run-shell-command "s-sysinfo" t)))
