;; ============================================================
;; AUTOSTART

(let ((autostart (concatenate 'string *home* "/.local/bin" "/s-autostart")))
  (run-shell-command autostart))
