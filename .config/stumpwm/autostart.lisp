;; ============================================================
;; AUTOSTART

(let ((autostart (concatenate 'string *lar* "/.local/bin" "/s-autostart")))
  (run-shell-command autostart))
