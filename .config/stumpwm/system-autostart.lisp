;; ============================================================
;; AUTOSTART

(let ((autostart (concatenate 'string *local-bin* "/" "s-wm-autostart")))
  (run-shell-command autostart))
