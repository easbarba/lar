;; ============================================================
;; AUTOSTART

(let ((autostart (concatenate 'string *local-bin* "/" "s_wm_autostart")))
  (run-shell-command autostart))
