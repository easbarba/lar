;; ============================================================
;; AUTOSTART


(let ((autostart (concatenate 'string *lar* "/.local/bin" "/s-autostart-apps")))
  (run-shell-command autostart))
