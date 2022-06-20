;; -----------------
;; CONTRIB MODULES
;; -----------------

;; CONTRIB MODULOS LOCALIZACAO
(set-module-dir (pathname-as-directory (concat *home* "/Builds/stumpwm-contrib")))

(mapcar #'load-module '("end-session"
                        ;; "ttf-fonts"
                        "stumptray"
                        "notify"))

(define-key *top-map* (kbd "s-Q") "logout")
(stumptray::stumptray)
(notify:notify-server-toggle)

;; "cpu"
;; "battery-portable"
;; "mem"
;; "net"
;; "wifi"
;; "disk"
;; "^> / %c / %t / %f / %B / %M / %d      "
