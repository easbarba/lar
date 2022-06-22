;; -----------------
;; CONTRIB MODULES
;; -----------------

;; CONTRIB MODULES LOCATION
(set-module-dir (pathname-as-directory (concat *home* "/Builds/stumpwm-contrib")))

(mapcar #'load-module '("end-session"
                        "stumptray"))

(define-key *top-map* (kbd "s-Q") "logout")
(stumptray::stumptray)
;; (notify:notify-server-toggle)

;; "ttf-fonts"
;; "notify"
;; "cpu"
;; "battery-portable"
;; "mem"
;; "net"
;; "wifi"
;; "disk"
;; "^> / %c / %t / %f / %B / %M / %d      "
