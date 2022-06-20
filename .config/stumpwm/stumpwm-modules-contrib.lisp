;; -----------------
;; CONTRIB MODULES
;; -----------------

;; CONTRIB MODULOS LOCALIZACAO
(set-module-dir (pathname-as-directory (concat *home* "/Builds/stumpwm-contrib")))

;; (load-module "battery-portable")
;; (load-module "cpu")
;; (load-module "mem")
;; (load-module "wifi")

(load-module "end-session")
(define-key *top-map* (kbd "s-Q") "logout")

(load-module "stumptray")
(stumptray::stumptray)

(load-module "notify")
(notify:notify-server-toggle)

;; "^> / %c / %t / %f / %B / %M / %d      "
