;; ==================================================
;; KEYBINDINGS


;; PREFIX KEY
(set-prefix-key (kbd "s-z"))

;; MISC
(define-key *top-map* (kbd "s-Q") "quit")
(define-key *top-map* (kbd "s-:") "eval")
(define-key *top-map* (kbd "s-;") "colon")
(define-key *top-map* (kbd "M-s-r") "reload")
(define-key *top-map* (kbd "s-x") "exec")
(define-key *top-map* (kbd "M-s-b") "mode-line")

(define-key *top-map* (kbd "s-j") "next-in-frame")
(define-key *top-map* (kbd "s-p") "prev-in-frame")
(define-key *top-map* (kbd "s-f") "fullscreen")
(define-key *top-map* (kbd "s-c") "delete")
(define-key *top-map* (kbd "s-r") "remove")
(define-key *top-map* (kbd "s-i") "iresize")

;; GROUPS
(define-key *top-map* (kbd "s-n") "gnext")
(define-key *top-map* (kbd "s-p") "gprev")
(define-key *top-map* (kbd "s-TAB") "gother")
(define-key *top-map* (kbd "s-m") "gmove")

;; SELECT GROUP
(define-key *top-map* (kbd "s-L") "grouplist")
(define-key *top-map* (kbd "s-0") "gselect 0")
(define-key *top-map* (kbd "s-1") "gselect 1")
(define-key *top-map* (kbd "s-2") "gselect 2")
(define-key *top-map* (kbd "s-3") "gselect 3")
(define-key *top-map* (kbd "s-4") "gselect 4")
(define-key *top-map* (kbd "s-5") "gselect 5")

;; MOVE
(define-key *top-map* (kbd "s-)") "pull 0")
(define-key *top-map* (kbd "s-!") "pull 1")
(define-key *top-map* (kbd "s-@") "pull 2")
(define-key *top-map* (kbd "s-#") "pull 3")
(define-key *top-map* (kbd "s-$") "pull 4")
(define-key *top-map* (kbd "s-%") "pull 5")
