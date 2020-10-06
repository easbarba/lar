;;; -*- lexical-binding: t;

(setq gc-cons-threshold 100000000)

;; UI minimo
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(scroll-bar-mode . 0) default-frame-alist)
