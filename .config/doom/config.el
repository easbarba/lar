;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq-default user-full-name "Barba"
              user-login-name "easbarba")

(cond ((member "Hack" (font-family-list))
       (setq doom-font (font-spec :family "Hack" :size 23 :weight 'semi-light)
             doom-variable-pitch-font (font-spec :family "sans" :size 23)))
      ((member "DejaVu Sans Mono" (font-family-list))
       (setq doom-font (font-spec :family "DejaVu Sans Mono" :size 23 :weight 'semi-light)
             doom-variable-pitch-font (font-spec :family "sans" :size 23))))

(setq doom-theme 'doom-nord-light
      org-directory "~/Documents/"
      display-line-numbers-type t)

;; BUILT-IN

(load! "lisp/init-dired")
(load! "lisp/init-utils")
(load! "lisp/init-variables")
(load! "lisp/init-functions")
(load! "lisp/init-misc")
(load! "lisp/init-builtin")
(load! "lisp/init-settings")

;; REPOSITORY

;; (load! "lisp/init-repository-languages")
(load! "lisp/init-repository-misc")
(load! "lisp/init-repository-readers")
