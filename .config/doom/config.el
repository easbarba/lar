;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "EAS Barbosa"
      user-mail-address "easbarbosa@tutanota.com")

(cond ((member "Hack" (font-family-list))
       (setq doom-font (font-spec :family "Hack" :size 22 :weight 'semi-light)
             doom-variable-pitch-font (font-spec :family "sans" :size 22)))
      ((member "DejaVu Sans Mono" (font-family-list))
       (setq doom-font (font-spec :family "DejaVu Sans Mono" :size 23 :weight 'semi-light)
             doom-variable-pitch-font (font-spec :family "sans" :size 23))))

(setq doom-theme 'doom-manegarm
      org-directory "~/Documents/"
      display-line-numbers-type t)

;; BUILT-IN

(load! "lisp/init-dired")
(load! "lisp/init-utils")
(load! "lisp/init-variables")
(load! "lisp/init-functions")
(load! "lisp/init-misc")
(load! "lisp/init-settings")

;; REPOSITORY

(load! "lisp/init-repository-languages")
(load! "lisp/init-repository-misc")
