;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(remove-hook 'after-init-hook #'debian-ispell-set-startup-menu)

(require 'iso-transl) ;; solves dbus keyboard conflict
(setq frame-resize-pixelwise t) ;; do not resize initial window size

(setq-default user-full-name "dynamite black"
              user-login-name "dynab")

(cond ((member "JetBrains Mono" (font-family-list))
       (setq doom-font (font-spec :family "JetBrains Mono" :size 20)
             doom-variable-pitch-font (font-spec :family "sans" :size 24)))
      ((member "Hack" (font-family-list))
       (setq doom-font (font-spec :family "Hack" :size 23)
             doom-variable-pitch-font (font-spec :family "sans" :size 23)))
      ((member "DejaVu Sans Mono" (font-family-list))
       (setq doom-font (font-spec :family "DejaVu Sans Mono")
             doom-variable-pitch-font (font-spec :family "sans" :size 23))))

(setq doom-theme 'doom-miramare
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

(load! "lisp/init-repository-misc")
;; (load! "lisp/init-repository-readers")
