;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(remove-hook 'after-init-hook #'debian-ispell-set-startup-menu)

(require 'iso-transl) ;; solves dbus keyboard conflict
(setq frame-resize-pixelwise t) ;; do not resize initial window size

(cond ((member "JetBrains Mono" (font-family-list))
       (setq doom-font (font-spec :family "JetBrains Mono" :size 22)
             doom-variable-pitch-font (font-spec :family "sans" :size 24)))
      ((member "DejaVu Sans Mono" (font-family-list))
       (setq doom-font (font-spec :family "DejaVu Sans Mono")
             doom-variable-pitch-font (font-spec :family "sans" :size 23))))

(when (string-equal system-name "barba-x220")
  (setq straight-disable-native-compile t)
  (when (member "JetBrains Mono" (font-family-list))
    (setq doom-font (font-spec :family "JetBrains Mono" :size 18)
          doom-variable-pitch-font (font-spec :family "sans" :size 20))))

(setq doom-theme 'doom-miramare ;; 1337 for terminal
      display-line-numbers-type t)

(setq org-directory "~/org/")

;; BUILT-IN
(load! "lisp/init-utils")
(load! "lisp/init-variables")
(load! "lisp/init-dired")
(load! "lisp/init-functions")
(load! "lisp/init-misc")
(load! "lisp/init-builtin")

;; REPOSITORY
(load! "lisp/init-repository-misc")
(load! "lisp/init-repository-languages")
