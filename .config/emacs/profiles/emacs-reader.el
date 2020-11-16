;;; -*- lexical-binding: t;

;;; Description: Emacs Readers Applications

;; * LOAD

(load (concat user-emacs-directory "early-init.el"))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-base)
(require 'init-settings)
(require 'init-dired)

(package-initialize)

(require 'init-repository-get)
(require 'init-repository-readers)

;; * BOOKS
(find-file "~/Books")
