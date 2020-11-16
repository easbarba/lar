;;; -*- lexical-binding: t;

;;; Description: GNU Emacs Configurations

;; (when (version< emacs-version "27.1")
;;   (load (concat user-emacs-directory "early-init.el"))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; SETTINGS

(require 'init-base)
(require 'init-settings)
(require 'init-modeline)

;; BUILTINS

(require 'init-builtin)
(require 'init-dired)
(require 'init-languages)

;; MY
(require 'init-my)

;; * LOAD EXTERNAL

(require 'init-repository-get)
(require 'init-repository-packages)
(require 'init-builtin-extras)

(provide 'init)
 ;;; init.el ends here
