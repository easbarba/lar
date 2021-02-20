;;; -*- lexical-binding: t;

;;; Description: GNU Emacs Configurations

(add-to-list 'load-path
	     (expand-file-name "lisp" user-emacs-directory))

;; SETTINGS

(require 'init-variables)
(require 'init-base)
(require 'init-settings)

;; BUILTINS
(require 'init-modeline)
(require 'init-dired)
(require 'init-builtin)
(require 'init-languages)

;; CUSTOM
(require 'init-bindings)
(require 'init-functions)

;; * LOAD EXTERNAL

(require 'init-repository-bootstrap)
(require 'init-repository-builtin)
(require 'init-repository-languages)
(require 'init-repository-packages)

(provide 'init)
 ;;; init.el ends here
