;;; init.el --- -*- lexical-binding: t;

;;; Commentary:
;;; Description: GNU Emacs Configurations
;;; Code:

;; * PATH
(push (expand-file-name "lisp" user-emacs-directory)
      load-path)

;; * SETTINGS
(require 'init-base)
(require 'init-variables)
(require 'init-settings)

;; * CUSTOM
(require 'init-bindings)
(require 'init-functions)

;; * BUILTINS
(require 'init-repository-bootstrap)
;; (require 'init-modeline)
(require 'init-dired)
;; (require 'init-builtin)
(require 'init-languages)

;; * LOAD EXTERNAL
(require 'init-repository-builtin)
(require 'init-repository-languages)
(require 'init-repository-misc)
(require 'init-repository-readers)
(require 'init-repository-exwm)

(provide 'init)
;;; init.el ends here
