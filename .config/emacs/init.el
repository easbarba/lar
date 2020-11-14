;;; -*- lexical-binding: t;

;;; Description: GNU Emacs Configurations

;; (when (version< emacs-version "27.1")
;;   (load (concat user-emacs-directory "early-init.el"))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-base)
(require 'init-settings)
(require 'init-modeline)

(require 'init-builtin)
(require 'init-languages)

(require 'init-my)

(require 'init-repository-packages)
(require 'init-builtin-extras)

(e/install-external-packages package-selected-packages)

(provide 'init)
 ;;; init.el ends here
