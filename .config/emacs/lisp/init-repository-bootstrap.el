;;; -*- lexical-binding: t;

;; ===================================
;; * BOOTSTRAP REPOSITORY PACKAGES
;; ===================================

(defun --e-install-packages (lst)
  "Install LST list."
  (package-refresh-contents)
  (dolist (package lst)
    (when (not (package-installed-p package))
      (package-install package))))

(defconst minimal-deps '(use-package magit async s f dash))

(unless (package-installed-p 'use-package)
  (--e-install-packages minimal-deps))

(when (require 'use-package nil :noerror)
  (require 'use-package)
  (setq-default use-package-verbose nil
		use-package-expand-minimally t
		use-package-enable-imenu-support t))

(defun e/install-packages ()
  "Install all packagesin package-selected-packages."
  (interactive)
    (--e-install-packages package-selected-packages))

(provide 'init-repository-bootstrap)
