;;; -*- lexical-binding: t;

(defun --e-install-packages (lst)
  "Install LST list."
  ;; (interactive)

  (package-refresh-contents)
  (dolist (package lst)
    (when (not (package-installed-p package))
      (package-install package))))

(let ((deps '(use-package s f dash)))
 (unless (package-installed-p 'use-package) ;; TODO check all deps
  (package-refresh-contents)
  (e/install-external-packages deps)))

(when (require 'use-package nil :noerror)
  (require 'use-package)
  (setq-default use-package-verbose nil
		use-package-expand-minimally t
		use-package-enable-imenu-support t))

(defun e/install-packages ()
  "Install all packagesin package-selected-packages."
  (interactive)
    (--e-install-packages package-selected-packages))

(provide 'init-repository-get)
