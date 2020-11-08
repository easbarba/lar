;;; -*- lexical-binding: t;

;;; Description:

(load (concat user-emacs-directory "early-init.el"))

(setq inhibit-startup-screen t
      initial-scratch-message ""
      initial-major-mode 'emacs-lisp-mode
      debug-ignored-errors nil
      ;; toggle-debug-on-error t
      vc-follow-symlinks t)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("melpa" . "http://melpa.org/packages/")))

(require 'package)

(package-initialize)

(defcustom *theme* 'misterioso
  "Emacs default theme."
  :type 'string
  :group 'my)

(when (display-graphic-p) (load-theme *theme* t))

;; ** FONTES
(cond ((member "JetBrains Mono" (font-family-list))
       (add-to-list 'default-frame-alist '(font . "JetBrains Mono-14")))
      ((member "DejaVu Sans Mono" (font-family-list))
       (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-14"))))

(add-hook 'dired-mode-hook
          (lambda ()
            (require 'dired)

            (define-key dired-mode-map "^"
              (lambda () (interactive) (find-alternate-file "..")))
            (define-key dired-mode-map "b" 'e/dired-up-directory)

            (require 'wdired)
            (setq wdired-allow-to-change-permissions t
                  wdired-allow-to-redirect-links t)

            (require 'dired-x)
            (put 'dired-find-alternate-file 'disabled nil)

            (dired-hide-details-mode)))
