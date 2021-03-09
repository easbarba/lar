;;; package --- base -*- lexical-binding: t;

;;; Commentary:
;;; Code:


;;; Description:

;; =========================
;; * INITIAL CONFIGURATIONS
;; =========================

;; =========================
;; ** GC

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
	  #'(lambda ()
	      (setq gc-cons-threshold 20000000 gc-cons-percentage 0.1)))

;; =========================
;; **  MINIMAL

(setq inhibit-startup-screen t
      initial-scratch-message ""
      initial-major-mode 'emacs-lisp-mode
      debug-ignored-errors nil
      toggle-debug-on-error t
      vc-follow-symlinks t)

;; =========================
;; ** BOOTSTRAP PACKAGES

(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("melpa" . "http://melpa.org/packages/")))

;; =========================
;; ** CUSTOM FILE

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t)

;; =========================
;; ** EMACS DIRECTORIES

(dolist (fld '("site-lisp" "etc" "erc"))
  (unless (file-exists-p (concat user-emacs-directory fld))
    (mkdir (concat user-emacs-directory fld))))

(provide 'init-base)
;;; init-base.el ends here
