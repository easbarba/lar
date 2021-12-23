;;; init-settings --- Summary: -*- lexical-binding: t;

;;; Code:
;;; Commentary:

;; =================================
;; * MISCELLANEOUS DEFAULT SETTINGS
;; =================================

;; =========================
;; ** THEMES

;; (when (display-graphic-p) (load-theme *theme* t))

;; ** HOOKS
;; (add-hook 'after-init-hook 'transient-mark-mode)
;; (add-hook 'focus-out-hook 'garbage-collect)
;; (add-hook 'makefile-mode-hook 'indent-tabs-mode)
;; (add-hook 'prog-mode-hook 'hs-minor-mode)
;; (add-hook 'after-save-hook 'backup-each-save)

;; UTF-8 as the default coding system
;; (when (fboundp 'set-charset-priority)(set-charset-priority 'unicode))
;; (setq-default buffer-file-coding-system 'utf-8)

;; ** DISABLE ANNOYING
;; (let ((disable-those '(narrow-to-region
;;                        narrow-to-page
;;                        narrow-to-defun
;;                        upcase-region
;;                        downcase-region)))
;;   (dolist (x disable-those)
;;     (put 'disable-those 'disable nil)))

;; ** ENABLE GLOBAL FEATURES
(global-auto-revert-mode t)
(electric-pair-mode)
(prefer-coding-system 'utf-8)
(which-function-mode)
(pixel-scroll-mode)

(setq network-security-level 'high
      read-process-output-max (* 1024 1024)
      gnutls-min-prime-bits 4096
      max-specpdl-size 1500
      load-prefer-newer t
      global-subword-mode t
      history-delete-duplicates t
      history-length 1000
      blink-matching-paren 'jump-offscreen
      default-directory "~/"
      text-quoting-style 'grave
      pixel-dead-time 0
      pixel-resolution-fine-flag t
      mouse-autoselect-window t
      focus-follows-mouse t
      scroll-conservatively 100000
      apropos-sort-by-scores t
      register-separator ?+)

;; DOOM SETTINGS
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

(provide 'init-settings)
;;; init-settings.el ends here
