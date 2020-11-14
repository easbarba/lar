;;; -*- lexical-binding: t;

;; =================================
;; * DEFALIAS / ADVICES-ADD / ADD-HOOK
;; =================================

;; DEFALIAS
(defalias 'yes-or-no-p 'y-or-n-p)

;; ** HOOKS
(add-hook 'after-init-hook 'global-auto-revert-mode)
(add-hook 'after-init-hook 'transient-mark-mode)
(add-hook 'focus-out-hook 'garbage-collect)
(add-hook 'makefile-mode-hook 'indent-tabs-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)
;; (add-hook 'after-save-hook 'backup-each-save)

;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)(set-charset-priority 'unicode))
(setq-default buffer-file-coding-system 'utf-8)

;; ** DISABLE ANNOYING
(let ((disable-those '(narrow-to-region
                       narrow-to-page
                       narrow-to-defun
                       upcase-region
                       downcase-region)))
  (dolist (x disable-those)
    (put 'disable-those 'disable nil)))

;; ** ENABLE GLOBAL FEATURES
(global-auto-revert-mode t)
(size-indication-mode t)
(blink-cursor-mode 0)
(delete-selection-mode t)
(transient-mark-mode t)
(global-font-lock-mode t)
(electric-pair-mode)
(prefer-coding-system 'utf-8)
(which-function-mode)
(pixel-scroll-mode)

(setq network-security-level 'high
      gnutls-verify-error t
      nsm-save-host-names t
      gnutls-min-prime-bits 4096
      global-auto-revert-non-file-buffers t
      global-so-long-mode t
      auto-revert-verbose nil
      max-specpdl-size 1500
      package-quickstart t
      max-lisp-eval-depth 10000
      garbage-collection-messages nil
      message-log-max t
      max-mini-window-height 1.8
      load-prefer-newer t
      large-file-warning-threshold 200000000
      require-final-newline t
      sentence-end-double-space nil
      frame-resize-pixelwise t
      global-subword-mode t
      confirm-kill-emacs 'y-or-n-p
      history-delete-duplicates t
      history-length 1000
      global-mark-ring-max 5000
      mark-ring-max 5000
      show-paren-delay 0
      blink-matching-paren 'jump-offscreen
      visible-bell t
      version-control t
      delete-old-versions t
      create-lockfiles nil
      split-width-threshold 0
      split-height-threshold nil
      enable-recursive-minibuffers t
      default-directory "~/"
      backup-directory-alist `(("." . (expand-file-name "etc/backups" user-emacs-directory)))
      auto-save-file-name-transforms `((".*" ,(expand-file-name "etc/auto-save-list" user-emacs-directory) t))
      find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")
      text-quoting-style 'grave
      ring-bell-function 'ignore
      pixel-dead-time 0
      pixel-resolution-fine-flag t
      mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed nil
      mouse-autoselect-window t
      focus-follows-mouse t
      scroll-margin 0
      scroll-conservatively 100000
      scroll-step 1
      suggest-key-bindings t
      completions-format 'vertical
      make-pointer-invisible t
      display-time-default-load-average nil
      apropos-sort-by-scores t
      delete-by-moving-to-trash nil
      register-separator ?+)

;; ====================
;; ** GLOBAL KEYBINDINGS
;; ====================
(global-set-key (kbd "C-o") 'newline-and-indent)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "M-s F") 'forward-word)
(global-set-key (kbd "C-/") 'undo-only)
(global-set-key (kbd "C-c b b") 'browse-url-at-point)
(global-set-key (kbd "C-c b l") 'find-library)
(global-set-key (kbd "C-c b e") 'elisp-index-search)
(global-set-key (kbd "C-c b p") 'list-packages)
(global-set-key (kbd "C-c b O") 'outline-hide-body)
(global-set-key (kbd "C-c b o") 'outline-hide-entry)
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-s-k") (lambda () (interactive) (kill-buffer)))
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)
(global-set-key (kbd "C-c b w") 'woman) ;; Apps
(global-set-key (kbd "C-c b g") 'grep-find)
(global-set-key (kbd "C-c t") #'(lambda () (interactive) (ansi-term "bash")))
(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-c b v") 'vc-dir)

(provide 'init-settings)
