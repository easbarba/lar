;;; -*- lexical-binding: t;

;; ================================
;; * ADDITIONAL LANGUAGES TOOLINGS
;; ================================

;; ====================
;; ** LANGUAGES FORMATERS

;;(defvar formatters '((ruby-mode . 'rubocopfmt))
;;"List of formmater per major-mode")

;;(defun choose-formatter
;;(let ((formatter (get major-mode (buffer-expose-mode))))))

;; ====================
;; ** LANGUAGES MISC

(use-package robe
  :disabled
  :hook ruby-mode
  :config
  (robe-start t)
  (with-eval-after-load 'robe
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-robe))))

;; (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
;;   (rvm-activate-corresponding-ruby))

(use-package inf-ruby
  :defer 1)

(use-package rspec-mode
  :defer 1)

(use-package ruby-compilation
  :defer 1
  :config
  (with-eval-after-load 'ruby-mode
    (define-key ruby-mode-map [S-f7] 'ruby-compilation-this-buffer)
    (define-key ruby-mode-map [f7] 'ruby-compilation-this-test))

  (with-eval-after-load 'ruby-compilation
    (defalias 'rake 'ruby-compilation-rake)))

(use-package geiser
  :defer 1
  :commands (geiser-mode)
  :custom
  (geiser-active-implementations '(guile))
  (geiser-repl-history-filename (concat user-emacs-directory
					"geiser-history")))

(use-package nodejs-repl
  :defer t
  :config
  (add-hook 'js-mode-hook
	    (lambda ()
	      (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
	      (define-key js-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
	      (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
	      (define-key js-mode-map (kbd "C-c C-c") 'nodejs-repl-send-buffer)
	      (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
	      (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl))))

(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package yasnippet
  :defer 1
  :config
  (yas-global-mode 1)
  (yas-reload-all)
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (global-set-key (kbd "C-c Y") 'yas-insert-snippet)
  (define-key yas-minor-mode-map (kbd "C-c y") yas-maybe-expand)

  (setq yas-wrap-around-region t
	yas-triggers-in-field t
	yas-prompt-functions '(yas-ido-prompt yas-completing-prompt)
	yas-verbosity 1
	yas-fallback-behavior 'return-nil)
  (use-package yasnippet-snippets
    :after yasnippet
    :config (yasnippet-snippets-initialize)))

(use-package flymake-shellcheck
  :defer 1
  :commands flymake-shellcheck-load
  :config (add-hook 'sh-mode-hook 'flymake-shellcheck-load))


;; ========================================
;; * LSP CLIENTS/SERVERS

(use-package eglot
  :defer t
  :commands (eglot eglot-ensure)
  :custom (eglot-autoshutdown t)
  :config
  (define-key eglot-mode-map (kbd "C-c c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c c f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c c h") 'eglot-help-at-point)

  (add-to-list 'eglot-server-programs `((css-mode scss-mode) . ("css-languageserver" "--stdio")))
  (add-to-list 'eglot-server-programs `((html-mode . ("html-languageserver" "--stdio")))))


(use-package lsp-mode
  :defer 1
  :commands lsp
  :hook
  (elixir-mode . lsp)
  (ruby-mode . lsp)
  (css-mode . lsp)
  (js-mode . lsp)
  (html-mode . lsp)
  (shell-mode . lsp)
  (json-mode . lsp)
  (yaml-mode . lsp)
  :custom
  (lsp-enable-indentation nil)
  (lsp-signature-auto-activate nil)
  (flycheck-checker-error-threshold 1400)
  (lsp-clients-elixir-server-executable (f-join user-emacs-directory
						 ".cache" "lsp" "elixir-ls"
						 "language_server.sh"))
  :bind
  ("C-c l w" . lsp-restart-workspace)
  ("C-c l r" . lsp-rename)
  ("C-c l a" . lsp-execute-code-action))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config (setq lsp-ui-sideline-enable nil))

(use-package lsp-dart
  :hook (dart-mode . lsp))

(use-package helm-lsp
  :disabled
  :defer 1
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :defer t
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :defer t
  ;; :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))


(provide 'init-repository-languages)
