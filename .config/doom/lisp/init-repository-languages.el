;;; -*- lexical-binding: t;

;; ================================
;; ADDITIONAL LANGUAGES TOOLINGS
;; ================================

;; ====================
;; * LANGUAGES FORMATERS

;;(defvar formatters '((ruby-mode . 'rubocopfmt))
;;"List of formmater per major-mode")

;;(defun choose-formatter
;;(let ((formatter (get major-mode (buffer-expose-mode))))))

;; ====================
;; * LANGUAGES MISC

(use-package page-break-lines
  :defer 1
  :config (global-page-break-lines-mode))

(use-package reformatter
  :defer 1
  :config
  (reformatter-define ruby-format :program "rufo")
  (reformatter-define lua-format :program "lua-format" :args '("-i")))

;; ====================
;; ** RUBY

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

(use-package ruby-test-mode
  :defer 1)

(use-package ruby-hash-syntax
  :defer 1)

(use-package ruby-end
  :defer 1)

(use-package ruby-refactor
  :defer 1)

(use-package seeing-is-believing
  :defer 1)

(use-package ruby-compilation
  :defer 1)

(use-package enh-ruby-mode
  :disabled
  :defer 1
  :config
  (autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode)))

(use-package ruby-compilation
  :defer 1
  :config
  (with-eval-after-load 'ruby-mode
    (define-key ruby-mode-map [S-f7] 'ruby-compilation-this-buffer)
    (define-key ruby-mode-map [f7] 'ruby-compilation-this-test))

  (with-eval-after-load 'ruby-compilation
    (defalias 'rake 'ruby-compilation-rake)))

(use-package ruby-tools
  :defer 1)

;; ====================
;; ** SCHEME

(use-package geiser
  :defer 1
  :hook (scheme-mode . geiser)
  :custom
  (geiser-active-implementations '(guile))
  (geiser-guile-binary "guile")
  (geiser-default-implementation 'guile)
  (geiser-repl-history-filename (f-join user-emacs-directory "geiser-history")))

;; ====================
;; ** JAVASCRIPT

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

;; ====================
;; ** ..

(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; ========================================
;; * TOOLS

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
  :disabled
  :defer 1
  :commands (eglot eglot-ensure)
  :custom (eglot-autoshutdown t)
  :config
  (add-hook 'ruby-mode 'eglot-ensure)
  (define-key eglot-mode-map (kbd "C-c c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c c f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c c h") 'eldoc)
  (define-key eglot-mode-map (kbd "C-c c h") 'eglot-help-at-point)
  (define-key eglot-mode-map (kbd ".") 'xref-find-definitions))


(use-package lsp-mode
  :defer 1
  :commands lsp
  :hook
  (lua-mode . lsp)
  (elixir-mode . lsp)
  (ruby-mode . lsp)
  (js-mode . lsp)
  (css-mode . lsp)
  (html-mode . lsp)
  (shell-script-mode . lsp)
  (shell-mode . lsp)
  (json-mode . lsp)
  (yaml-mode . lsp)
  :custom
  (lsp-idle-delay 0.500)
  (lsp-completion-provider :capf)
  (lsp-modeline-diagnostics-scope :workspace)
  (lsp-enable-indentation t)
  ;; (lsp-solargraph-use-bundler t)
  ;; (lsp-log-io nil) ; if set to true can cause a performance hit
  ;; (lsp-print-performance t)
  ;; (lsp-enable-file-watchers nil)
  (lsp-signature-auto-activate t)
  (flycheck-checker-error-threshold 1400)
  :bind
  ("C-c l w" . lsp-restart-workspace)
  ("C-c l r" . lsp-rename)
  ("C-c l a" . lsp-execute-code-action))

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config (setq lsp-ui-sideline-enable nil))


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
