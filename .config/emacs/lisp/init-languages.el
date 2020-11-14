;;; -*- lexical-binding: t;

;; ====================
;; * LANGUAGE MODES

;; ** Ruby
(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (setq ruby-align-chained-calls t)
	     (setq-default ruby-use-encoding-map nil
			   ruby-insert-encoding-magic-comment nil)

	     ;; (add-auto-mode 'ruby-mode
	     ;; 		    "\\.rxml\\'"
	     ;; 		    "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'" "\\.ru\\'"
	     ;; 		    "\\.gemspec\\'" "Kirkfile\\'")
	     ;; (add-auto-mode 'conf-mode "Gemfile\\.lock\\'")
	     ))

;; ** C
(add-hook 'c-mode-hook
	  '(lambda ()
	     ;; (local-set-key (kbd "<tab>") 'indent-for-tab-command)
	     (setq c-default-style "bsd"
		   c-basic-offset 4
		   truncate-lines t)))

;; ** PYTHON
(add-hook 'python-mode-hook
	  '(lambda ()
	     (setq-default python-shell-interpreter "python3"
			   python-indent-offset 4)
	     (local-set-key (kbd "C-c i") 'python-black-buffer)))

;; ** COMMON LISP
(add-hook 'lisp-mode-hook
	  '(lambda ()
	     ;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
	     (require 'cl-lib)
	     (setq inferior-lisp-program "sbcl")))

(add-hook 'json-mode-hook
	  '(lambda () (local-set-key (kbd "C-c i") 'json-pretty-print-buffer-ordered)))


(provide 'init-languages)
