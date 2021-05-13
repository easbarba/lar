;;; -*- lexical-binding: t;

;; ======================
;; ** GLOBAL KEYBINDINGS
;; ======================

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
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)
(global-set-key (kbd "C-c b w") 'woman)
(global-set-key (kbd "C-c b g") 'grep-find)
(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-c b v") 'vc-dir)


(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-s-k") (lambda () (interactive) (kill-buffer)))
(global-set-key (kbd "C-c t") #'(lambda () (interactive) (ansi-term "bash")))

(provide 'init-bindings)
