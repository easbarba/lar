;;; -*- lexical-binding: t;

;; ================================
;; ADDITIONAL LANGUAGES TOOLINGS
;; ================================

(add-hook! ruby-mode lsp!)
(add-hook! python-mode lsp!)
(add-hook! js-mode lsp!)
(add-hook! css-mode lsp!)
;; (add-hook! html-mode lsp!)
(add-hook! shell-script-mode lsp!)
(add-hook! shell-mode lsp!)
(add-hook! json-mode lsp!)
(add-hook! yaml-mode lsp!)
;; (add-hook! vue-mode-hook lsp!)

(provide 'init-repository-languages)
