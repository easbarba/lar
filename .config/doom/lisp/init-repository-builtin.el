;;; -*- lexical-binding: t;

;; =========================

(use-package! dired-ranger
  :after dired)

(use-package! dired-git-info
  :after dired
  :config (define-key dired-mode-map ")" 'dired-git-info-mode))

(use-package! dired-collapse
  :after dired
  :hook (dired-collapse-mode . dired-mode))

(use-package! dired-subtree
  :after dired
  :hook (dired-subtree . dired-mode))

(use-package! flx-ido
  :disabled
  :defer 1
  :after ido
  :config
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t))

(use-package ido-completing-read+
  :disabled
  :defer 1
  :after ido
  :config (ido-ubiquitous-mode 1))

(use-package ido-hacks
  :disabled
  :defer 1
  :config (ido-hacks-mode))

(provide 'init-repository-builtin)
