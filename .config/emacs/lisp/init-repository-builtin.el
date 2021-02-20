;;; -*- lexical-binding: t;

;; =========================

(use-package async
  :after dired
  :config
  (when (equal system-name "WINDOWS-LX")
    (setq dired-async-log-file (concat temporary-file-directory "dired-async.log")))
  (global-set-key [remap dired-do-copy] 'dired-async-do-copy)
  (global-set-key [remap dired-do-rename] 'dired-async-do-rename)
  (global-set-key [remap dired-do-shell-command]
		  'dired-do-async-shell-command)
  (global-set-key [remap dired-do-symlink] 'dired-async-do-symlink)
  (global-set-key [remap dired-do-hardlink] 'dired-async-do-hardlink))

(use-package dired-ranger
  :disabled
  :defer 1
  :after dired)

(use-package dired-git-info
  :disabled
  :after dired
  :config (define-key dired-mode-map ")" 'dired-git-info-mode))

(use-package dired-collapse
  :disabled
  :after dired
  :hook (dired-collapse-mode . dired-mode))

(use-package dired-subtree
  :disabled
  :after dired
  :hook (dired-subtree . dired-mode))

(use-package flx-ido
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
