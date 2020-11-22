;;; -*- lexical-binding: t;

;;; ------------------------------------------------------------
;;; * PACOTES EXTERNOS

;; * LIBRARIES

(use-package f)
(use-package s)
(use-package dash)

;; * REPOSITORIES PACKAGES
;; ** REPOSITORIES PACKAGES - DIRED EXTRA

;; ** REPOSITORIES PACKAGES - COMPLETION EXTRA
(use-package exec-path-from-shell
  :defer 1
  :config (exec-path-from-shell-initialize))

(use-package ivy
  :defer 1
  :config

  (use-package counsel
    :defer 1)

  (use-package swiper
    :defer 1)

  (ivy-mode 1)

  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  ;; (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

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

;; ** REPOSITORIES PACKAGES - TOOLS

(use-package webpaste
  :defer t)

(use-package imgbb
  :defer t)

(use-package treemacs
  :defer t
  :bind (("C-c z" . treemacs)))

(use-package outshine
  :defer t
  :hook (prog-mode . outshine-mode)
  :bind ("C-c b c". outshine-cycle))

(use-package bug-hunter
  :defer t)

(use-package indent-guide-char
  :defer t
  :config (indent-guide-global-mode))

(use-package highlight-indent-guides
  :defer t
  :custom (highlight-indent-guides-method 'character))

(use-package eyebrowse
  :defer 1
  :bind (("<C-S-iso-lefttab>" . eyebrowse-last-window-config)
	 ("C-c C-w n" . (lambda () (interactive) (eyebrowse-next-window-config 1)))
	 ("C-c C-w p" . (lambda () (interactive) (eyebrowse-prev-window-config 1))))
  :custom
  (eyebrowse-new-workspace t)
  (eyebrowse-mode-line-style 'current)
  :config (eyebrowse-mode t))

(use-package helpful
  :defer t
  :bind
  (("C-h f" . 'helpful-callable)
   ("C-h F" . 'helpful-function)
   ("C-h v" . 'helpful-variable)
   ("C-h k" . 'helpful-key)
   ("C-c C-d" . 'helpful-at-point)
   ("C-c c" . 'helpful-command)))

(use-package vterm
  :defer t
  :bind ("C-c T" . 'vterm))

(use-package deadgrep
  :defer t
  :bind ("C-c b g" . 'deadgrep))

(use-package elfeed
  :defer t
  :custom
  (elfeed-db-directory "~/.config/elfeed")

  (elfeed-feeds
   '("https://planet.gnu.org/rss20.xml"
     "https://www.fsf.org/static/fsforg/rss/news.xml"
     "https://www.fsf.org/static/fsforg/rss/blogs.xml"
     "https://www.strathweb.com/feed"
     "https://nullprogram.com/feed"
     "https://tonarinoyj.jp/rss/series/13932016480028984490"
     "https://www.anahuac.eu/feed"
     "https://emacsredux.com/feed.xml"
     "https://metaredux.com/feed.xml"
     "http://sachachua.com/blog/category/emacs/feed"))
  :config
  (setq url-queue-timeout 30)
  (setq-default elfeed-search-filter "@2-week-ago +unread")

  ;; Entries older than 2 weeks are marked as read
  (add-hook 'elfeed-new-entry-hook
	    (elfeed-make-tagger :before "3 weeks ago"
				:remove 'eunread))

  (defun e/elfeed-yt-channel-id ()
    "Paste Youtube's channel id to formatted RSS feed for Elfeed or GNUS."
    (interactive)
    (insert (concat "\""
		    "https://www.youtube.com/feeds/videos.xml?channel_id="
		    (nth 4
			 (split-string
			  (read-from-minibuffer
			   "Enter Youtube's channel id: ") "/"))
		    "\" ;; "
		    (read-from-minibuffer "Enter Elfeed entry name: "))))

  (defun e/elfeed-play-with-video-player ()
    "Play entry link with video player - Ambrevar."
    (interactive)
    (let ((entry (if (eq major-mode 'elfeed-show-mode)
		     elfeed-show-entry
		   (elfeed-search-selected :single))))
      (e/play-video (elfeed-entry-link entry))))
  (define-key elfeed-search-mode-map "m" 'e/elfeed-play-with-video-player)

  (defun e/elfeed-open-with-eww ()
    "Open in eww with `eww-readable' - Ambrevar."
    (interactive)
    (let ((entry (if (eq major-mode 'elfeed-show-mode)
		     elfeed-show-entry (elfeed-search-selected :single))))
      (eww  (elfeed-entry-link entry))
      (add-hook 'eww-after-render-hook 'eww-readable nil t)))
  (define-key elfeed-search-mode-map "e" 'e/elfeed-open-with-eww)

  (defvar elfeed-visit-patterns
    '(("youtu\\.?be" . elfeed-play-with-player)
      ("phoronix" . elfeed-open-with-eww))
    "List of (regexps . function) to match against elfeed entry link to know
	    whether how to visit the link.")

  (defun e/elfeed-visit-maybe-externally ()
    "Open w/ external function if entry link matches `elfeed-visit-patterns',
	    show normally otherwise.  - Ambrevar."
    (interactive)
    (let ((entry (if (eq major-mode 'elfeed-show-mode)
		     elfeed-show-entry
		   (elfeed-search-selected :single)))
	  (patterns elfeed-visit-patterns))
      (while (and patterns (not (string-match (caar patterns)
					      (elfeed-entry-link entry))))
	(setq patterns (cdr patterns)))
      (cond
       (patterns
	(funcall (cdar patterns)))
       ((eq major-mode 'elfeed-search-mode)
	(call-interactively 'elfeed-search-show-entry))
       (t (elfeed-show-visit))))))

(use-package define-word
  :defer 3
  :config
  (global-set-key (kbd "C-c a d p") 'define-word-at-point)
  (global-set-key (kbd "C-c a d w") 'define-word))

(use-package emms
  :defer 1
  :bind (("C-c a e f" . emms-play-file)
	 ("C-c a e d" . emms-play-directory)
	 ("C-c a e s" . emms-smart-browse)
	 ("C-c a e r" . emms-toggle-repeat-playlist)
	 ("C-c a e R" . emms-toggle-repeat-track)
	 ("C-c a e e" . emms-pause)
	 ("C-c a e n" . emms-next)
	 ("C-c a e p" . emms-previous))
  :config
  (require 'emms-setup)

  (emms-all)
  (emms-default-players)
  (emms-history-load)
  (setq emms-player-list '(emms-player-mpv)
	emms-source-file-directory-tree-function
	'emms-source-file-directory-tree-find
	emms-source-file-default-directory *musica*)
  ;; mpv - ignore global config file
  (add-to-list 'emms-player-mpv-parameters "--no-config")
  (emms-mode-line nil)
  (emms-playing-time nil)

  (defun e/emms-sleep ()
    "EMMS timer, listen good music and it will pause in a few minutes late."
    (interactive)
    (let* ((minutes
	    (string-to-number
	     (read-from-minibuffer "Enter minutes to pause emms: ")))
	   (seconds (* minutes 60)))

      (message "Emms will stop in %s minutes" minutes)
      (sit-for seconds)
      (emms-pause)
      (e/sistema-suspender)))

  ;; Dired
  (define-key dired-mode-map "E" 'emms-play-dired))

(use-package google-translate
  :defer 1
  :config (require 'google-translate-smooth-ui)
  :custom
  (google-translate-translation-directions-alist
   '(("en" . "de") ("pt" . "de") ("pt" . "en"))))

;; ** REPOSITORIES PACKAGES - LANGUAGES

;; *** REPOSITORIES PACKAGES - LANGUAGES FORMATERS

;;(defvar formatters '((ruby-mode . 'rubocopfmt))
;;"List of formmater per major-mode")

;;(defun choose-formatter
;;(let ((formatter (get major-mode (buffer-expose-mode))))))

;; *** REPOSITORIES PACKAGES - LANGUAGES MISC

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
  :defer t
  :config (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package lsp-mode
  :defer 1
  :hook
  (go-mode . lsp)
  (ruby-mode . lsp)
  (js-mode . lsp)
  (html-mode . lsp)
  (css-mode . lsp)
  :commands lsp
  :init
  :custom
  (lsp-enable-indentation nil)
  (lsp-signature-auto-activate nil)
  (flycheck-checker-error-threshold 1400)
  :bind
  ("C-c l w" . lsp-restart-workspace)
  ("C-c l r" . lsp-rename)
  ("C-c l a" . lsp-execute-code-action))

(use-package eglot
  :defer 1
  :commands (eglot eglot-ensure)
  :custom (eglot-autoshutdown t)
  :config
  (define-key eglot-mode-map (kbd "C-c c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c c f") 'eglot-format)
  (define-key eglot-mode-map (kbd "C-c c h") 'eglot-help-at-point)

  (add-to-list 'eglot-server-programs `((css-mode scss-mode) . ("css-languageserver" "--stdio")))
  (add-to-list 'eglot-server-programs `((html-mode . ("html-languageserver" "--stdio")))))

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

;; ** REPOSITORIES PACKAGES - TERM

(use-package esh-autosuggest
  :defer 1
  :config (add-hook 'eshell-mode-hook #'esh-autosuggest-mode))

(use-package esh-help
  :after eshell
  :config (setup-esh-help-eldoc))

(use-package eshell-prompt-extras
  :after esh-opt
  :config (autoload 'epe-theme-lambda "eshell-prompt-extras")
  :custom
  (eshell-highlight-prompt nil)
  (eshell-prompt-function 'epe-theme-lambda))

(defvar eshell-syntax-highlighting-path (f-join *site-lisp* "eshell-syntax-highlighting.el"))
(use-package eshell-syntax-highlighting
  :defer 1
  :load-path eshell-syntax-highlighting-path
  :after esh-mode
  :config (eshell-syntax-highlighting-enable))

(use-package bash-completion
  :defer 1
  :config (bash-completion-setup))

;; ** REPOSITORIES PACKAGES - TOOLS

(use-package isolate
  :defer 1
  :config
  (defun activate-mark-hook@set-transient-map ()
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map "s" #'isolate-quick-add)
       (define-key map "S" #'isolate-long-add)
       (define-key map "d" #'isolate-quick-delete)
       (define-key map "D" #'isolate-long-delete)
       (define-key map "c" #'isolate-quick-change)
       (define-key map "C" #'isolate-long-change)
       map)
     #'region-active-p))

  (add-hook 'activate-mark-hook #'activate-mark-hook@set-transient-map))

(use-package reformatter
  :defer 1
  :config
  (reformatter-define lua-format :program "lua-format" :args '("-i")))

(use-package eshell-toggle
  :bind (("C-c x" . eshell-toggle)))

(use-package shackle
  :defer 1
  :init (shackle-mode 1)
  :config
  (setq shackle-rules '(("*Python*"          :regexp t :align 'right :size 15)
			("*eshell*"          :regexp t :align 'below :size 15)
			("*Backtrace*"       :regexp t :align 'below :size 20)
			("*Warnings*"        :regexp t :align 'below :size 15)
			("*Ibuffer*"         :regexp t :align 'below :size 25)
			("*Process List*"    :regexp t :align 'below :size 15)
			("*undo-tree*"       :regexp t :popup :align 'right)
			("*compilation*"     :regexp t :align 'below :size 0.4)
			("*shell*"           :regexp t :align 'below :size 15)
			("*slime-repl sbcl*" :regexp t :align 'below :size 15))))


(use-package bufler
  :defer t
  :bind ("C-c b i" . 'bufler))

(use-package highlight-indent-guides
  :defer t
  :config (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package objed
  :defer t
  :custom
  (objed-use-hl nil)
  ;; (objed-disabled-modes '(Info-mode erc-mode elfeed-search-mode))
  (objed-mode-line-format nil)
  :config (objed-mode))

(use-package org-bullets
  :defer 1
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-make-toc
  :defer t
  :hook (org-mode . org-make-toc-mode))

(use-package erc-hl-nicks
  :after erc
  :config
  (require 'erc-hl-nicks)
  (erc-update-modules))

(use-package erc-image
  :after erc)

(use-package ibuffer-vc
  :hook (ibuffer-vc . ibuffer-mode)
  :config (add-hook 'ibuffer-hook
		    #'(lambda ()
			(ibuffer-vc-set-filter-groups-by-vc-root)
			(unless (eq ibuffer-sorting-mode 'filename/process)
			  (ibuffer-do-sort-by-filename/process)))))

(use-package aggressive-indent
  :disabled
  :defer 1
  :init (global-aggressive-indent-mode 1)
  :custom (aggressive-indent-comments-too t)
  :config
  (define-key aggressive-indent-mode-map (kbd "C-c C-q") nil)

  (add-to-list 'aggressive-indent-excluded-modes 'haskell-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'pug-mode))

(use-package ace-window
  :defer t
  :bind ("C-x o" . 'ace-window))

(use-package rainbow-delimiters
  :defer 1
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package expand-region
  :defer t
  :bind (("C-c +" . er/contract-region)
	 ("C-c =" . er/expand-region))
  :config
  (defadvice kill-region (before slick-cut activate compile)
    "When called interactively with no active region,
      kill a single line instead."
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (list (line-beginning-position)
	     (line-beginning-position 2))))))

(use-package undo-tree
  :defer 1
  :bind
  (("C-c u" . 'undo-tree-visualize)
   ("C-c U" . 'undo-tree-visualize-redo))
  :config (global-undo-tree-mode))

(use-package multiple-cursors
  :defer 1
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package diff-hl
  :defer t
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

  (setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)

  ;; (global-diff-hl-mode t)
  (diff-hl-margin-mode t)
  (diff-hl-flydiff-mode t))

(use-package which-key
  :defer 1
  :config (which-key-mode))

(use-package webpaste
  :defer t
  :custom (webpaste-provider-priority '("dpaste.de")))

(use-package anzu
  :defer t
  :custom (anzu-mode-lighter ""))

(use-package crux
  :defer t)

(use-package paradox
  :defer 1
  :custom
  (paradox-execute-asynchronously t)
  (paradox-hide-wiki-packages t)
  :config
  (paradox-enable)
  (remove-hook 'paradox-after-execute-functions #'paradox--report-buffer-print))

(use-package smartparens
  :disabled
  :defer 1
  :config
  (require 'smartparens-config)
  (progn (show-smartparens-global-mode t))
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'java-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'shell-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode))

(use-package macrostep
  :defer t
  :bind (("C-c a m" . macrostep-expand)))

(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
  (setq-default company-dabbrev-other-buffers 'all
		company-tooltip-align-annotations t)
  (setq company-idle-delay 0
	company-begin-commands '(self-insert-command)
	company-minimum-prefix-length 2
	company-tooltip-limit 10
	company-show-numbers nil))

(use-package company-statistics
  :defer 1
  :config (add-hook 'after-init-hook 'company-statistics-mode))

(use-package company-quickhelp
  :defer 1
  :after company
  :config (company-quickhelp-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package olivetti
  :defer 1
  :custom (olivetti-body-width 122)
  :config (add-hook 'olivetti-mode 'eww-mode))

(use-package zoom
  :defer 1
  :config
  (zoom-mode)
  (setq zoom-ignored-major-modes '(dired-mode ediff-mode)))

(use-package quelpa
  :defer 1
  :config
  (quelpa
   '(quelpa-use-package
     :fetcher git
     :url "https://github.com/quelpa/quelpa-use-package.git"))
  (require 'quelpa-use-package))

(use-package beacon
  :defer 1
  :config
  (beacon-mode 1)
  (setq-default beacon-lighter "")
  (setq-default beacon-size 20))

(use-package guix
  :defer t)

(use-package doom-modeline
  :defer 1
  :custom
  (doom-modeline-env-version t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-vcs-max-length 1)
  (doom-modeline-segment--buffer-position t)
  (doom-modeline-segment--pdf-pages t)
  ;; (doom-modeline-segment--vcs nil)
  (doom-modeline-workspace-name t)
  (doom-modeline-lsp nil)
  (doom-modeline-irc nil)
  :config
  (setq-default doom-modeline-buffer-file-name-style 'file-name)
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t))
  (doom-modeline-init))

(use-package doom-themes
  :defer 1
  :config
  (disable-theme *theme*)
  (let ((cool-themes '(doom-city-lights
		       doom-challenger-deep
		       doom-dark+
		       doom-flatwhite
		       doom-gruvbox
		       doom-gruvbox-light
		       doom-henna
		       doom-horizon
		       doom-palenight
		       doom-miramare
		       doom-moonlight
		       doom-nord
		       doom-nord-light
		       doom-oceanic-next
		       doom-outrun-electric
		       doom-old-hope
		       doom-one
		       doom-one-light
		       doom-rouge
		       doom-spacegrey
		       doom-solarized-light
		       doom-zenburn)))
    (load-theme (nth (random (length cool-themes))
		     cool-themes)
		t)))

(use-package magit
  :defer 1
  :bind
  ("C-c m" . 'magit-status)
  ("C-c M" . 'magit-dispatch)
  :custom  (ediff-window-setup-function 'ediff-setup-windows-plain))

(provide 'init-repository-packages)
