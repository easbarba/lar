;;; init-repository-misc --- Summary -*- lexical-binding: t;

;;; Commentary:

;; ================================================
;;; * REPOSITORY MISCELLANEOUS PACKAGES & LIBRARIES
;; ================================================

;; ========================================
;; * LIBRARIES

;; (use-package! f)
;; (use-package! s)
;; (use-package! dash)

;;; Code:

(use-package! duplicate-thing
  :defer 1
  :bind (("C-c a q" . duplicate-thing)))

(use-package! alert
  :commands alert
  :custom (alert-default-style 'notifications))

;; (after! magit
;;   (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package! outshine
  :defer t
  :hook (prog-mode . outshine-mode)
  :bind ("C-c b c" . outshine-cycle))

(use-package! guix
  :defer t
  :hook (scheme-mode . guix-devel-mode))

(after! elfeed
  (setq elfeed-db-directory "~/.config/elfeed")
  (setq elfeed-feeds
   '("https://guix.gnu.org/feeds/blog.atom"
     "https://elephly.net/feed.xml"
     "https://www.ruby-lang.org/en/feeds/news.rss"
     "https://rubyweekly.com/rss/"
     "https://www.honeybadger.io/blog/feed.xml"
     "https://michael.stapelberg.ch/feed.xml"
     "https://edelpero.svbtle.com/feed"
     "https://debxp.org/feed/"
     "https://drewdevault.com/blog/index.xml"
     "https://blog.appsignal.com/feed.xml"
     "https://tonarinoyj.jp/atom/series/13932016480028984490"
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCdKJlY5eAoSumIlcOcYxIGg" ;; Nunca vi 1 cientista
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCSTlOTcyUmzvhQi6F8lFi5w" ;; Atila Iamarino
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCrSM1UV_KLAdTzgxyxl3POw" ;; Ola Ciencia
     "http://sachachua.com/blog/category/emacs/feed"))
  (setq url-queue-timeout 30)
  (setq elfeed-search-filter "@2-week-ago +unread")

  ;; Entries older than 2 weeks are marked as read
  (add-hook 'elfeed-new-entry-hook
	    (elfeed-make-tagger :before "3 weeks ago"
				:remove 'unread))

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
  (define-key elfeed-search-mode-map "m" #'e/elfeed-play-with-video-player))

(use-package! define-word
  :defer 3
  :config
  (global-set-key (kbd "C-c a d p") 'define-word-at-point)
  (global-set-key (kbd "C-c a d w") 'define-word))

(use-package! bufler
  :defer t
  :bind ("C-c b i" . 'bufler))

(use-package! org-make-toc
  :defer t
  :hook (org-mode . org-make-toc-mode))

(use-package! zoom
  :defer 1
  :config
  (zoom-mode)
  (setq zoom-ignored-major-modes '(dired-mode ediff-mode)))

(use-package! company-org-block
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

;; (use-package! org-present
;;   :config
;;   (eval-after-load "org-present"
;;   '(progn
;;      (add-hook 'org-present-mode-hook
;;                (lambda ()
;;                  (org-present-big)
;;                  (org-display-inline-images)
;;                  (org-present-hide-cursor)
;;                  (org-present-read-only)
;;                  (display-line-numbers-mode -1)))
;;      (add-hook 'org-present-mode-quit-hook
;;                (lambda ()
;;                  (org-present-small)
;;                  (org-remove-inline-images)
;;                  (org-present-show-cursor)
;;                  (org-present-read-write)
;;                  (display-line-numbers-mode 1))))))


;; (defun dw/org-present-prepare-slide ()
;;   (org-overview)
;;   (org-show-entry)
;;   (org-show-children))

;; (defun dw/org-present-hook ()
;;   (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
;;                                      (header-line (:height 4.5) variable-pitch)
;;                                      (org-code (:height 1.55) org-code)
;;                                      (org-verbatim (:height 1.55) org-verbatim)
;;                                      (org-block (:height 1.25) org-block)
;;                                      (org-block-begin-line (:height 0.7) org-block)))
;;   (setq header-line-format " ")
;;   (org-display-inline-images)
;;   (dw/org-present-prepare-slide))

;; (defun dw/org-present-quit-hook ()
;;   (setq-local face-remapping-alist '((default variable-pitch default)))
;;   (setq header-line-format nil)
;;   (org-present-small)
;;   (org-remove-inline-images))

;; (defun dw/org-present-prev ()
;;   (interactive)
;;   (org-present-prev)
;;   (dw/org-present-prepare-slide))

;; (defun dw/org-present-next ()
;;   (interactive)
;;   (org-present-next)
;;   (dw/org-present-prepare-slide))

;; (use-package org-present
;;   :bind (:map org-present-mode-keymap
;;          ("C-c C-j" . dw/org-present-next)
;;          ("C-c C-k" . dw/org-present-prev))
;;   :hook ((org-present-mode . dw/org-present-hook)
;;          (org-present-mode-quit . dw/org-present-quit-hook)))



;; (use-package! matrix-client)

;; (use-package emms
;;   :defer 1
;;   :bind (("C-c a e f" . emms-play-file)
;; 	 ("C-c a e d" . emms-play-directory)
;; 	 ("C-c a e s" . emms-smart-browse)
;; 	 ("C-c a e r" . emms-toggle-repeat-playlist)
;; 	 ("C-c a e R" . emms-toggle-repeat-track)
;; 	 ("C-c a e e" . emms-pause)
;; 	 ("C-c a e n" . emms-next)
;; 	 ("C-c a e p" . emms-previous))
;;   :config
;;   (require 'emms-setup)

;;   (emms-all)
;;   (emms-default-players)
;;   (emms-history-load)
;;   (setq emms-player-list '(emms-player-mpv)
;; 	emms-source-file-directory-tree-function
;; 	'emms-source-file-directory-tree-find
;; 	emms-source-file-default-directory *music*)
;;   ;; mpv - ignore global config file
;;   (add-to-list 'emms-player-mpv-parameters "--no-config")
;;   (emms-mode-line nil)
;;   (emms-playing-time nil)

;;   (defun e/emms-sleep ()
;;     "EMMS timer, listen good music and it will pause in a few minutes late."
;;     (interactive)
;;     (let* ((minutes
;; 	    (string-to-number
;; 	     (read-from-minibuffer "Enter minutes to pause emms: ")))
;; 	   (seconds (* minutes 60)))

;;       (message "Emms will stop in %s minutes" minutes)
;;       (sit-for seconds)
;;       (emms-pause)
;;       (e/sistema-suspender)))

;;   ;; Dired
;;   (define-key dired-mode-map "E" 'emms-play-dired))

;; (use-package google-translate
;;   :defer 1
;;   :config (require 'google-translate-smooth-ui)
;;   :custom
;;   (google-translate-translation-directions-alist
;;    '(("en" . "de") ("pt" . "de") ("pt" . "en"))))

;; ;; ** REPOSITORIES PACKAGES - TERM

;; (use-package esh-autosuggest
;;   :defer 1
;;   :config (add-hook 'eshell-mode-hook #'esh-autosuggest-mode))

;; (use-package esh-help
;;   :after eshell
;;   :config (setup-esh-help-eldoc))

;; (use-package eshell-prompt-extras
;;   :after esh-opt
;;   :config (autoload 'epe-theme-lambda "eshell-prompt-extras")
;;   :custom
;;   (eshell-highlight-prompt nil)
;;   (eshell-prompt-function 'epe-theme-lambda))

;; (use-package eshell-syntax-highlighting
;;   :defer 1
;;   :after esh-mode
;;   :config (eshell-syntax-highlighting-global-mode +1))

;; (use-package bash-completion
;;   :defer 1
;;   :config (bash-completion-setup))

;; ;; ** REPOSITORIES PACKAGES - TOOLS

;; (use-package isolate
;;   :defer 1
;;   :config
;;   (defun activate-mark-hook@set-transient-map ()
;;     (set-transient-map
;;      (let ((map (make-sparse-keymap)))
;;        (define-key map "s" #'isolate-quick-add)
;;        (define-key map "S" #'isolate-long-add)
;;        (define-key map "d" #'isolate-quick-delete)
;;        (define-key map "D" #'isolate-long-delete)
;;        (define-key map "c" #'isolate-quick-change)
;;        (define-key map "C" #'isolate-long-change)
;;        map)
;;      #'region-active-p))

;;   (add-hook 'activate-mark-hook #'activate-mark-hook@set-transient-map))

;; (use-package eshell-toggle
;;   :bind (("C-c x" . eshell-toggle)))

;; (use-package shackle
;;   :defer 1
;;   :init (shackle-mode 1)
;;   :config
;;   (setq shackle-rules '(("*Python*"          :regexp t :align 'right :size 15)
;; 			("*eshell*"          :regexp t :align 'below :size 15)
;; 			("*Backtrace*"       :regexp t :align 'below :size 20)
;; 			("*Warnings*"        :regexp t :align 'below :size 15)
;; 			("*Ibuffer*"         :regexp t :align 'below :size 25)
;; 			("*Process List*"    :regexp t :align 'below :size 15)
;; 			("*undo-tree*"       :regexp t :popup :align 'right)
;; 			("*compilation*"     :regexp t :align 'below :size 0.4)
;; 			("*shell*"           :regexp t :align 'below :size 15)
;; 			("*slime-repl sbcl*" :regexp t :align 'below :size 15))))


;; (use-package highlight-indent-guides
;;   :defer t
;;   :config (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

;; (use-package objed
;;   :defer t
;;   :custom
;;   (objed-use-hl nil)
;;   ;; (objed-disabled-modes '(Info-mode erc-mode elfeed-search-mode))
;;   (objed-mode-line-format nil)
;;   :config (objed-mode))

;; (use-package org-bullets
;;   :defer 1
;;   :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


;; (use-package erc-hl-nicks
;;   :disabled
;;   :after erc
;;   :config
;;   (require 'erc-hl-nicks)
;;   (erc-update-modules))

;; (use-package erc-image
;;   :disabled
;;   :after erc)

;; (use-package ibuffer-vc
;;   :hook (ibuffer-vc . ibuffer-mode)
;;   :config (add-hook 'ibuffer-hook
;; 		    #'(lambda ()
;; 			(ibuffer-vc-set-filter-groups-by-vc-root)
;; 			(unless (eq ibuffer-sorting-mode 'filename/process)
;; 			  (ibuffer-do-sort-by-filename/process)))))

;; (use-package aggressive-indent
;;   :disabled
;;   :defer 1
;;   :init (global-aggressive-indent-mode 1)
;;   :custom (aggressive-indent-comments-too t)
;;   :config
;;   (define-key aggressive-indent-mode-map (kbd "C-c C-q") nil)

;;   (add-to-list 'aggressive-indent-excluded-modes 'haskell-mode)
;;   (add-to-list 'aggressive-indent-excluded-modes 'pug-mode))

;; (use-package ace-window
;;   :defer t
;;   :bind ("C-x o" . 'ace-window))

;; (use-package rainbow-delimiters
;;   :defer 1
;;   :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; (use-package expand-region
;;   :defer t
;;   :bind (("C-c +" . er/contract-region)
;; 	 ("C-c =" . er/expand-region))
;;   :config
;;   (defadvice kill-region (before slick-cut activate compile)
;;     "When called interactively with no active region,
;;       kill a single line instead."
;;     (interactive
;;      (if mark-active (list (region-beginning) (region-end))
;;        (list (line-beginning-position)
;; 	     (line-beginning-position 2))))))

;; (use-package undo-tree
;;   :defer 1
;;   :bind
;;   (("C-c u" . 'undo-tree-visualize)
;;    ("C-c U" . 'undo-tree-visualize-redo))
;;   :config (global-undo-tree-mode))

;; (use-package multiple-cursors
;;   :defer 1
;;   :config
;;   (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;;   (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;;   (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;;   (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

;; (use-package diff-hl
;;   :defer t
;;   :config
;;   (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;;   (setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)

;;   ;; (global-diff-hl-mode t)
;;   (diff-hl-margin-mode t)
;;   (diff-hl-flydiff-mode t))

;; (use-package which-key
;;   :defer 1
;;   :config (which-key-mode))

;; (use-package webpaste
;;   :defer t
;;   :custom (webpaste-provider-priority '("dpaste.de")))

;; (use-package anzu
;;   :defer t
;;   :custom (anzu-mode-lighter ""))

;; (use-package crux
;;   :defer t)

;; (use-package paradox
;;   :disabled
;;   :defer 1
;;   :custom
;;   (paradox-execute-asynchronously t)
;;   (paradox-hide-wiki-packages t)
;;   :config
;;   (paradox-enable)
;;   (remove-hook 'paradox-after-execute-functions #'paradox--report-buffer-print))

;; (use-package smartparens
;;   :disabled
;;   :defer 1
;;   :config
;;   (require 'smartparens-config)
;;   (progn (show-smartparens-global-mode t))
;;   (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
;;   (add-hook 'java-mode-hook 'turn-on-smartparens-strict-mode)
;;   (add-hook 'shell-mode-hook 'turn-on-smartparens-strict-mode)
;;   (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode))

;; (use-package macrostep
;;   :defer t
;;   :bind (("C-c a m" . macrostep-expand)))

;; (use-package company
;;   :defer 1
;;   :config
;;   (add-hook 'after-init-hook 'global-company-mode)
;;   (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
;;   (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
;;   (setq-default company-dabbrev-other-buffers 'all
;; 		company-tooltip-align-annotations t)
;;   (setq company-idle-delay 0
;; 	company-begin-commands '(self-insert-command)
;; 	company-minimum-prefix-length 2
;; 	company-tooltip-limit 10
;; 	company-show-numbers nil))

;; (use-package company-statistics
;;   :defer 1
;;   :config (add-hook 'after-init-hook 'company-statistics-mode))

;; (use-package company-quickhelp
;;   :defer 1
;;   :after company
;;   :config (company-quickhelp-mode))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

;; (use-package olivetti
;;   :defer 1
;;   :hook
;;   ((eww-mode . olivetti-mode)
;;    (nov-mode . olivetti-mode))
;;   :custom (olivetti-body-width 86))

;; (use-package quelpa
;;   :defer t
;;   :config
;;   (quelpa
;;    '(quelpa-use-package
;;      :fetcher git
;;      :url "https://github.com/quelpa/quelpa-use-package.git"))
;;   (require 'quelpa-use-package))


;; (use-package beacon
;;   :defer 1
;;   :config
;;   (beacon-mode 1)
;;   (setq-default beacon-lighter "")
;;   (setq-default beacon-size 20))

;; (use-package doom-modeline
;;   :defer 1
;;   :custom
;;   (doom-modeline-env-version t)
;;   (doom-modeline-buffer-encoding nil)
;;   (doom-modeline-env-enable-ruby t)
;;   (doom-modeline-env-ruby-executable "ruby")
;;   ;; (doom-modeline-vcs-max-length 1)
;;   ;; (doom-modeline-segment--vcs nil)
;;   (doom-modeline-segment--buffer-position t)
;;   (doom-modeline-segment--pdf-pages t)
;;   (doom-modeline-workspace-name t)
;;   (doom-modeline-lsp t)
;;   (doom-modeline-irc nil)
;;   :config
;;   (setq-default doom-modeline-buffer-file-name-style 'file-name)
;;   (unless (member "all-the-icons" (font-family-list))
;;     (all-the-icons-install-fonts t))
;;   (doom-modeline-init))

;; (use-package doom-themes
;;   :defer 1
;;   :config
;;   (defun e/cool-themes()
;;     (interactive)
;;     (let* ((day-themes '(doom-acario-light
;; 			 doom-flatwhite
;; 			 doom-one-light
;; 			 doom-homage-white
;; 			 doom-opera-light
;; 			 doom-gruvbox-light
;; 			 doom-solarized-light
;; 			 doom-nord-light))
;; 	   (night-themes '(doom-gruvbox
;; 			   doom-miramare
;; 			   doom-dracula
;; 			   doom-laserwave
;; 			   doom-old-hope
;; 			   doom-rouge))
;; 	   (current-hour (string-to-number
;; 			  (format-time-string "%H" (current-time))))
;; 	   (current-themes night-themes))
;;       (e/change-theme (nth (random (length current-themes))
;; 			   current-themes) t)))
;;   (e/cool-themes))

;; (use-package magit-todos
;;   :defer t)

;; (use-package bug-hunter
;;   :defer t)

;; (use-package indent-guide-char
;;   :defer t
;;   :config (indent-guide-global-mode))

;; (use-package highlight-indent-guides
;;   :defer t
;;   :custom (highlight-indent-guides-method 'character))

;; (use-package eyebrowse
;;   :defer 1
;;   :bind (("<C-S-iso-lefttab>" . eyebrowse-last-window-config)
;; 	 ("C-c C-w n" . (lambda () (interactive) (eyebrowse-next-window-config 1)))
;; 	 ("C-c C-w p" . (lambda () (interactive) (eyebrowse-prev-window-config 1))))
;;   :custom
;;   (eyebrowse-new-workspace t)
;;   (eyebrowse-mode-line-style 'current)
;;   :config (eyebrowse-mode t))

;; (use-package helpful
;;   :defer t
;;   :bind
;;   (("C-h f" . 'helpful-callable)
;;    ("C-h F" . 'helpful-function)
;;    ("C-h v" . 'helpful-variable)
;;    ("C-h k" . 'helpful-key)
;;    ("C-c C-d" . 'helpful-at-point)
;;    ("C-c c" . 'helpful-command)))

;; (use-package vterm
;;   :defer t
;;   :bind ("C-c T" . 'vterm))

;; (use-package telega
;;   :commands telega
;;   :config
;;   (setq telega-user-use-avatars nil
;; 	telega-use-tracking-for '(any pin unread)
;; 	telega-chat-use-markdown-formatting t
;; 	telega-emoji-use-images t
;; 	telega-completing-read-function #'ivy-completing-read
;; 	telega-msg-rainbow-title nil
;; 	telega-chat-fill-column 75))

;; (use-package deadgrep
;;   :defer t
;;   :bind ("C-c b g" . 'deadgrep))


;; ========================================
;; ** EXPLORE COMPLETION EXTRA

;; (use-package ivy
;;   :defer 1
;;   :config
;;   (use-package counsel :defer 1)
;;   (use-package swiper :defer 1)

;;   (ivy-mode 1)

;;   (setq ivy-use-virtual-buffers t
;; 	search-default-mode #'char-fold-to-regexp ;; enable this if you want `swiper' to use it
;; 	enable-recursive-minibuffers t)

;;   (global-set-key "\C-s" 'swiper)
;;   (global-set-key (kbd "C-c C-r") 'ivy-resume)
;;   (global-set-key (kbd "<f6>") 'ivy-resume)
;;   (global-set-key (kbd "M-x") 'counsel-M-x)
;;   (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;   (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;;   (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;;   (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
;;   (global-set-key (kbd "<f1> l") 'counsel-find-library)
;;   (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;;   (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;;   (global-set-key (kbd "C-c r") 'counsel-recentf)
;;   (global-set-key (kbd "C-c g") 'counsel-git)
;;   (global-set-key (kbd "C-c j") 'counsel-git-grep)
;;   ;; (global-set-key (kbd "C-c k") 'counsel-ag)n
;;   (global-set-key (kbd "C-x l") 'counsel-locate)
;;   (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;; (use-package ivy-yasnippet
;;   :defer 1)

;; (use-package ivy-rich
;;   :defer 1
;;   :config
;;   (ivy-rich-mode 1)
;;   (setcdr (assq t ivy-format-functions-alist)
;; 	  #'ivy-format-function-line))

;; (use-package ivy-posframe
;;   :defer 1
;;   :custom
;;   (ivy-posframe-width      115)
;;   (ivy-posframe-min-width  115)
;;   (ivy-posframe-height     10)
;;   (ivy-posframe-min-height 10)
;;   :config
;;   (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;;   (setq ivy-posframe-parameters '((parent-frame . nil)
;; 				  (left-fringe . 8)
;; 				  (right-fringe . 8)))
;;   (ivy-posframe-mode 1))

;; (use-package ivy-avy
;;   :defer 1)

;; ;; ;; ========================================
;; ;; ;; ** TOOLS

;; (use-package async
;;   :after dired
;;   :config
;;   (when (equal system-name "WINDOWS-LX")
;;     (setq dired-async-log-file (concat temporary-file-directory "dired-async.log")))
;;   (global-set-key [remap dired-do-copy] 'dired-async-do-copy)
;;   (global-set-key [remap dired-do-rename] 'dired-async-do-rename)
;;   (global-set-key [remap dired-do-shell-command]
;; 		  'dired-do-async-shell-command)
;;   (global-set-key [remap dired-do-symlink] 'dired-async-do-symlink)
;;   (global-set-key [remap dired-do-hardlink] 'dired-async-do-hardlink))

;; (use-package openwith
;;   :disabled
;;   :config
;;   (setq openwith-associations
;; 	(list
;; 	 (list (openwith-make-extension-regexp
;; 		'("mpg" "mpeg" "mp3" "mp4"
;; 		  "avi" "wmv" "wav" "mov" "flv"
;; 		  "ogm" "ogg" "mkv"))
;; 	       "mpv"
;; 	       '(file))
;; 	 (list (openwith-make-extension-regexp
;; 		'("xbm" "pbm" "pgm" "ppm" "pnm"
;; 		  "png" "gif" "bmp" "tif" "jpeg")) ;; Removed jpg because Telega was
;; 	       ;; causing feh to be opened...
;; 	       "feh"
;; 	       '(file))
;; 	 (list (openwith-make-extension-regexp
;; 		'("pdf"))
;; 	       "zathura"
;; 	       '(file))))
;;   (openwith-mode 1))

;; (use-package all-the-icons-ivy-rich
;;   :defer 1
;;   :config (all-the-icons-ivy-rich-mode 1))

;; (use-package deft
;;   :commands (deft)
;;   :config (setq deft-directory (f-join *home* "Documents")
;; 		deft-recursive t
;; 		deft-extensions '("md" "org")))

;; (use-package org-appear
;;   :disabled
;;   :hook (org-mode . org-appear-mode))

;; (use-package beginend
;;   :defer 1
;;   :config (beginend-global-mode))

;; (use-package exec-path-from-shell
;;   :defer 1
;;   :config (when (memq window-system '(mac ns x))
;; 	    (exec-path-from-shell-initialize)))

;; (use-package vimish-fold
;;   :defer 1)

;; (use-package origami
;;   :defer t
;;   :hook (yaml-mode . origami-mode))

;; (use-package webpaste
;;   :defer t)

;; (use-package imgbb
;;   :defer t)

;; (use-package treemacs
;;   :defer t
;;   :bind ("C-c z" . treemacs))


(provide 'init-repository-misc)
;;; init-repository-misc.el ends here
