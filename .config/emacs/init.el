;;; -*- lexical-binding: t;

;;; Descricao: GNU Emacs configuracoes

;; * CONFIGURACOES INICIAS
;; ** GC
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
	  #'(lambda ()
	      (setq gc-cons-threshold 20000000 gc-cons-percentage 0.1)))

;; **  Minimas
(setq inhibit-startup-screen t
      initial-scratch-message ""
      initial-major-mode 'emacs-lisp-mode
      debug-ignored-errors nil
      ;; toggle-debug-on-error t
      vc-follow-symlinks t)

;; ** PACOTES

(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("melpa" . "http://melpa.org/packages/")))


;; Louvado seja o sol
(package-initialize)

;; ** custom arquivo
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t)

;; ** Pastas do Emacs
(dolist (fld '("site-lisp" "etc" "erc"))
  (unless (file-exists-p (concat user-emacs-directory fld))
    (mkdir (concat user-emacs-directory fld))))

(defconst *lisp-folder* (concat user-emacs-directory "lisp"))

(add-to-list 'load-path *lisp-folder*)

;; ** TEMAS
(defcustom *theme* 'adwaita
  "Emacs tema padrao."
  :type 'string
  :group 'my)

(when (display-graphic-p) (load-theme *theme* t))

;; ** FONTES
(cond ((member "JetBrains Mono" (font-family-list))
       (add-to-list 'default-frame-alist '(font . "JetBrains Mono-12")))
      ((member "DejaVu Sans Mono" (font-family-list))
       (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-13"))))

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

;; ====================
;; * MODELINE

(defun simple-mode-line-render (left right)
  "Modeline separator generator by LEFT and RIGHT wings."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds" available-width) left right)))

(setq-default mode-line-format
	      '((:eval
		 (simple-mode-line-render
		  (format-mode-line ;; left wing
		   (quote
		    ((:eval ;; up dir + filename
		      (propertize
		       (concat (file-name-nondirectory
				(directory-file-name default-directory))
			       "/"
			       (replace-regexp-in-string "*" "" (buffer-name)))
		       'face 'font-lock-keyword-face
		       'help-echo (buffer-file-name)))
		     )))
		  (format-mode-line  ;; right wing
		   (quote
		    ("  "
		     ((:eval
		       (cond
			(buffer-read-only
			 (propertize " RO "
				     'face 'font-lock-keyword-face
				     'help-echo "Buffer Read-Only"))
			((buffer-modified-p)
			 (propertize " MOD "
				     'face 'font-lock-warning-face
				     'help-echo "Buffer has been modified"))))
		      "  "
		      (:eval (propertize "Ln %2l Col %2c - %2o%%"
					 'face 'font-lock-builtin-face
					 'help-echo "Cursor position"))
		      "  "
		      mode-line-modes
		      "  "
		      mode-line-misc-info))))))))

;; List here minor modes to be hidden
(defvar hidden-minor-modes
  '(abbrev-mode
    auto-fill-function
    auto-highlight-symbol-mode
    auto-revert-mode
    aggressive-indent-mode
    erc-mode
    anzu-mode
    beacon-mode
    olivetti-mode
    color-identifiers-mode
    electric-indent-mode
    electric-pair-mode
    eldoc-box-hover-mode
    emms-volume-minor-mode
    eldoc-mode
    flyspell-mode
    global-whitespace-mode
    golden-ratio-mode
    midnight-mode
    visual-line-mode
    smartparens-mode
    page-break-lines-mode
    rainbow-mode
    zoom-mode
    rainbow-delimiters-mode
    projectile-mode
    which-key-mode
    whitespace-mode
    yas-minor-mode
    yas-global-mode
    undo-tree-mode
    org-src-mode
    overwrite-mode
    outline-mode
    outline-minor-mode
    outshine-mode
    Info-mode
    symbol-overlay
    ivy-mode
    autodoc-mode
    company-box-mode
    persp-mode
    perspective-mode
    hs-minor-mode
    company-mode
    smooth-scroll-mode))

(defun purge-minor-modes ()
  "Purge minor modes off mode-line."
  (interactive)
  (dolist (x hidden-minor-modes nil)
    (let ((h (cdr (assoc x minor-mode-alist))))
      (when h
	(setcar h "")))))

(add-hook 'after-change-major-mode-hook 'purge-minor-modes)
;; * PACOTES INTERNOS
;; ** IDO
  (require 'ido)
  (setq ido-everywhere t)
  (ido-mode 1)

  (setq ido-create-new-buffer 'always
        ido-enable-prefix nil
        ido-enable-regexp t
        ido-decorations
        (quote ("\n-> " "" "\n " "\n ..." "[" "]" " [No match]" " [Matched]" "
             [Not readable]" " [Too big]" " [Confirm]"))
        ido-file-extensions-order '(".lisp" ".py" ".org" ".el")
        ido-max-directory-size 100000
        ido-use-filename-at-point t
        ido-enable-dot-prefix t
        ido-use-url-at-point t
        ido-use-filename-at-point 'guess
        ido-use-virtual-buffers t)

  (define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil)

  (add-hook 'ido-setup-hook
            (lambda ()
              (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
              (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))

  (defun e/ido-bookmark-jump (bname)
    "Switch to bookmark BNAME interactively using `ido'."
    (interactive
     (list (ido-completing-read "Bookmark: " (bookmark-all-names) nil t)))
    (bookmark-jump bname))

  (defun e/recentf-ido-find-file ()
    "Find a recent file using Ido."
    (interactive)
    (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
      (when file
        (find-file file))))
  (global-set-key (kbd "C-c r") 'e/recentf-ido-find-file)

  (defun e/mx-ido ()
    "Open Mx in ido-fashioned way."
    (interactive)
    (call-interactively
     (intern
      (ido-completing-read
       "M-x "
       (all-completions "" obarray 'commandp)))))

;; ===== ORG-MODE
(add-hook 'org-mode-hook
	  (lambda ()
	    (require 'org)
	    (require 'ox-md)

	    (setq org-hide-leading-stars t
		  org-hide-emphasis-markers t
		  org-edit-src-content-indentation 0
		  org-src-tab-acts-natively t
		  org-confirm-babel-evaluate nil
		  org-support-shift-select 'always
		  org-src-fontify-natively t
		  org-fontify-whole-heading-line t
		  org-fontify-done-headline t
		  org-fontify-quote-and-verse-blocks t
		  org-log-done t
		  org-startup-with-inline-images nil
		  org-preview-latex-default-process 'dvisvgm
		  org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

	    (defun e/tangle-on-save-org-mode-file()
	      "Tangle org file on save."
	      (interactive)
	      (when (eq major-mode 'org-mode)
		(message "%s" major-mode)
		(org-babel-tangle)))
	    (add-hook 'after-save-hook 'e/tangle-on-save-org-mode-file)

	    (defun e/narrow-or-widen-dwim (p)
	      "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.

      Intelligently means: region, org-src-block, org-subtree, or defun,
      whichever applies first.
      Narrowing to org-src-block actually calls `org-edit-src-code'.
      With prefix P, don't widen, just narrow even if buffer is already narrowed."
	      (interactive "P")
	      (declare (interactive-only))
	      (cond ((and (buffer-narrowed-p) (not p)) (widen))
		    ((region-active-p)
		     (narrow-to-region (region-beginning) (region-end)))
		    ((derived-mode-p 'org-mode)
		     ;; `org-edit-src-code' is not a real narrowing command.
		     ;; Remove this first conditional if you don't want it.
		     (cond ((org-in-src-block-p)
			    (org-edit-src-code)
			    (delete-other-windows))
			   ((org-at-block-p)
			    (org-narrow-to-block))
			   (t (org-narrow-to-subtree))))
		    (t (narrow-to-defun))))
	    (global-set-key (kbd "C-c d") 'e/narrow-or-widen-dwim)

	    (eval-after-load 'org-src
	      '(define-key org-src-mode-map
		 "\C-x\C-s" #'org-edit-src-exit))))

;; ** DIRED
  (add-hook 'dired-mode-hook
            (lambda ()
              (require 'dired)

              (when (eq system-type 'berkeley-unix)
                (setq insert-directory-program "gls"))

              (setq global-auto-revert-non-file-buffers t
                    dired-listing-switches "-laGh1v --group-directories-first"
                    auto-revert-verbose nil
                    dired-omit-mode t
                    dired-dwim-target t
                    dired-recursive-deletes (quote top)
                    dired-recursive-deletes t
                    dired-compress-files-alist
                    '(("\\.zip\\'" . "zip %o -r --filesync %i")
                      ("\\.tar\\.gz\\'" . "tar -c %i | gzip -c9 > %o"))
                    dired-guess-shell-alist-user
                    (list
                     '("\\.odt\\'" "libreoffice")
                     '("\\.\\(?:mp4\\|webm\\|mkv\\|ogv\\)\\(?:\\.part\\)?\\'"
                       ,*player*)
                     '("\\.html?\\'" *browser*))
                    dired-create-destination-dirs t)

              (defun e/dired-up-directory ()
                "Ora - Up directory no spawn directories in Ibuffer."
                (interactive)
                (let ((this-directory default-directory)
                      (buffer (current-buffer)))
                  (dired-up-directory)
                  (unless (cl-find-if
                           (lambda (w)
                             (with-selected-window w
                               (and (eq major-mode 'dired-mode)
                                    (equal default-directory this-directory))))
                           (delete (selected-window) (window-list)))
                    (kill-buffer buffer))))

              (defun e/dired-open-file ()
                "In dired, open the file named on this line."
                (interactive)
                (let* ((file (dired-get-filename nil t)))
                  (message "Opening %s..." file)
                  (call-process "xdg-open" nil 0 nil file)
                  (message "Opening %s done" file)))

              (define-key dired-mode-map "z" 'dired-do-compress)
              (define-key dired-mode-map "^"
                (lambda () (interactive) (find-alternate-file "..")))
              (define-key dired-mode-map "b" 'e/dired-up-directory)
              (define-key dired-mode-map "r" 'e/dired-open-file)
              (define-key dired-mode-map (kbd "C-c j") 'dired-two-panel)
              (define-key dired-mode-map "I"
                (lambda () (interactive) (info (dired-get-filename))))

              (require 'wdired)
              (setq wdired-allow-to-change-permissions t
                    wdired-allow-to-redirect-links t)

              (require 'dired-x)
              (put 'dired-find-alternate-file 'disabled nil)

              (dired-hide-details-mode)))

;; ** ESHELL
  (add-hook 'eshell-mode-hook
            (lambda ()
              (require 'eshell)
              (require 'em-smart)

              (add-to-list 'eshell-visual-commands "ssh")
              (add-to-list 'eshell-visual-commands "tail")
              (add-to-list 'eshell-visual-commands "htop")
              (add-to-list 'eshell-visual-commands "emacs")
              (add-to-list 'eshell-visual-commands "vim")

              (setq eshell-scroll-to-bottom-on-input 'all
                    eshell-visual-subcommands
                    '("git" "commit" "--amend" "log" "l" "diff" "show")
                    eshell-error-if-no-glob t
                    eshell-where-to-jump 'begin
                    eshell-review-quick-commands nil
                    eshell-smart-space-goes-to-end t
                    eshell-hist-ignoredups t
                    eshell-save-history-on-exit t
                    eshell-prefer-lisp-functions nil
                    eshell-destroy-buffer-when-process-dies t)

              (setenv "PATH" (concat "/usr/local/bin:/usr/local/sbin:"
                                     (getenv "PATH")))
              (getenv "PATH")
              (setenv "PAGER" "cat")

              (add-hook 'eshell-expand-input-functions
                        #'eshell-expand-history-references)

              (defun eshell/find-file (file)
                "Find FILE using Eshell."
                (find-file file))

              (defun e/eshell-runit ()
                "Open Eshell using directory associated with the current buffer's file.

      The eshell is renamed to match that
      directory to make multiple eshell windows easier."
                (interactive)
                (let* ((height (/ (window-total-height) 3)))
                  (split-window-vertically (- height))
                  (other-window 1)
                  (eshell "new")
                  (insert (concat "make -ks build && make -ks run"))
                  (eshell-send-input)))
              (global-set-key (kbd "C-c n") 'e/eshell-runit)

              (defun eshell/clear ()
                "Function to clear eshell buffer."
                (let ((eshell-buffer-maximum-lines 0))
                  (eshell-truncate-buffer)))

              (defun eshell/gst (&rest args)
                "Git status ARGS."
                (magit-status (pop args) nil)
                (eshell/echo))   ;; The echo command suppresses output

              (defun eshell/find (&rest args)
                "Wrapper around the ‘find’ executable using ARGS."
                (let ((cmd (concat "find " (string-join args))))
                  (shell-command-to-string cmd)))


              (defun eshell/hp (&rest args)
                "Emily run ARGS."
                (let ((cmd (concat "cero" *space* (string-join args))))
                  (shell-command-to-string cmd)))

              (defun eshell/dp (&rest args)
                "Emily - run distro ARGS."
                (let ((cmd (concat "distro" *space* (string-join args))))
                  (shell-command-to-string cmd)))

              (defun e/eshell-here ()
                "Open Eshell using directory associated with the current buffer's file.

      The eshell is renamed to match that
      directory to make multiple eshell windows easier."
                (interactive)
                (let* ((height (/ (window-total-height) 3)))
                  (split-window-vertically (- height))
                  (other-window 1)
                  (eshell "new")
                  (insert (concat "ls"))
                  (eshell-send-input)))
              (global-set-key (kbd "C-c E") 'e/eshell-here)

              (defun e/eshell-quit-or-delete-char (arg)
                "ARG."
                (interactive "p")
                (if (and (eolp) (looking-back eshell-prompt-regexp))
                    (progn
                      (eshell-life-is-too-much) ; Why not? (eshell/exit)
                      (ignore-errors
                        (delete-window)))
                  (delete-forward-char arg)))

              (add-hook 'eshell-mode-hook
                        (lambda ()
                          (global-set-key (kbd "C-d") 'e/eshell-quit-or-delete-char)))

              (defun e/eshell-there (host)
                "Eshell with Tramp automatically connect to a remote system, HOST.

      The hostname can be either the IP address, or FQDN,
      and can specify the user account, as in
      root@blah.com. HOST can also be a complete Tramp reference."
                (interactive "sHost: ")

                (let* ((default-directory
                         (cond
                          ((string-match-p "^/" host) host)

                          ((string-match-p (ha/eshell-host-regexp 'full) host)
                           (string-match (ha/eshell-host-regexp 'full) host)
                           (let* ((user1 (match-string 2 host))
                                  (host1 (match-string 3 host))
                                  (user2 (match-string 6 host))
                                  (host2 (match-string 7 host)))
                             (if host1
                                 (ha/eshell-host->tramp user1 host1)
                               (ha/eshell-host->tramp user2 host2))))

                          (t (format "/%s:" host)))))
                  (eshell-here)))

              (defun e/eshell-close ()
                "Close eshell."
                (insert "exit")
                (eshell-send-input)
                (delete-window))

              (defun eshell-next-prompt (n)
                "Move to end of Nth next prompt in the buffer.
  See `eshell-prompt-regexp'."
                (interactive "p")
                (re-search-forward eshell-prompt-regexp nil t n)
                (when eshell-highlight-prompt
                  (while (not (get-text-property (line-beginning-position) 'read-only) )
                    (re-search-forward eshell-prompt-regexp nil t n)))
                (eshell-skip-prompt))

              (defun eshell-previous-prompt (n)
                "Move to end of Nth previous prompt in the buffer.
  See `eshell-prompt-regexp'."
                (interactive "p")
                (backward-char)
                (eshell-next-prompt (- n)))

              (defun eshell-insert-history ()
                "Displays the eshell history to select and insert back into your eshell."
                (interactive)
                (insert (ido-completing-read "Eshell history: "
                                             (delete-dups
                                              (ring-elements eshell-history-ring)))))

              (add-hook 'eshell-mode-hook
                        (lambda ()
                          (define-key eshell-mode-map (kbd "M-S-P") 'eshell-previous-prompt)
                          (define-key eshell-mode-map (kbd "M-S-N") 'eshell-next-prompt)
                          (define-key eshell-mode-map (kbd "M-r") 'eshell-insert-history)))


              (add-hook 'after-save-hook
                        'executable-make-buffer-file-executable-if-script-p)))

;; ** SHELL
  (defun e/shell-here ()
    "Open `shell' using directory associated with the current buffer's file.

         The `shell' is renamed to match that
         directory to make multiple eshell windows easier."
    (interactive)
    (let* ((height (/ (window-total-height) 3)))
      (split-window-vertically (- height))
      (other-window 1)
      (shell)))

  (defun e/ansi-term-here ()
    "Open `ansi-term' using directory associated with the current buffer's file.

      The `ansi-term' is renamed to match that
      directory to make multiple eshell windows easier."
    (interactive)
    (let* ((height (/ (window-total-height) 3)))
      (split-window-vertically (- height))
      (other-window 1)
      (ansi-term "bash" "Mini-Shell")))
  (global-set-key (kbd "C-c T") #'e/ansi-term-here)

;; ** FLYMAKE
(add-hook 'prog-mode-hook
          (lambda ()
            ;; (require 'flymake)
            (flymake-mode)

            (local-set-key (kbd "C-c b f") 'flymake-show-diagnostics-buffer)
            (local-set-key (kbd "M-n") 'flymake-goto-next-error)
            (local-set-key (kbd "M-p") 'flymake-goto-prev-error)))



;; ** INFO
(add-hook 'Info-mode-hook
          (lambda ()
            (require 'info)
            (setq Info-additional-directory-list
                  (list (expand-file-name (concat *share* "/info/"))))

            ;; (concat (getenv "GUIX_PROFILE") "/share/info")
            ;; (add-to-list 'Info-directory-list "/usr/local/share/info/")

            (define-key Info-mode-map (kbd "W") 'define-word-at-point)))

;; ** VC
(add-hook 'prog-mode-hook
          (lambda ()
            (require 'vc)

            (setq vc-dir-backend 'git
                  vc-make-backup-files t)

            (define-key vc-prefix-map "=" 'vc-ediff)))

;; ** DIFF
(add-hook 'diff-mode-hook
          '(lambda ()
             ;; (require 'diff)
             (setq-local whitespace-style
                         '(face
                           tabs
                           tab-mark
                           spaces
                           space-mark
                           trailing
                           indentation::space
                           indentation::tab
                           newline
                           newline-mark))
             (whitespace-mode 1)))

;; ** EDIFF
(add-hook 'diff-mode-hook
          '(lambda ()
             (require 'ediff)

             (setq-default ediff-diff-options "-w"
                           ediff-split-window-function 'split-window-horizontally
                           ediff-window-setup-function 'ediff-setup-windows-plain)
             (add-hook 'ediff-after-quit-hook-internal 'winner-undo)))

;; ** OUTLINE
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (outline-minor-mode)
            (setq outline-regexp ";;\\ \\*")))



;; ** RECENTF
  (require 'recentf)
  (recentf-mode 1)
  (run-at-time nil (* 5 120) 'recentf-save-list)
  (setq recentf-max-saved-items 1000
        recentf-max-menu-items 60
        ;; disable recentf-cleanup on Emacs start, because it can cause
        recentf-auto-cleanup 'never ;; problems with remote files
        recentf-auto-cleanup 600  ;; clean up the recent files
        ;; exclude ** from recentfiles buffer
        recentf-exclude '("^/var/folders\\.*"
                          "COMMIT_MSG"
                          "[0-9a-f]\\{32\\}-[0-9a-f]\\{32\\}\\.org"
                          "github.*txt$"
                          "COMMIT_EDITMSG\\'"
                          ()			".*-autoloads\\.el\\'"
                          "recentf"
                          ".*pang$" ".*cache$"
                          "[/\\]\\.elpa/"))

  ;; Add visited dired visited directories to recentf
  (defun recentf-track-opened-file ()
    "Insert dired/file name just opened or written into the recent list."
    (let ((buff-name (or buffer-file-name
                         (and (derived-mode-p 'dired-mode)
                              default-directory))))
      (and buff-name
           (recentf-add-file buff-name)))
    ;; Must return nil because it is run from `write-file-functions'.
    nil)

  (defun recentf-track-closed-file ()
    "Update the recent list when a file or dired buffer is killed.
  That is, remove a non kept file from the recent list."
    (let ((buff-name (or buffer-file-name
                         (and
                          (derived-mode-p 'dired-mode)
                          default-directory))))
      (and buff-name
           (recentf-remove-if-non-kept buff-name))))
  (add-hook 'dired-after-readin-hook 'recentf-track-opened-file)

;; ** IBUFFER
;; (require 'ibuffer)
(global-set-key (kbd "C-c b i") 'ibuffer)
(defalias 'list-buffers 'ibuffer) ;; make ibuffer the default buffer manager.

;; (define-ibuffer-column size-h
;;   (:name "Size" :inline t)
;;   (file-size-human-readable (buffer-size))) ;


;; ;; Modify the default ibuffer-formats (toggle with `)
;; (setq ibuffer-formats
;;       '((mark modified read-only vc-status-mini " "
;;               (name 22 22 :left :elide)
;;               " "
;;               (size-h 9 -1 :right)
;;               " "
;;               (mode 12 12 :left :elide)
;;               " "
;;               vc-relative-file)
;;         (mark modified read-only vc-status-mini " "
;;               (name 22 22 :left :elide)
;;               " "
;;               (size-h 9 -1 :right)
;;               " "
;;               (mode 14 14 :left :elide)
;;               " "
;;               (vc-status 12 12 :left)
;;               " "
;;               vc-relative-file)))

;; (setq ibuffer-filter-group-name-face 'font-lock-doc-face)

;; ** ERC
(add-hook 'erc-mode-hook
	  '(lambda ()
	     (require 'erc)
	     (require 'erc-log)
	     (require 'erc-notify)
	     (require 'erc-spelling)
	     (require 'erc-autoaway)
	     (add-hook 'window-configuration-change-hook
		       '(lambda ()
			  (setq erc-fill-column (- (window-width) 2))))
	     (erc-track-remove-from-mode-line)
	     (setq-local scroll-margin 1)
	     (setq erc-rename-buffers t
		   erc-kill-buffer-on-part t
		   erc-mode-line-format "%a"
		   erc-kill-queries-on-quit t
		   erc-kill-server-buffer-on-quit t
		   erc-query-display 'buffer
		   erc-save-buffer-on-part t
		   erc-log-channels-directory (concat user-emacs-directory "erc/logs")
		   erc-server-coding-system '(utf-8 . utf-8)
		   erc-track-enable-kebyindings nil
		   erc-prompt-for-nickserv-password nil
		   erc-hide-timestamps t
		   erc-join-buffer 'bury
		   erc-interpret-mirc-color t
		   erc-spelling-dictionaries '(("#emacs" "american"))
		   erc-autojoin-channels-alist '(("freenode.net"
						  "#ruby"
						  "#guix"
						  "#bootstrappable"
						  "#emacs"))
		   erc-lurker-hide-list '("JOIN" "PART" "QUIT")
		   erc-track-exclude-types
		   '("JOIN" "NICK" "PART" "QUIT" "MODE"
		     "324" "329" "332" "333" "353" "477"))
	     (add-to-list 'erc-modules 'notifications)
	     (add-to-list 'erc-modules 'spelling)
	     (erc-track-mode t)
	     (erc-spelling-mode 1)
	     (erc-truncate-mode +1)
	     (erc-services-mode 1)
	     (erc-update-modules)))

;; ** DOC-VIEW
(require 'doc-view)
(setq doc-view-continuous t
      doc-view-resolution 400)

(add-hook 'auto-revert-mode 'doc-view)

(define-key doc-view-mode-map (kbd "/") #'doc-view-reverse-colors)

(defun define-doc-view-current-cache-dir ()
  ;; doc-view-current-cache-dir was renamed to doc-view--current-cache-dir in Emacs 24.5
  (or (fboundp 'doc-view-current-cache-dir)
      (defalias 'doc-view-current-cache-dir 'doc-view--current-cache-dir)))
(eval-after-load "doc-view" '(define-doc-view-current-cache-dir))

(defun doc-view-reverse-colors ()
  "Inverts document colors.\n
Requires an installation of ImageMagick (\"convert\")."
  (interactive)
  ;; error out when ImageMagick is not installed
  (if (/= 0 (call-process-shell-command "convert -version"))
      (error "Reverse colors requires ImageMagick (convert)")
    (when (eq major-mode 'doc-view-mode)
      ;; assume current doc-view internals about cache-names
      (let ((file-name (expand-file-name (format "page-%d.png"
						 (doc-view-current-page))
					 (doc-view-current-cache-dir))))
	(call-process-shell-command
	 "convert" nil nil nil "-negate" file-name file-name)
	(clear-image-cache)
	(doc-view-goto-page (doc-view-current-page))))))

(defun doc-view-reverse-colors-all-pages ()
  "Inverts document colors on all pages.\n
Requires an installation of ImageMagick (\"convert\")."
  (interactive)
  ;; error out when ImageMagick is not installed
  (if (/= 0 (call-process-shell-command "convert -version"))
      (error "Reverse colors requires ImageMagick (convert)")
    (when (eq major-mode 'doc-view-mode)
      ;; assume current doc-view internals about cache-names
      (let ((orig (doc-view-current-page))
	    (page nil))
	(message "Reversing video on all pages...")
	(dotimes (pnum (doc-view-last-page-number))
	  (setq page (expand-file-name (format "page-%d.png" (1+ pnum))
				       (doc-view-current-cache-dir)))
	  (call-process-shell-command
	   "convert" nil nil nil "-negate" page page))
	(clear-image-cache)
	(doc-view-goto-page orig)
	(message "Done reversing video!")))))


(easy-menu-define e/doc-view-menu doc-view-mode-map "Menu for Doc-View Mode."
  '("DocView"
    ["Switch to a different mode" doc-view-toggle-display
     :help "Switch to a different mode"]
    ["Open Text" doc-view-open-text
     :help "Display the current doc's contents as text"]
    "--"
    ("Navigate Doc"
     ["Goto Page ..." doc-view-goto-page
      :help "View the page given by PAGE"]
     "--"
     ["Scroll Down" doc-view-scroll-down-or-previous-page
      :help "Scroll page down ARG lines if possible, else goto previous page"]
     ["Scroll Up" doc-view-scroll-up-or-next-page
      :help "Scroll page up ARG lines if possible, else goto next page"]
     "--"
     ["Next Line" doc-view-next-line-or-next-page
      :help "Scroll upward by ARG lines if possible, else goto next page"]
     ["Previous Line" doc-view-previous-line-or-previous-page
      :help "Scroll downward by ARG lines if possible, else goto previous page"]
     ("Customize"
      ["Continuous Off"
       (setq doc-view-continuous nil)
       :help "Stay put in the current page, when moving past first/last line"
       :style radio :selected
       (eq doc-view-continuous nil)]
      ["Continuous On"
       (setq doc-view-continuous t)
       :help "Goto to the previous/next page, when moving past first/last line"
       :style radio :selected
       (eq doc-view-continuous t)]
      "---"
      ["Save as Default"
       (customize-save-variable 'doc-view-continuous doc-view-continuous)
       t])
     "--"
     ["Next Page" doc-view-next-page :help "Browse ARG pages forward"]
     ["Previous Page" doc-view-previous-page :help "Browse ARG pages backward"]
     "--"
     ["First Page" doc-view-first-page :help "View the first page"]
     ["Last Page" doc-view-last-page :help "View the last page"])
    "--"
    ("Adjust Display"
     ["Enlarge" doc-view-enlarge :help "Enlarge the document by FACTOR"]
     ["Shrink" doc-view-shrink :help "Shrink the document"]
     "--"
     ["Fit Width To Window" doc-view-fit-width-to-window
      :help "Fit the image width to the window width"]
     ["Fit Height To Window" doc-view-fit-height-to-window
      :help "Fit the image height to the window height"]
     "--"
     ["Fit Page To Window" doc-view-fit-page-to-window
      :help "Fit the image to the window"]
     "--"
     ["Set Slice From Bounding Box" doc-view-set-slice-from-bounding-box
      :help "Set the slice from the document's BoundingBox information"]
     ["Set Slice Using Mouse" doc-view-set-slice-using-mouse
      :help "Set the slice of the images that should be displayed"]
     ["Set Slice" doc-view-set-slice
      :help "Set the slice of the images that should be displayed"]
     ["Reset Slice" doc-view-reset-slice
      :help "Reset the current slice"])
    ("Search"
     ["New Search ..."
      (doc-view-search t)
      :help
      "Jump to the next match or initiate a new search if NEW-QUERY is given"]
     "--"
     ["Search" doc-view-search
      :help
      "Jump to the next match or initiate a new search if NEW-QUERY is given"]
     ["Backward" doc-view-search-backward
      :help "Call `doc-view-search' for backward search"]
     "--"
     ["Show Tooltip" doc-view-show-tooltip
      :help nil])
    ("Maintain"
     ["Reconvert Doc" doc-view-reconvert-doc
      :help "Reconvert the current document"]
     "--"
     ["Clear Cache" doc-view-clear-cache
      :help "Delete the whole cache (`doc-view-cache-directory')"]
     ["Dired Cache" doc-view-dired-cache
      :help "Open `dired' in `doc-view-cache-directory'"]
     "--"
     ["Revert Buffer" doc-view-revert-buffer
      :help "Like `revert-buffer', but preserves the buffer's current modes"]
     "--"
     ["Kill Proc" doc-view-kill-proc
      :help "Kill the current converter process(es)"]
     ["Kill Proc And Buffer" doc-view-kill-proc-and-buffer
      :help "Kill the current buffer"])
    "--"
    ["Customize"
     (customize-group 'doc-view)]))
(easy-menu-define e/doc-view-minor-mode-menu doc-view-minor-mode-map
  "Menu for Doc-View Minor Mode."
  '("DocView*"
    ["Display in DocView Mode" doc-view-toggle-display
     :help "View"]
    ["Exit DocView Mode" doc-view-minor-mode]))

;; ** FLYSPELL
(require 'flyspell)
(if (executable-find "aspell")
    (progn
      (setq ispell-program-name "aspell"
            ispell-extra-args '("--sug-mode=ultra")))
  (setq ispell-program-name "ispell"))
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
;; (dolist (hooks '(org-mode-hook
;;                  text-mode-hook))
;;   (add-hook hooks 'flyspell-mode))

;; ** ICOMPLETE
(require 'icomplete)
(icomplete-mode 1)   ;; Enable icomplete as much as possible



;; ** ABBREV
;; (require 'abbrev)
;; (add-hook 'text-mode-hook 'abbrev-mode) ;; abbrev config
;; (setq-default abbrev-mode t)  ;; turn on abbrev mode globally
;; (setq save-abbrevs 'silently)

(defun xah-global-abbrev-position-cursor (&optional @pos)
  "Move cursor back to ^ if exist, else put at end.
        Return true if found, else false. Version 2016-10-24"
  (interactive)
  (let (($found-p (search-backward "^" (if @pos
                                           @pos
                                         (max (point-min) (- (point) 100)))
                                   t)))
    (when $found-p (delete-char 1))
    $found-p))

(advice-add 'expand-abbrev :after (lambda () (xah-global-abbrev-position-cursor)))
;; mimics yasnippet point move to  !
(defadvice expand-abbrev (after move-to-point activate)
  (xah-global-abbrev-position-cursor))


;; ** SAVEPLACE
(require 'saveplace)
(save-place-mode 1)
(setq-default save-place t)
(setq save-place-file (expand-file-name "etc/places" user-emacs-directory)
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "etc/backups"))))

;; ** GO-TO-ADDRESS
(add-hook 'prog-mode-hook 'goto-address-mode)
(add-hook 'text-mode-hook 'goto-address-mode)


;; ** HIPPIE-EXPAND
(require 'hippie-exp)

(global-set-key (kbd "<C-tab>") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        ;; Try to expand word "dynamically", searching the current buffer.
        try-expand-dabbrev
        ;; Try to expand word "dynamically", searching all other buffers.
        try-expand-dabbrev-all-buffers
        ;; Try to expand word "dynamically", searching the kill ring.
        try-expand-dabbrev-from-kill
        ;; Try to complete text as a file name, as many characters as unique.
        try-complete-file-name-partially
        ;; Try to complete text as a file name.
        try-complete-file-name
        ;; Try to complete as an Emacs Lisp symbol, as many characters as unique.
        try-complete-lisp-symbol-partially
        ;; Try to complete word as an Emacs Lisp symbol.
        try-complete-lisp-symbol
        ;; Try to expand word before point according to all abbrev tables.
        try-expand-all-abbrevs
        ;; Try to complete the current line to an entire line in the buffer.
        try-expand-list
        ;; Try to complete the current line to an entire line in the buffer.
        try-expand-line))


;; ** IMAGE MODE
(add-hook 'image-mode-hook
          (lambda () ;; open next/previous image fitted
            (local-set-key (kbd "<right>") (defun next-image-fitted ()
                                             (interactive)
                                             (image-next-file)
                                             (image-transform-fit-to-width)))
            (local-set-key (kbd "<left>") (defun left-image-fitted ()
                                            (interactive)
                                            (image-previous-file 1)
                                            (image-transform-fit-to-width)))))

;; ** WHITESPACE
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook
          (lambda () (interactive) (setq show-trailing-whitespace 1)))
(add-hook 'org-mode-hook
          (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq show-trailing-whitespace t
      whitespace-style '(face lines-tail))


;; ** SAVEHIST
(require 'savehist)
(savehist-mode t)
(setq savehist-save-minibuffer-history t
      savehist-autosave-interval nil
      ;; savehist-file (expand-file-name "etc/savehist" user-emacs-directory)
      savehist-save-minibuffer-history 1
      savehist-additional-variables '(Info-history-list)
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring))


;; ** EWW
(with-eval-after-load "eww"
  (define-key eww-mode-map (kbd "W") 'define-word-at-point))

;; ** WINDMOVE
(require 'windmove)
(setq windmove-wrap-around t )

;; ** PRETTIFY-SYMBOLS
(global-prettify-symbols-mode 1)
(setq prettify-symbols-alist '(("lambda" . 955)))
(defun e/add-pretty-lambda ()
  "Lisp symbols as pretty Unicode symbols."
  (setq prettify-symbols-alist
        '(("lambda" . 955)  ;; λ
          ("->" . 8594)     ;; →
          ("=>" . 8658)     ;; ⇒
          ("map" . 8614)))) ;; ↦
(add-hook 'scheme-mode-hook 'e/add-pretty-lambda)
(add-hook 'clojure-mode-hook 'e/add-pretty-lambda)
(add-hook 'haskell-mode-hook 'e/add-pretty-lambda)

;; ** WINNER
(winner-mode 1)
;; (remove-hook 'minibuffer-setup-hook 'winner-save-unconditionally)


;; ** MIDNIGHT
(require 'midnight)
(midnight-delay-set 'midnight-delay "4:30am")
(setq midnight-period 5000)


;; ** UNIQUIFY
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-after-kill-buffer-p t
      uniquify-separator "/"
      uniquify-ignore-buffers-re "^\\*")

;; ** ISEARCH
(defun e/isearch-exit-other-end ()
  "Exit isearch, at the opposite end of the string."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))

(define-key isearch-mode-map [(control return)] #'isearch-exit-other-end)
;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-r") 'isearch-backward-regexp)
;; (global-set-key (kbd "C-M-s") 'isearch-forward)
;; (global-set-key (kbd "C-M-r") 'isearch-backward)

;; ** ELDOC
(add-hook 'eldoc-mode-hook
          (lambda ()
            (require 'eldoc)
            ;; (global-eldoc-mode)
            eldoc-echo-area-use-multiline-p nil

            (setq eldoc-idle-delay 0.1)))


;; ** PAREN
;; (require 'paren)

;; Visually indicate matching pairs of parentheses.
(show-paren-mode 1)

;; ====================
;; * MODOS LINGUAGENS
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

;; ============================
;; * VARIAVEIS GLOBAIS

(defconst *site-lisp* (concat user-emacs-directory "site-lisp")
  "Emacs site-lisp folder.")

(defconst *lar* (getenv "HOME")
  "$HOME folder location.")

(defconst *xdg-config* (concat *lar* "/.config")
  "XDG Config folder.")

(defconst *local* (concat *lar* "/.local")
  "$HOME/.local folder.")

(defconst *share* (concat *local* "/share")
  "XDG local share folder.")

(defconst *download* (concat *lar* "/Downloads")
  "$HOME/Download folder.")

(defgroup *vars* nil
  "CUSTOM variables set by user"
  :group 'extensions
  :group 'convenience)

(defcustom *video* (concat *lar* "/Videos")
  "Folder to save videos."
  :type 'string
  :group '*vars*)

(defcustom *musica* (concat *lar* "/Musica")
  "Folder to save songs."
  :type 'string
  :group '*vars*)

(defcustom *space* "\s"
  "Folder to save songs."
  :type 'string
  :group '*vars*)

(defcustom *and* "\s&&\s"
  "Folder to save songs."
  :type 'string
  :group '*vars*)

;; ===============
;; * SYSTEM SOFTWARE

(defun e/return-exec (apps)
  "Return first executable in APPS found."
  (require 'cl-lib)
  (cl-dolist (app apps)
    (when (executable-find app)
      (cl-return app))))

(defcustom *player* "mpv"
  "Default video/audio player."
  :type 'string
  :group '*vars*)

(defcustom *browser* (e/return-exec '("firefox" "google-chrome"))
  "Default Internet Browser."
  :type 'string
  :group '*vars*)

(defcustom *downloader* (e/return-exec '("wget" "curl"))
  "Check if listed Downloaders exist, pick in order, else ask for one."
  :group '*vars*
  :type 'string)

(defcustom *media-downloader* (e/return-exec '("youtube-dl"))
  "Youtube-dl - Media downloader."
  :type 'string)

(defcustom *ffmpeg-p* (when (e/return-exec '("ffmpeg")) t)
  "FFMPEG - confirm if it installed."
  :type 'boolean)

;; ** PACOTES DE SISTEMAS INTERFACE
;; *** YOUTUBE-DL
(defun e/get-video (url)
  "Download Video w/ URL - GPL-3.0."
  (interactive "p")
  (let ((default-directory *video*))
	(start-process "VIDEO-DOWNLOADER" "VIDEO-DOWNLOADER" *media-downloader* (current-kill 0 t))
	(message "Watch progress at VIDEO buffer!")))
(global-set-key (kbd "C-c f V") #'(lambda () (interactive) (e/get-video (current-kill 0 t))))

(defun e/get-audio (url)
  "Download Audio w/ URL - GPL-3.0!"
  (interactive "p")
  (let ((default-directory *musica*)
	      (format "--extract-audio")
	      (audio-format "--audio-format")
	      (audio-extension "vorbis"))
	  (start-process "AUDIO-DOWNLOADER" "AUDIO-DOWNLOADER" *media-downloader* format audio-format audio-extension url)
	  (message "Watch progress at AUDIO buffer!")))
(global-set-key (kbd "C-c f A") #'(lambda () (interactive) (e/get-audio (current-kill 0 t))))

(defun e/play-video (&optional url)
  "Call Video Player with online video's URL on clipboard!
  Default to `mpv' | GPLv3."
  (interactive)

  (let ((url (if url
		 url
	       ;; "use current clipboard string"
	       (setq url (current-kill 0 t)))))

    (start-process "VIDEO-TO-PLAYER" nil *player* url)
    (message "Url: %s" url)
    (message "Playing video with %s in an instant!" *player*)))
(global-set-key (kbd "C-c f p") 'e/play-video)

;; *** FFMPEG - ffmpeg features using Dired
(defun e/dired-ffmpeg-convert-to-format ()
  "Ffmpeg convert file format."
  (interactive)
  (let* ((file (dired-get-filename nil t))
	 (output-format (read-from-minibuffer "Format to convert to: ")))
    (if *ffmpeg-p*
	(start-process "FFMPEG-FORMAT"
		       "FFMPEG-FORMAT"
		       "ffmpeg" "-i"
		       file (concat (file-name-base file) "." output-format))
      (message "FFMPEG IS NOT INSTALLED"))))
(global-set-key (kbd "C-c C-f f") 'e/dired-ffmpeg-convert-to-format)

(defun e/dired-ffmpeg-boost-volume ()
  "Ffmpeg boost file volume."
  (interactive)
  (let* ((file (dired-get-filename nil t))
	 (volume (read-from-minibuffer "Volume boost quantity (n): "))
	 (extension (file-name-extension (dired-get-filename nil t))))
    (if *ffmpeg-p*
	(start-process "FFMPEG-BOOST"
		       "FFMPEG-BOOST"
		       "ffmpeg" "-i" file "-filter:a"
		       (concat "volume=" volume)
		       (concat (file-name-base file) "-louder." extension))
      (message "FFMPEG IS NOT INSTALLED"))))
(global-set-key (kbd "C-c C-f f") 'e/dired-ffmpeg-boost-volume)

(defun e/send-file ()
  "Send file to mobile phone."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (start-process "SENDING" "SENDING" "cero" "operations" "send" file)))
(global-set-key (kbd "C-c S") 'e/send-file)


(defun e/barinfo ()
  "Apresenta informacoes do sistema."
  (interactive)
  (let ((info (string-trim (shell-command-to-string "wmbar-info"))))
    (progn
      (start-process "A" "A" "notify-send" info)
      (message "%s" info))))

;; ** e/FUNCTIONS
;; *** MISC
(defun my-dired-convert-epub-to-org ()
  "In dired, convert Epub file to Org using pandoc."
  (interactive)
  (dolist (file (dired-get-marked-files))
    (start-process "EPUB-ORG" "EPUB-ORG" "pandoc" file "--columns=120" "-f" "epub" "-t" "org" "-s" "-o"
		   (concat (substring file 0 -4) "org"))))


(defun my-dired-find-and-clean-novels ()
  "In dired, convert Epub file to Org using pandoc."
  (interactive)
  (dolist (file (dired-get-marked-files))
    (find-file file)
    (my-novels-clean)))


(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "Emacs" nil ".svg"))
	 (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

(defun my-novels-clean ()
  "Cleaning Org Novels."
  (interactive)

  (save-excursion
    (outline-show-all)
    (goto-char 1)
    (while (ignore-errors (re-search-forward "<<.*?>>"))
      (kill-whole-line)))

  (save-excursion
    (goto-char 1)
    (while (ignore-errors (re-search-forward "\\[\\[[a-z]+:[a-z]+\.[a-z]+#[a-z]+_[0-9]+\\]\\[\\[[0-9]+]\\]\\]"))
      (replace-match "")))

  (save-excursion
    (outline-show-all)
    (goto-char 1)
    (while (ignore-errors (re-search-forward "---"))
      (replace-match " - ")))

  (save-excursion
    (outline-show-all)
    (goto-char 1)
    (while (ignore-errors (re-search-forward " "))
      (replace-match " ")))

  (save-excursion
    (outline-show-all)
    (goto-char 1)
    (while (ignore-errors (re-search-forward "file:images"))
      (kill-whole-line)))

  (save-excursion
    (outline-show-all)
    (goto-char 1)
    (while (ignore-errors (re-search-forward "+TITLE:"))
      (kill-whole-line)))

  (save-excursion
    (outline-show-all)
    (goto-char 1)
    (while (ignore-errors (re-search-forward "+AUTHOR"))
      (kill-whole-line)))

  (save-excursion
    (outline-show-all)
    (goto-char 1)
    (while (ignore-errors (re-search-forward "+DATE:"))
      (kill-whole-line)))

  (save-excursion
    (outline-show-all)
    (goto-char 1)
    (while (ignore-errors (re-search-forward ":PROPERTIES:"))
      (kill-whole-line)))

  (save-excursion
    (outline-show-all)
    (goto-char 1)
    (while (ignore-errors (re-search-forward ":LANGUAGE"))
      (kill-whole-line)))

  (save-excursion
    (outline-show-all)
    (goto-char 1)
    (while (ignore-errors (re-search-forward ":END:"))
      (kill-whole-line)))

  (save-excursion
    (goto-char 1)
    (while (ignore-errors (re-search-forward " $"))
      (kill-whole-line)))

  (save-excursion
    (outline-show-all)
    (goto-char 1)
    (while (ignore-errors (re-search-forward ":CLASS:"))
      (kill-whole-line)))

  (save-excursion
    (goto-char 1)
    (while (ignore-errors (re-search-forward "\\\\"))
      (replace-match "")))

  (save-excursion
    ;; (indent-buffer)
    (save-buffer)
    (kill-current-buffer)))


(defun e/search-engine ()
  "Search term on internet search engines/repositories.

     By default use word-at-point or ask for a term."
  (interactive)

  (let* ((word-at-point (word-at-point))
	 (symbol-at-point (symbol-at-point))
	 (term-at-point (symbol-name symbol-at-point))
	 (term-buffer (read-from-minibuffer ;; Ask for a term to be entered.
		       (if (or word-at-point symbol-at-point)
			   (concat
			    "Symbol (default "
			    term-at-point
			    "): ") "Search for: (no default): "))))

    ;; use term-at-point or term-buffer
    (let ((term (if (string= term-buffer "")
		    term-at-point
		  term-buffer))
	  ;; Pick a search engine
	  (search-type
	   (read-from-minibuffer
	    "Search in Google, .NetDocs, GitHub, SoV, DDG? ")))
      (cond
       ((string-equal search-type "")
	(browse-url (concat "https://www.google.com/search?ion=1&q=" term)))
       ((string-equal search-type "g")
	(browse-url (concat "https://github.com/search?q=" term)))
       ((string-equal search-type "r")
	(browse-url (concat "https://www.reddit.com/search/?q=" term)))
       ((string-equal search-type "s")
	(browse-url (concat "https://stackoverflow.com/search?q=" term)))
       ((string-equal search-type "n")
	(browse-url (concat "https://docs.microsoft.com/en-us/search/?search="
			    term
			    "&category=All&scope=.NET&category=All")))
       ((string-equal search-type "d")
	(browse-url (concat "https://duckduckgo.com/?q=" term)))))))
(global-set-key (kbd "C-c f s") 'e/search-engine)

(defun e/dired-count-marked-files ()
  "Count marked files."
  (interactive)
  (let ((count 0))
    (dolist (file (dired-get-marked-files))
      (setq count (1+ count))
      (message "%s" count))))

(defun e/kill-this-buffer-for-real ()
  "Kill the current buffer and window."
  (interactive)
  (kill-this-buffer)
  (delete-window))
(global-set-key (kbd "C-c k") 'e/kill-this-buffer-for-real)

(defun e/change-theme (&rest args)
  "Like `load-theme', but disable all themes before loading the new one, ARGS."
  ;; The `interactive' magic is for creating a future-proof passthrough.
  (interactive (advice-eval-interactive-spec
		(cadr (interactive-form #'load-theme))))
  (mapc #'disable-theme custom-enabled-themes)
  (apply (if (called-interactively-p 'any) #'funcall-interactively #'funcall)
	 #'load-theme args))

(defun e/create-scratch-buffer ()
  "Create a new scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (emacs-lisp-mode))

(defun e/sudo-edit (&optional arg)
  "Edit currently visited file as root.

     With a prefix ARG prompt for a file to visit.
     Will also prompt for a file to visit if current
     buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
			 (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun e/copy-line ()
  "Copy entire line - aboabo."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (kill-ring-save
     (point)
     (line-end-position)))
  (message "1 line copied"))
(global-set-key (kbd "C-c w") 'e/copy-line)

(defun e/surround (open)
  "Replace pair at point by OPEN and its corresponding closing character.
     The closing character is lookup in the syntax table or asked to
     the user if not found."
  (interactive
   (list
    (read-char
     (format "Replacing pair %c%c by (or hit RET to delete pair):"
	     (char-after)
	     (save-excursion
	       (forward-sexp 1)
	       (char-before))))))
  (if (memq open '(?\n ?\r))
      (delete-pair)
    (let ((close (cdr (aref (syntax-table) open))))
      (when (not close)
	(setq close
	      (read-char
	       (format "Don't know how to close character %s (#%d) ; please provide a closing character: "
		       (single-key-description open 'no-angles)
		       open))))
      (e/surround-replace-pair open close))))
(global-set-key (kbd "C-c s") 'e/surround)

(defun e/surround-replace-pair (open close)
  "Replace pair at point by respective chars OPEN and CLOSE.
     If CLOSE is nil, lookup the syntax table. If that fails, signal
     an error."
  (let ((close (or close
		   (cdr-safe (aref (syntax-table) open))
		   (error "No matching closing char for character %s (#%d)"
			  (single-key-description open t)
			  open)))
	(parens-require-spaces))
    (insert-pair 1 open close))
  (delete-pair)
  (backward-char 1))

;; ** e/MACROS
;; ---------------
(defmacro assoc-val (var vars)
  "Get value(cdr) of alist."
  `(cdr (assoc ',var ,vars)))

(defmacro assoc-key (var vars)
  "Get key(car) of alist."
  `(car (assoc ',var ,vars)))

;; ** e/PREDICATES
(defun pack-installed-p (package)
  "Predicate: Confirm if PACKAGE is installed."
  (when (executable-find package)
    t))

;;; ------------------------------------------------------------
;;; * PACOTES EXTERNOS

(defcustom e/packages (quote
 (eglot jetbrains-darcula-theme nixpkgs-fmt nixos-options nix-mode counsel selectrum-prescient yari yard-mode bundler ruby-compilation company-inf-ruby shackle rspec-mode rubocopfmt ruby-tools rufo robe rubocop ruby-test-mode chruby rbenv rvm git-link buffer-expose org-make-toc lua-mode lsp-ui org-journal darkroom slime-company highlight-symbol bufler magit emms doom-themes 0blayout hgignore-mode basic-ide typescript-mode editorconfig yaml-mode company-box vterm ox-epub outline-toc helm deadgrep fish-completion org-present emmet-mode helpful highlight-indent-guides company-shell outshine json-mode fish-mode gnu-elpa-keyring-update indent-guide hl-todo rainbow-mode dap-mode flymake-json gitignore-mode gitattributes-mode wgrep writeroom-mode dired-sidebar dired-ranger peep-dired all-the-icons-dired ido-at-point ido-hacks flx-ido beacon which-key expand-region fill-column-indicator rainbow-delimiters erc-hl-nicks erc-image elfeed org-bullets gif-screencast goto-chg yasnippet-snippets htmlize markdown-toc company-statistics company-quickhelp aggressive-indent pdf-tools eshell-prompt-extras esh-autosuggest esh-help anzu google-translate bug-hunter dired-collapse dired-git-info langtool smartparens olivetti nov doom-modeline imgbb webpaste flymake-shellcheck flymake-diagnostic-at-point crux paradox eyebrowse ido-completing-read+ grip-mode iedit multiple-cursors diff-hl zoom define-word luarocks languagetool ansi package-build shut-up epl git commander f dash s cask dockerfile-mode frog-jump-buffer isolate litable eshell-toggle eshell-syntax-highlighting eshell-did-you-mean rake prettier-js binder csharp-mode rust-mode php-mode use-package))
  "Ah, esses pacotes ma-ra-vi-lho-sos."
  :type 'list
  :group 'elx)

(defun e/install-external-packages (lst)
  "Install LST list."
  (interactive)

  (package-refresh-contents)
  (dolist (package lst)
    (when (not (package-installed-p package))
      (package-install package))))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (e/install-external-packages '(use-package s f dash)))

(unless (require 'magit nil :noerror)
  (e/install-external-packages e/packages))

(when (require 'use-package nil :noerror)
  (require 'use-package)
	 (setq-default use-package-verbose nil
		       use-package-expand-minimally t
		       use-package-enable-imenu-support t))

(use-package f)
(use-package s)
(use-package dash)

;;(defvar formatters '((ruby-mode . 'rubocopfmt))
;;"List of formmater per major-mode")

;;(defun choose-formatter
    ;;(let ((formatter (get major-mode (buffer-expose-mode))))))

;; * REPOSITORIES PACKAGES
;; ** REPOSITORIES PACKAGES - DIRED EXTRA

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

;; ** REPOSITORIES PACKAGES - COMPLETION EXTRA

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

(use-package selectrum
  :disabled
  :defer 1
  :config
  (ido-mode 0)
  (selectrum-mode +1))

(use-package selectrum-prescient
  :disabled
  :defer 1
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

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
  :hook (prog-mode . outshine-mode))

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

(use-package nov
  :defer t
  :init (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :config
  (add-hook 'nov-mode-hook 'olivetti-mode)
  (define-key nov-mode-map (kbd "w") 'define-word-at-point))

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

(use-package pdf-tools
  :defer 3
  :config
  (pdf-tools-install)
  ;; (add-hook 'pdf-tools-enabled-hook 'auto-revert-mode)
  ;; (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
  (setq pdf-annot-activate-created-annotations t
	pdf-view-resize-factor 1.1)
  ;; (define-key pdf-view-mode-map (kbd "C-n") 'pdf-view-next-page-command)
  ;; (define-key pdf-view-mode-map (kbd "C-p") 'pdf-view-previous-page-command)
  ;; (define-key pdf-view-mode-map (kbd "n") 'pdf-view-next-line-or-next-page)
  ;; (define-key pdf-view-mode-map (kbd "p")'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "/") 'pdf-view-midnight-minor-mode)

  ;; --------------- pdf page number on mode-line
  (define-pdf-cache-function pagelabels)
  (add-hook 'pdf-view-mode-hook
	    (lambda ()
	      (setq-local mode-line-position
			  '(" ("
			    ;; (:eval (nth (1- (pdf-view-current-page))
			    ;; 		  (pdf-cache-pagelabels)))
			    ;; "/"
			    (:eval (number-to-string
				    (pdf-view-current-page)))
			    "/"
			    (:eval (number-to-string
				    (pdf-cache-number-of-pages)))")"))))

  ;; --------------- pdf-tools reopen last page
  ;; https://github.com/politza/pdf-tools/issues/18#issuecomment-269515117

  (defun e/pdf-set-last-viewed-bookmark ()
    (interactive)
    (when (eq major-mode 'pdf-view-mode)
      (bookmark-set (e/pdf-generate-bookmark-name))))

  (defun e/pdf-jump-last-viewed-bookmark ()
    (bookmark-set "fake") ; this is new
    (when
	(e/pdf-has-last-viewed-bookmark)
      (bookmark-jump (e/pdf-generate-bookmark-name))))

  (defun e/pdf-has-last-viewed-bookmark ()
    (assoc
     (e/pdf-generate-bookmark-name) bookmark-alist))

  (defun e/pdf-generate-bookmark-name ()
    (concat "PDF-LAST-VIEWED: " (buffer-file-name)))

  (defun e/pdf-set-all-last-viewed-bookmarks ()
    (dolist (buf (buffer-list))
      (with-current-buffer buf
	(e/pdf-set-last-viewed-bookmark))))

  (add-hook 'kill-buffer-hook 'e/pdf-set-last-viewed-bookmark)
  (add-hook 'pdf-view-mode-hook 'e/pdf-jump-last-viewed-bookmark)
  (unless noninteractive
    (add-hook 'kill-emacs-hook #'e/pdf-set-all-last-viewed-bookmarks)))

(use-package google-translate
  :defer 1
  :config (require 'google-translate-smooth-ui)
  :custom
  (google-translate-translation-directions-alist
   '(("pt" . "en") ("en" . "fr") ("en" . "de"))))


 (use-package ereader
   :defer 1
   :config
   (add-hook 'ereader-mode-hook
	     (lambda ()
	       (face-remap-add-relative 'variable-pitch
					:family "Hack"
					:height 1.4)))
   (setq visual-fill-column-center-text t)
   (add-hook 'ereader-mode-hook 'visual-line-mode))

;; ** REPOSITORIES PACKAGES - LANGUAGES

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

(use-package flycheck
  :after lsp-mode
  :custom (flycheck-checker-error-threshold 1400))

(use-package lsp-mode
  :defer 1
  :hook
  (ruby-mode . lsp)
  (lua-mode . lsp)
  (js-mode . lsp)
  (html-mode . lsp)
  (css-mode . lsp)
  :commands lsp
  :custom
  (lsp-enable-indentation nil)
  (lsp-signature-auto-activate nil)
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

(use-package feather
  :disabled
  :config (feather-mode))

(use-package leaf
  :disabled
  :config (feather-mode))

(use-package leaf-keywords
  :disabled
  :config (leaf-keywords-init))

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

(provide 'init)
 ;;; init.el ends here
