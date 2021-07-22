;;; package --- Builtin Packages Configurations -*- lexical-binding: t;

;;; Commentary:
;;; Code:

;; =========================
;; * BUILTIN PACKAGES
;; =========================

(after! erc
  (setq-local scroll-margin 1)
  (setq erc-nick "easbarbosa"
	erc-user-full-name "EAS Barbosa"
        erc-server "irc.libera.chat"
	erc-rename-buffers t
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
	erc-autojoin-channels-alist '(("libera.chat"
				       "#debian"
				       "#guix"
                                       "#ruby"
				       "#scheme"
				       "#lua"
				       "#emacs"))
	erc-lurker-hide-list '("JOIN" "PART" "QUIT")
	erc-track-exclude-types
	'("JOIN" "NICK" "PART" "QUIT" "MODE"
	  "324" "329" "332" "333" "353" "477")))

(after! org
  (setq-hook! org-mode truncate-lines t)

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
       "\C-x\C-s" #'org-edit-src-exit)))

(add-hook 'image-mode-hook
          (lambda ()
            ;; open next/previous image fitted
            (local-set-key (kbd "<right>") (lambda ()
				             (interactive)
				             (image-next-file)
				             (image-transform-fit-to-width)))
            (local-set-key (kbd "<left>") (lambda ()
				            (interactive)
				            (image-previous-file 1)
				            (image-transform-fit-to-width)))))

;; (use-package ido
;;   :init
;;   (setq ido-everywhere t)
;;   (ido-mode 1)

;;   (setq ido-create-new-buffer 'always
;; 	ido-enable-prefix nil
;; 	ido-enable-regexp t
;; 	ido-decorations
;; 	(quote ("\n-> " "" "\n " "\n ..." "[" "]" " [No match]" " [Matched]" "
;; 	     [Not readable]" " [Too big]" " [Confirm]"))
;; 	ido-file-extensions-order '(".lisp" ".py" ".org" ".el")
;; 	ido-max-directory-size 100000
;; 	ido-use-filename-at-point t
;; 	ido-enable-dot-prefix t
;; 	ido-use-url-at-point t
;; 	ido-use-filename-at-point 'guess
;; 	ido-use-virtual-buffers t)

;;   (define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil)

;;   (add-hook 'ido-setup-hook
;; 	    (lambda ()
;; 	      (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
;; 	      (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))

;;   (defun e/ido-bookmark-jump (bname)
;;     "Switch to bookmark BNAME interactively using `ido'."
;;     (interactive
;;      (list (ido-completing-read "Bookmark: " (bookmark-all-names) nil t)))
;;     (bookmark-jump bname))

;;   (defun e/recentf-ido-find-file ()
;;     "Find a recent file using Ido."
;;     (interactive)
;;     (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
;;       (when file
;; 	(find-file file))))
;;   (global-set-key (kbd "C-c r") 'e/recentf-ido-find-file)

;;   (defun e/mx-ido ()
;;     "Open Mx in ido-fashioned way."
;;     (interactive)
;;     (call-interactively
;;      (intern
;;       (ido-completing-read
;;        "M-x "
;;        (all-completions "" obarray 'commandp))))))

;; (after! org
;;   :init
;;   (require 'ox-md)

;;   (setq org-hide-leading-stars t
;; 	org-hide-emphasis-markers t
;; 	org-edit-src-content-indentation 0
;; 	org-src-tab-acts-natively t
;; 	org-confirm-babel-evaluate nil
;; 	org-support-shift-select 'always
;; 	org-src-fontify-natively t
;; 	org-fontify-whole-heading-line t
;; 	org-fontify-done-headline t
;; 	org-fontify-quote-and-verse-blocks t
;; 	org-log-done t
;; 	org-startup-with-inline-images nil
;; 	org-preview-latex-default-process 'dvisvgm
;; 	org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
;; )

;; (use-package eshell
;;   :init
;;   (require 'em-smart)
;;   (require 'em-term)
;;   :custom
;;   (eshell-scroll-to-bottom-on-input 'all)
;;   (eshell-visual-subcommands '("git" "commit" "--amend" "log" "l" "diff" "show"))
;;   (eshell-error-if-no-glob t)
;;   (eshell-where-to-jump 'begin)
;;   (eshell-review-quick-commands nil)
;;   (eshell-smart-space-goes-to-end t)
;;   (eshell-hist-ignoredups t)
;;   (eshell-save-history-on-exit t)
;;   (eshell-prefer-lisp-functions nil)
;;   (eshell-destroy-buffer-when-process-dies t)
;;   :config
;;   (let ((apps '(ssh tail htop emacs vim)))
;;     (dolist (app apps)
;;       (add-to-list 'eshell-visual-commands (format "%s" app))))

;;   (setenv "PATH" (concat "/usr/local/bin:/usr/local/sbin:" (getenv "PATH")))
;;   (getenv "PATH")
;;   (setenv "PAGER" "cat")

;;   (add-hook 'eshell-expand-input-functions #'eshell-expand-history-references)

;;   (defun eshell/find-file (file)
;;     "Find FILE using Eshell."
;;     (find-file file))

;;   (defun e/eshell-runit ()
;;     "Open Eshell using directory associated with the current buffer's file.

;;       The eshell is renamed to match that
;;       directory to make multiple eshell windows easier."
;;     (interactive)
;;     (let* ((height (/ (window-total-height) 3)))
;;       (split-window-vertically (- height))
;;       (other-window 1)
;;       (eshell "new")
;;       (insert (concat "make -ks build && make -ks run"))
;;       (eshell-send-input)))
;;   (global-set-key (kbd "C-c n") 'e/eshell-runit)

;;   (defun eshell/clear ()
;;     "Function to clear eshell buffer."
;;     (let ((eshell-buffer-maximum-lines 0))
;;       (eshell-truncate-buffer)))

;;   (defun eshell/gst (&rest args)
;;     "Git status ARGS."
;;     (magit-status (pop args) nil)
;;     (eshell/echo))   ;; The echo command suppresses output

;;   (defun eshell/find (&rest args)
;;     "Wrapper around the ‘find’ executable using ARGS."
;;     (let ((cmd (concat "find " (string-join args))))
;;       (shell-command-to-string cmd)))


;;   (defun eshell/hp (&rest args)
;;     "Emily run ARGS."
;;     (let ((cmd (concat "cero" *space* (string-join args))))
;;       (shell-command-to-string cmd)))

;;   (defun eshell/dp (&rest args)
;;     "Emily - run distro ARGS."
;;     (let ((cmd (concat "distro" *space* (string-join args))))
;;       (shell-command-to-string cmd)))

;;   (defun e/eshell-here ()
;;     "Open Eshell using directory associated with the current buffer's file.

;;       The eshell is renamed to match that
;;       directory to make multiple eshell windows easier."
;;     (interactive)
;;     (let* ((height (/ (window-total-height) 3)))
;;       (split-window-vertically (- height))
;;       (other-window 1)
;;       (eshell "new")
;;       (insert (concat "ls"))
;;       (eshell-send-input)))
;;   (global-set-key (kbd "C-c E") 'e/eshell-here)

;;   (defun e/eshell-quit-or-delete-char (arg)
;;     "ARG."
;;     (interactive "p")
;;     (if (and (eolp) (looking-back eshell-prompt-regexp))
;; 	(progn
;; 	  (eshell-life-is-too-much) ; Why not? (eshell/exit)
;; 	  (ignore-errors
;; 	    (delete-window)))
;;       (delete-forward-char arg)))

;;   (add-hook 'eshell-mode-hook
;; 	    (lambda ()
;; 	      (global-set-key (kbd "C-d") 'e/eshell-quit-or-delete-char)))

;;   (defun e/eshell-there (host)
;;     "Eshell with Tramp automatically connect to a remote system, HOST.

;;       The hostname can be either the IP address, or FQDN,
;;       and can specify the user account, as in
;;       root@blah.com. HOST can also be a complete Tramp reference."
;;     (interactive "sHost: ")

;;     (let* ((default-directory
;; 	     (cond
;; 	      ((string-match-p "^/" host) host)

;; 	      ((string-match-p (ha/eshell-host-regexp 'full) host)
;; 	       (string-match (ha/eshell-host-regexp 'full) host)
;; 	       (let* ((user1 (match-string 2 host))
;; 		      (host1 (match-string 3 host))
;; 		      (user2 (match-string 6 host))
;; 		      (host2 (match-string 7 host)))
;; 		 (if host1
;; 		     (ha/eshell-host->tramp user1 host1)
;; 		   (ha/eshell-host->tramp user2 host2))))

;; 	      (t (format "/%s:" host)))))
;;       (eshell-here)))

;;   (defun e/eshell-close ()
;;     "Close eshell."
;;     (insert "exit")
;;     (eshell-send-input)
;;     (delete-window))

;;   (defun eshell-next-prompt (n)
;;     "Move to end of Nth next prompt in the buffer.
;;   See `eshell-prompt-regexp'."
;;     (interactive "p")
;;     (re-search-forward eshell-prompt-regexp nil t n)
;;     (when eshell-highlight-prompt
;;       (while (not (get-text-property (line-beginning-position) 'read-only) )
;; 	(re-search-forward eshell-prompt-regexp nil t n)))
;;     (eshell-skip-prompt))

;;   (defun eshell-previous-prompt (n)
;;     "Move to end of Nth previous prompt in the buffer.
;;   See `eshell-prompt-regexp'."
;;     (interactive "p")
;;     (backward-char)
;;     (eshell-next-prompt (- n)))

;;   (defun eshell-insert-history ()
;;     "Displays the eshell history to select and insert back into your eshell."
;;     (interactive)
;;     (insert (ido-completing-read "Eshell history: "
;; 				 (delete-dups
;; 				  (ring-elements eshell-history-ring)))))

;;   (add-hook 'eshell-mode-hook
;; 	    (lambda ()
;; 	      (define-key eshell-mode-map (kbd "M-S-P") 'eshell-previous-prompt)
;; 	      (define-key eshell-mode-map (kbd "M-S-N") 'eshell-next-prompt)
;; 	      (define-key eshell-mode-map (kbd "M-r") 'eshell-insert-history)))

;;   (add-hook 'after-save-hook
;; 	    'executable-make-buffer-file-executable-if-script-p)

;;   (defun e/shell-here ()
;;     "Open `shell' using directory associated with the current buffer's file.

;; 	 The `shell' is renamed to match that
;; 	 directory to make multiple eshell windows easier."
;;     (interactive)
;;     (let* ((height (/ (window-total-height) 3)))
;;       (split-window-vertically (- height))
;;       (other-window 1)
;;       (shell))))


;; (use-package ansi-term
;;   :disabled
;;   :config
;;   (defun e/ansi-term-here ()
;;     "Open `ansi-term' using directory associated with the current buffer's file.

;;       The `ansi-term' is renamed to match that
;;       directory to make multiple eshell windows easier."
;;     (interactive)
;;     (let* ((height (/ (window-total-height) 3)))
;;       (split-window-vertically (- height))
;;       (other-window 1)
;;       (ansi-term "bash" "Mini-Shell")))
;;   (global-set-key (kbd "C-c T") #'e/ansi-term-here))


;; (use-package flymake
;;   :init
;;   (local-set-key (kbd "C-c b f") 'flymake-show-diagnostics-buffer)
;;   (local-set-key (kbd "M-n") 'flymake-goto-next-error)
;;   (local-set-key (kbd "M-p") 'flymake-goto-prev-error))

;; (use-package info
;;   :init
;;   :custom (Info-additional-directory-list
;; 	   (list (expand-file-name (concat *share* "/info/"))))
;;   (define-key Info-mode-map (kbd "W") 'define-word-at-point))

;; (use-package vc
;;   :init (define-key vc-prefix-map "=" 'vc-ediff)
;;   :custom (vc-dir-backend 'git vc-make-backup-files t))

;; (use-package diff
;;   :init
;;   (setq-local whitespace-style
;; 	      '(face
;; 		tabs
;; 		tab-mark
;; 		spaces
;; 		space-mark
;; 		trailing
;; 		indentation::space
;; 		indentation::tab
;; 		newline
;; 		newline-mark))
;;   (whitespace-mode 1))

;; (use-package ediff
;;   :init
;;   (setq-default ediff-diff-options "-w"
;; 		ediff-split-window-function 'split-window-horizontally
;; 		ediff-window-setup-function 'ediff-setup-windows-plain)
;;   (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

;; (use-package outline
;;   :init (outline-minor-mode)
;;   :custom (outline-regexp ";;\\ \\*"))

;; (use-package recentf
;;   :init
;;   (recentf-mode 1)
;;   (run-at-time nil (* 5 120) 'recentf-save-list)
;;   :custom
;;   (recentf-max-saved-items 1000)
;;   (recentf-max-menu-items 60) ;; disable recentf-cleanup on Emacs start, because it can cause
;;   (recentf-auto-cleanup 'never) ;; problems with remote files
;;   (recentf-auto-cleanup 600)  ;; clean up the recent files
;;   (recentf-exclude '("^/var/folders\\.*"
;; 		     "COMMIT_MSG"
;; 		     "[0-9a-f]\\{32\\}-[0-9a-f]\\{32\\}\\.org"
;; 		     "github.*txt$"
;; 		     "COMMIT_EDITMSG\\'"
;; 		     ()			".*-autoloads\\.el\\'"
;; 		     "recentf"
;; 		     ".*pang$" ".*cache$"
;; 		     "[/\\]\\.elpa/"))) ;; exclude ** from recentfiles buffer

;; ;; Add visited dired visited directories to recentf
;; (defun recentf-track-opened-file ()
;;   "Insert dired/file name just opened or written into the recent list."
;;   (let ((buff-name (or buffer-file-name
;; 		       (and (derived-mode-p 'dired-mode)
;; 			    default-directory))))
;;     (and buff-name
;; 	 (recentf-add-file buff-name)))
;;   ;; Must return nil because it is run from `write-file-functions'.
;;   nil)

;; (defun recentf-track-closed-file ()
;;   "Update the recent list when a file or dired buffer is killed.
;; That is, remove a non kept file from the recent list."
;;   (let ((buff-name (or buffer-file-name
;; 		       (and
;; 			(derived-mode-p 'dired-mode)
;; 			default-directory))))
;;     (and buff-name
;; 	 (recentf-remove-if-non-kept buff-name))))
;; (add-hook 'dired-after-readin-hook 'recentf-track-opened-file)

;; (use-package ibuffer
;;   :init
;;   (global-set-key (kbd "C-c b i") 'ibuffer)
;;   (defalias 'list-buffers 'ibuffer)) ;; make ibuffer the default buffer manager.

;; (use-package doc-view
;;   :config
;;   (setq doc-view-continuous t
;; 	doc-view-resolution 400)

;;   (add-hook 'auto-revert-mode 'doc-view)

;;   (define-key doc-view-mode-map (kbd "/") #'doc-view-reverse-colors)

;;   (defun define-doc-view-current-cache-dir ()
;;     "`doc-view-current-cache-dir' was renamed to function `doc-view--current-cache-dir' in Emacs 24.5."
;;     (or (fboundp 'doc-view-current-cache-dir)
;; 	(defalias 'doc-view-current-cache-dir 'doc-view--current-cache-dir)))
;;   (eval-after-load "doc-view" '(define-doc-view-current-cache-dir))

;;   (defun doc-view-reverse-colors ()
;;     "Inverts document colors.
;; Requires an installation of ImageMagick (\"convert\")."
;;     (interactive)
;;     ;; error out when ImageMagick is not installed
;;     (if (/= 0 (call-process-shell-command "convert -version"))
;; 	(error "Reverse colors requires ImageMagick (convert)")
;;       (when (eq major-mode 'doc-view-mode)
;; 	;; assume current doc-view internals about cache-names
;; 	(let ((file-name (expand-file-name (format "page-%d.png"
;; 						   (doc-view-current-page))
;; 					   (doc-view-current-cache-dir))))
;; 	  (call-process-shell-command
;; 	   "convert" "-negate" file-name file-name)
;; 	  (clear-image-cache)
;; 	  (doc-view-goto-page (doc-view-current-page))))))

;;   (defun doc-view-reverse-colors-all-pages ()
;;     "Inverts document colors on all pages.
;; Requires an installation of ImageMagick (\"convert\")."
;;     (interactive)
;;     ;; error out when ImageMagick is not installed
;;     (if (/= 0 (call-process-shell-command "convert -version"))
;; 	(error "Reverse colors requires ImageMagick (convert)")
;;       (when (eq major-mode 'doc-view-mode)
;; 	;; assume current doc-view internals about cache-names
;; 	(let ((orig (doc-view-current-page))
;; 	      (page nil))
;; 	  (message "Reversing video on all pages...")
;; 	  (dotimes (pnum (doc-view-last-page-number))
;; 	    (setq page (expand-file-name (format "page-%d.png" (1+ pnum))
;; 					 (doc-view-current-cache-dir)))
;; 	    (call-process-shell-command
;; 	     "convert" "-negate" page page))
;; 	  (clear-image-cache)
;; 	  (doc-view-goto-page orig)
;; 	  (message "Done reversing video!")))))


;;   (easy-menu-define e/doc-view-menu doc-view-mode-map "Menu for Doc-View Mode."
;;     '("DocView"
;;       ["Switch to a different mode" doc-view-toggle-display
;;        :help "Switch to a different mode"]
;;       ["Open Text" doc-view-open-text
;;        :help "Display the current doc's contents as text"]
;;       "--"
;;       ("Navigate Doc"
;;        ["Goto Page ..." doc-view-goto-page
;; 	:help "View the page given by PAGE"]
;;        "--"
;;        ["Scroll Down" doc-view-scroll-down-or-previous-page
;; 	:help "Scroll page down ARG lines if possible, else goto previous page"]
;;        ["Scroll Up" doc-view-scroll-up-or-next-page
;; 	:help "Scroll page up ARG lines if possible, else goto next page"]
;;        "--"
;;        ["Next Line" doc-view-next-line-or-next-page
;; 	:help "Scroll upward by ARG lines if possible, else goto next page"]
;;        ["Previous Line" doc-view-previous-line-or-previous-page
;; 	:help "Scroll downward by ARG lines if possible, else goto previous page"]
;;        ("Customize"
;; 	["Continuous Off"
;; 	 (setq doc-view-continuous nil)
;; 	 :help "Stay put in the current page, when moving past first/last line"
;; 	 :style radio :selected
;; 	 (eq doc-view-continuous nil)]
;; 	["Continuous On"
;; 	 (setq doc-view-continuous t)
;; 	 :help "Goto to the previous/next page, when moving past first/last line"
;; 	 :style radio :selected
;; 	 (eq doc-view-continuous t)]
;; 	"---"
;; 	["Save as Default"
;; 	 (customize-save-variable 'doc-view-continuous doc-view-continuous)
;; 	 t])
;;        "--"
;;        ["Next Page" doc-view-next-page :help "Browse ARG pages forward"]
;;        ["Previous Page" doc-view-previous-page :help "Browse ARG pages backward"]
;;        "--"
;;        ["First Page" doc-view-first-page :help "View the first page"]
;;        ["Last Page" doc-view-last-page :help "View the last page"])
;;       "--"
;;       ("Adjust Display"
;;        ["Enlarge" doc-view-enlarge :help "Enlarge the document by FACTOR"]
;;        ["Shrink" doc-view-shrink :help "Shrink the document"]
;;        "--"
;;        ["Fit Width To Window" doc-view-fit-width-to-window
;; 	:help "Fit the image width to the window width"]
;;        ["Fit Height To Window" doc-view-fit-height-to-window
;; 	:help "Fit the image height to the window height"]
;;        "--"
;;        ["Fit Page To Window" doc-view-fit-page-to-window
;; 	:help "Fit the image to the window"]
;;        "--"
;;        ["Set Slice From Bounding Box" doc-view-set-slice-from-bounding-box
;; 	:help "Set the slice from the document's BoundingBox information"]
;;        ["Set Slice Using Mouse" doc-view-set-slice-using-mouse
;; 	:help "Set the slice of the images that should be displayed"]
;;        ["Set Slice" doc-view-set-slice
;; 	:help "Set the slice of the images that should be displayed"]
;;        ["Reset Slice" doc-view-reset-slice
;; 	:help "Reset the current slice"])
;;       ("Search"
;;        ["New Search ..."
;; 	(doc-view-search t)
;; 	:help
;; 	"Jump to the next match or initiate a new search if NEW-QUERY is given"]
;;        "--"
;;        ["Search" doc-view-search
;; 	:help
;; 	"Jump to the next match or initiate a new search if NEW-QUERY is given"]
;;        ["Backward" doc-view-search-backward
;; 	:help "Call `doc-view-search' for backward search"]
;;        "--"
;;        ["Show Tooltip" doc-view-show-tooltip
;; 	:help nil])
;;       ("Maintain"
;;        ["Reconvert Doc" doc-view-reconvert-doc
;; 	:help "Reconvert the current document"]
;;        "--"
;;        ["Clear Cache" doc-view-clear-cache
;; 	:help "Delete the whole cache (`doc-view-cache-directory')"]
;;        ["Dired Cache" doc-view-dired-cache
;; 	:help "Open `dired' in `doc-view-cache-directory'"]
;;        "--"
;;        ["Revert Buffer" doc-view-revert-buffer
;; 	:help "Like `revert-buffer', but preserves the buffer's current modes"]
;;        "--"
;;        ["Kill Proc" doc-view-kill-proc
;; 	:help "Kill the current converter process(es)"]
;;        ["Kill Proc And Buffer" doc-view-kill-proc-and-buffer
;; 	:help "Kill the current buffer"])
;;       "--"
;;       ["Customize"
;;        (customize-group 'doc-view)]))
;;   (easy-menu-define e/doc-view-minor-mode-menu doc-view-minor-mode-map
;;     "Menu for Doc-View Minor Mode."
;;     '("DocView*"
;;       ["Display in DocView Mode" doc-view-toggle-display
;;        :help "View"]
;;       ["Exit DocView Mode" doc-view-minor-mode])))

;; (use-package flyspell
;;   :init
;;   (if (executable-find "aspell")
;;       (progn
;; 	(setq ispell-program-name "aspell"
;; 	      ispell-extra-args '("--sug-mode=ultra")))
;;     (setq ispell-program-name "ispell")))

;; (use-package icomplete
;;   :init
;;   (icomplete-mode 1))

;; (use-package abbrev
;;   :config
;;   (defun xah-global-abbrev-position-cursor (&optional @pos)
;;     "Move cursor back to ^ if exist, else put at end.
;; Return true if found, else false. Version 2016-10-24"
;;     (interactive)
;;     (let (($found-p (search-backward "^" (if @pos
;; 					     @pos
;; 					   (max (point-min) (- (point) 100)))
;; 				     t)))
;;       (when $found-p (delete-char 1))
;;       $found-p))

;;   (advice-add 'expand-abbrev :after (lambda () (xah-global-abbrev-position-cursor)))
;;   ;; mimics yasnippet point move to  !
;;   (defadvice expand-abbrev (after move-to-point activate)
;;     (xah-global-abbrev-position-cursor)))

;; (use-package saveplace
;;   :init
;;   (save-place-mode 1)
;;   (setq-default save-place t)
;;   :custom
;;   (save-place-file (expand-file-name "etc/places" user-emacs-directory))
;;   (backup-directory-alist `(("." . ,(concat user-emacs-directory
;; 					    "etc/backups")))))

;; (use-package goto-addr
;;   :config
;;   (add-hook 'prog-mode-hook 'goto-address-mode)
;;   (add-hook 'text-mode-hook 'goto-address-mode))

;; (use-package hippie-exp
;;   :init
;;   (global-set-key (kbd "<C-tab>") 'hippie-expand)
;;   :custom
;;   (hippie-expand-try-functions-list
;;    '(yas-hippie-try-expand
;;      ;; Try to expand word "dynamically", searching the current buffer.
;;      try-expand-dabbrev
;;      ;; Try to expand word "dynamically", searching all other buffers.
;;      try-expand-dabbrev-all-buffers
;;      ;; Try to expand word "dynamically", searching the kill ring.
;;      try-expand-dabbrev-from-kill
;;      ;; Try to complete text as a file name, as many characters as unique.
;;      try-complete-file-name-partially
;;      ;; Try to complete text as a file name.
;;      try-complete-file-name
;;      ;; Try to complete as an Emacs Lisp symbol, as many characters as unique.
;;      try-complete-lisp-symbol-partially
;;      ;; Try to complete word as an Emacs Lisp symbol.
;;      try-complete-lisp-symbol
;;      ;; Try to expand word before point according to all abbrev tables.
;;      try-expand-all-abbrevs
;;      ;; Try to complete the current line to an entire line in the buffer.
;;      try-expand-list
;;      ;; Try to complete the current line to an entire line in the buffer.
;;      try-expand-line)))

;; (use-package whitespace
;;   :init
;;   (add-hook 'before-save-hook 'whitespace-cleanup)
;;   (add-hook 'before-save-hook 'delete-trailing-whitespace)
;;   (add-hook 'prog-mode-hook
;; 	    (lambda () (interactive) (setq show-trailing-whitespace 1)))
;;   (add-hook 'org-mode-hook
;; 	    (lambda () (interactive) (setq show-trailing-whitespace 1)))

;;   ;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;;   (setq show-trailing-whitespace t
;; 	whitespace-style '(face lines-tail)))

;; (use-package savehist
;;   :init
;;   (savehist-mode t)
;;   (setq savehist-save-minibuffer-history t
;; 	savehist-autosave-interval nil
;; 	;; savehist-file (expand-file-name "etc/savehist" user-emacs-directory)
;; 	savehist-save-minibuffer-history 1
;; 	savehist-additional-variables '(Info-history-list)
;; 	savehist-additional-variables '(kill-ring search-ring regexp-search-ring)))


;; (use-package eww
;;   :config
;;   (define-key eww-mode-map (kbd "W") 'define-word-at-point))

;; (use-package windmove
;;   :custom (windmove-wrap-around t))

;; (use-package prettify
;;   :disabled
;;   :config
;;   (global-prettify-symbols-mode 1)
;;   (setq prettify-symbols-alist '(("lambda" . 955)))
;;   (defun e/add-pretty-lambda ()
;;     "Lisp symbols as pretty Unicode symbols."
;;     (setq prettify-symbols-alist
;; 	  '(("lambda" . 955)  ;; λ
;; 	    ("->" . 8594)     ;; →
;; 	    ("=>" . 8658)     ;; ⇒
;; 	    ("map" . 8614)))) ;; ↦
;;   (add-hook 'scheme-mode-hook 'e/add-pretty-lambda)
;;   (add-hook 'clojure-mode-hook 'e/add-pretty-lambda)
;;   (add-hook 'haskell-mode-hook 'e/add-pretty-lambda))

;; (use-package winner
;;   :init (winner-mode 1))

;; (use-package midnight
;;   :init
;;   (midnight-delay-set 'midnight-delay "4:30am")
;;   :custom ( midnight-period 5000))

;; (use-package uniquify
;;   :custom
;;   (uniquify-buffer-name-style 'forward)
;;   (uniquify-after-kill-buffer-p t)
;;   (uniquify-separator "/")
;;   (uniquify-ignore-buffers-re "^\\*"))

;; (use-package isearch
;;   :config
;;   (defun e/isearch-exit-other-end ()
;;     "Exit isearch, at the opposite end of the string."
;;     (interactive)
;;     (isearch-exit)
;;     (goto-char isearch-other-end))
;;   (define-key isearch-mode-map [(control return)] #'isearch-exit-other-end))

;; (use-package eldoc
;;   :custom
;;   (eldoc-idle-delay 0.1)
;;   (eldoc-echo-area-use-multiline-p nil))

;; (use-package paren
;;   :init (show-paren-mode 1))

(provide 'init-builtin)
;;; init-builtin.el ends here
