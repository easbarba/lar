;;; package --- Builtin Packages Configurations -*- lexical-binding: t;

;;; Commentary:
;;; Code:

;; =========================
;; * BUILTIN PACKAGES
;; =========================

(after! erc
  (setq-local scroll-margin 1)
  (setq erc-nick user-login-name
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
                                       "#ruby"
                                       "#guile"
                                       "#scheme"
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
				             (image-next-file 1)
				             (image-transform-fit-to-width)))
            (local-set-key (kbd "<left>") (lambda ()
				            (interactive)
				            (image-previous-file 1)
				            (image-transform-fit-to-width)))))


(provide 'init-builtin)
;;; init-builtin.el ends here
