;;; -*- lexical-binding: t;

(use-package pdf-tools
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

(use-package nov
  :defer t
  :init (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :config
  (add-hook 'nov-mode-hook 'olivetti-mode)
  (define-key nov-mode-map (kbd "w") 'define-word-at-point))

(provide 'init-repository-readers)
