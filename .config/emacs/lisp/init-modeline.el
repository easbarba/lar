;;; -*- lexical-binding: t;

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

(provide 'init-modeline)
