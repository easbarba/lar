;;; -*- lexical-binding: t;

;; ============================
;; * MISC WEB SNIPPETS
;; ============================

(defun e/screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "Emacs" nil ".svg"))
	 (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

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


(defun e/change-theme (&rest args)
  "Like `load-theme', but disable all themes before loading the new one, ARGS."
  ;; The `interactive' magic is for creating a future-proof passthrough.
  (interactive (advice-eval-interactive-spec
		(cadr (interactive-form #'load-theme))))
  (mapc #'disable-theme custom-enabled-themes)
  (apply (if (called-interactively-p 'any) #'funcall-interactively #'funcall)
	 #'load-theme args))
(provide 'init-misc)
