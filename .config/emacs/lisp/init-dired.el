;;; -*- lexical-binding: t;

;; =========================
;; * DIRED
;; =========================

(add-hook
 'dired-mode-hook
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

(provide 'init-dired)
