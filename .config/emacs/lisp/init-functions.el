;;; -*- lexical-binding: t;

;; ============================
;; * CUSTOM FUNCTIONS
;; ============================

;; ===============
;; * SYSTEM SOFTWARE

(defun e/return-exec (apps)
  "Return first executable in APPS found.
TODO: return if running"
  (require 'cl-lib)
  (cl-dolist (app apps)
    (when (executable-find app)
      (cl-return app))))

(defcustom *ffmpeg-p* (when (e/return-exec '("ffmpeg")) t)
  "FFMPEG - confirm if it installed."
  :type 'boolean)

;; *** MEDIA

(defun e/get-video (url)
  "Download Video w/ URL - GPL-3.0."
  (interactive)
  (alert "Getting Video.")
  (start-process "GET-VIDEO" "GET-VIDEO" "cejo" "media" "get" url))
(global-set-key (kbd "C-c f V") #'(lambda () (interactive) (e/get-video (current-kill 0 t))))

(defun e/get-audio (url)
  "Download Audio w/ URL - GPL-3.0!"
  (interactive)
  (alert "Getting Audio.")
  (start-process "GET-AUDIO" "GET-AUDIO" "cejo" "media" "get" url "vorbis"))
(global-set-key (kbd "C-c f A") #'(lambda () (interactive) (e/get-audio (current-kill 0 t))))

(defun e/play-video (url)
  "Call Video Player with online video's URL on clipboard!"
  (interactive)
  (alert "Playing Video.")
  (start-process "PLAY-VIDEO" "PLAY-VIDEO" "cejo" "media" "play" url))
(global-set-key (kbd "C-c f P") #'(lambda () (interactive) (e/play-video (current-kill 0 t))))

;; * FFMPEG - ffmpeg features using Dired

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
      (alert "FFMPEG IS NOT INSTALLED"))))
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
      (alert "FFMPEG IS NOT INSTALLED"))))
(global-set-key (kbd "C-c C-f f") 'e/dired-ffmpeg-boost-volume)

(defun e/send-file ()
  "Send file to mobile phone."
  (interactive)
  (let ((file (dired-get-filename nil t)))
    (start-process "SENDING" "SENDING" "cero" "ops" "send" file)))
(global-set-key (kbd "C-c S") 'e/send-file)

(defun e/sysinfo ()
  "Apresenta informacoes do sistema."
  (interactive)
  (let ((info (string-trim (shell-command-to-string "sysinfo"))))
    (alert info)))

;; ========================================
;; * MISC

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
       ((string-equal search-type "s")
	(browse-url (concat "https://stackoverflow.com/search?q=" term)))
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

(defun e/filepath-to-clipboard ()
  (interactive)
  (kill-new (buffer-file-name)))
(global-set-key (kbd "C-c b q") 'e/filepath-to-clipboard)

(defun e/yt-feed()
  (interactive)
  (let* ((base "https://www.youtube.com/feeds/videos.xml?channel_id=")
	(url (current-kill 0 t))
	(id (car (last (split-string url "/"))))
	(name (read-from-minibuffer "Channel name: ")))
    (newline-and-indent)
    (previous-line 1)
    (insert (concat "\"" base id "\" ;; ") name)))

(provide 'init-functions)
