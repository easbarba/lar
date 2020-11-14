;;; -*- lexical-binding: t;

;; ============================
;; * GLOBAL VARIABLES

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

(defcustom *musica* (concat *lar* "/Music")
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

(provide 'init-my)
