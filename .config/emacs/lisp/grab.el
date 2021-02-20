;;; grab.el --- Simple get Emacs Packages from public repositories or download file. -*- lexical-binding: t; -*-

;;; License

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Information

;; Author: Alexander Barbosa - easbarbosa@pm.me
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: download, git, elisp, library, minor-mode
;; URL: https://.com/eabarbosa/grab

;;; Commentary:

;;; Usage:

;;
;;

;;; Code:

(defgroup grab nil
  "Simple get Emacs Packages from public repositories or download file."
  :group 'applications
  :link '(url-link "https://github.com/elxbarbosa/grab")
  :version "27.1")

(defcustom grab-repo-list '()
  "List of repositories to get."
  :group 'grab
  :type 'list)

(defcustom grab-download-dir (concat user-emacs-directory "site-lisp")
  "Folder where elisp libraries reside."
  :group 'grab
  :type 'string)

(defvar grab--known-repo  '("savannah.gnu.org" "github.com"
			       "gitlab.com" )
  "List of known repositories."
  :type 'string)

(defvar grab--known-url-protocols '("https://" "https://www."
				       "http://" "http://www."
				       "www.")
  "Return repo-name with no protocol prefix."
  :type 'string)

(defvar grab-lisp-folders-to-path '()
  "Custom Elisp libraries to Emacs load Path."
  :type 'list)

(defun grab-repo-domain (url)
  "."
  (dolist (x grab--known-repo x)
    (when (nth 1 (split-string url x))
      (setq repo-domain x)))
  repo-domain)

(defun grab-remove-protocol (url)
  "Remove URL protocol off."
  (let ((cleaned-url ""))
    (dolist (x grab--known-url-protocols cleaned-url)
      (when (string-prefix-p x url)
        (setq cleaned-url (string-remove-prefix x url))))))

(defun grab-repo-name (url)
  "Parse GitHub's URL and return repository name."
  (let ((repo-name (nth 2 (split-string (grab-remove-protocol url)  "/"))))
    (get-add-repo-to-list repo-name)
    repo-name))

(defun grab-add-repo-to-list (repo-name)
  "Add REPO-NAME  to `grab-lisp-folders-to-path'."
  (unless (member repo-name grab-lisp-folders-to-path)
    (setq grab-lisp-folders-to-path
          (progn
            (when (member nil grab-lisp-folders-to-path)
              (remove nil grab-lisp-folders-to-path))
            (append grab-lisp-folders-to-path
                    (list repo-name))))))


(defun grab-clone-pull (url pull)
  "Clone/PULL URL.
Depending repository exist pull update, else clone it."
  (if pull
      (start-process "PULLING" "PULLING" "git" "pull" url)
    (start-process "CLONING" "CLONING" "git" "clone" url)))

(defun grab-file ()
  "Download file."
  (start-process "GETTING" "GETTING" getter url))

(defun grab-pull-p (package)
  "."
  "Git directory exist? Git Pull it."
  (when (file-exists-p (concat dir-repo-name "/.git"))
    t))

(defun grab-dir-exist-p (dir-repo-name)
  "DIR-REPO-NAME exist? if not make it and return t."
  (if (file-directory-p directory)
      directory
    (progn (mkdir directory) directory)))

(defun grab-dir-repo-name (directory-repository-name)
  "."
  (let ((default-directory grab-download-dir))
    (expand-file-name
     (concat default-directory
             "/"
             (grab-repo-name directory-repository-name)))))

(defun grab-repo-do-list (repositories)
  "Get REPOSITORIES listed in repo list or pull updates."
  (let ((default-directory grab-download-dir))
    (dolist (x grab-repo-list)
      (let ((current-repo-path (grab-dir-repo-name x)))
        ;; Pull/Clone Repository
        (if (file-directory-p current-repo-path)
            (let ((default-directory current-repo-path))
              (grab-clone-pull x t)) ;; Pull
          (grab-clone-pull x nil)))))) ;; Clone

;; ---------------
;; Add /lisp folder to Emacs Path
(let ((default-directory (concat user-emacs-directory "site-lisp")))
  (normal-top-level-add-subdirs-to-load-path))


;; TODO: interactive choose if file downloader or library grabber!
(defun grab (&optional arg)
  "File Downloader(C-u) and Emacs libraries Getter, URL. ARG.

`Repositories': GNU Savannah, GitLab, GitHub.
`DIRECTORY': set grab-download-dir.
`TODO:' Interactive ask for folder
GPL-3.0!"
  (interactive)

  ;; Make ~/emacs.d/lisp folder
  (unless (file-directory-p grab-download-dir)
    (mkdir grab-download-dir))

  ;; ...and there was light...
  (if (not (equal current-prefix-arg nil))
      (let ((clipboard-url (current-kill 0 t))
            (default-directory *download*))
        (let ((download-buffer (url-retrieve-synchronously clipboard-url)))
          (save-excursion
            (set-buffer download-buffer)
            (goto-char (point-min))
            (re-search-forward "^$" nil 'move)
            (forward-char)
            (delete-region (point-min) (point))
            (write-file (concat default-directory "/"
                                (car
                                 (last
                                  (split-string clipboard-url "/" t))))))))
    (progn
      (message "No list provided. Using list provided by get-repo-list!" )
      (grab-repo-do-list grab-repo-list))))

;; ------------------------------
;; TODO:
(defun grab-autoremove ()
  "Remove packages not listed in get-repo-list.")

(defun grab-reinstall ()
  "Reinstall listed packages.")

(provide 'grab)
;;; grab.el ends here
