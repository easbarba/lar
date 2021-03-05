;;; package --- global variables -*- lexical-binding: t;

;;; Commentary:
;;; Code:

;; =======================
;; CUSTOM GLOBAL VARIABLES
;; =======================

(defconst *site-lisp* (concat user-emacs-directory "site-lisp")
  "Emacs site-lisp folder.")

(defconst *home* (getenv "HOME")
  "$HOME folder location.")

(defconst *xdg-config* (concat *home* "/.config")
  "XDG Config folder.")

(defconst *local* (concat *home* "/.local")
  "$HOME/.local folder.")

(defconst *share* (concat *local* "/share")
  "XDG local share folder.")

(defconst *download* (concat *home* "/Downloads")
  "$HOME/Download folder.")

(defgroup *vars* nil
  "CUSTOM variables set by user"
  :group 'extensions
  :group 'convenience)

(defcustom *videos* (concat *home* "/Videos")
  "Folder to save videos."
  :type 'string
  :group '*vars*)

(defcustom *music* (concat *home* "/Music")
  "Folder to save songs."
  :type 'string
  :group '*vars*)

(defcustom *space* "?\s"
  "Folder to save songs."
  :type 'string
  :group '*vars*)

(defcustom *theme* 'tango
  "Emacs default theme."
  :type 'string
  :group 'my)

(provide 'init-variables)
;;; init-variables.el ends here
