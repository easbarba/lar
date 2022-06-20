;; ============================================================
;; FOLDERS LOCATIONS

(defparameter *lar*
  (directory-namestring (user-homedir-pathname))
  "Home Folder.")
(defparameter *music*
  (concatenate 'string *lar* "Music")
  "Tunes Folder.")
(defparameter *books*
  (concatenate 'string *lar* "Books")
  "Books Folder.")
(defparameter *pictures*
  (concatenate 'string *lar* "Pictures")
  "Fotografias Folder.")
(defparameter *downloads*
  (concatenate 'string *lar* "Downloads")
  "Downloads Folder.")
(defparameter *local-bin*
  (concatenate 'string *lar* ".local/bin")
  "Local bin Folder.")
