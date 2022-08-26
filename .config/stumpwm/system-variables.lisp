;; ============================================================
;; SYSTEM APPS

(defun eas/commandv (exec)
  "Return EXEC path if found."
  (zerop
   (nth-value 2 (uiop:run-program
		 (concatenate 'string "command -v" " " exec)
		 :force-shell t :ignore-error-status t))))

(defparameter *editor* (cond ((eas/commandv "emacs") "emacs")
                             ((eas/commandv "code") "code")
                             ((eas/commandv "kate") "kate"))
  "Default Editor.")

(defparameter *browser* (cond ((eas/commandv "chromium") "chromium")
                              ((eas/commandv "firefox") "firefox")
                              ((eas/commandv "nyxt") "nyxt")
                              ((eas/commandv "google-chrome") "google-chrome"))
  "Default Browser.")

(defparameter *terminal* (cond ((eas/commandv "alacritty") "alacritty")
                               ((eas/commandv "Gnome-terminal") "Gnome-terminal")
			                   ((eas/commandv "st") "st"))
  "Default terminal.")

(defparameter *locker* (cond ((eas/commandv "slock") "slock")
			     ((eas/commandv "i3lock") "i3lock"))
  "Default System Locker.")
