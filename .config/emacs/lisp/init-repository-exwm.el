;;; init-repository-exwm.el ends here
;;; package --- base -*- lexical-binding: t;

;;; Commentary:
;;; Code:


;;; Description:

;; ===================================
;; Emacs X11 Windows Manager
;; ===================================

(when (display-graphic-p)
  (use-package exwm
    :config
    (require 'cl-generic)
    (require 'cl-lib)
    (require 'xelb)
    (require 'exwm)

    (require 'exwm-config)
    (exwm-config-ido)
    (exwm-config-misc)

    (require 'exwm-systemtray)
    (exwm-systemtray-enable)

    (exwm-enable)
    :custom
    (exwm-workspace-number 6)
    (exwm-workspace-display-echo-area-timeout 2)
    (exwm-workspace-show-all-buffers t)
    (use-dialog-box nil))

  (defvar are-workspaces-loaded 0
    "are workspaces loaded? start out with value of 0.")

  (defvar exwm-toggle-workspace 0
    "previously selected workspace. used with `exwm-jump-to-last-exwm'.")

  (add-hook 'exwm-update-class-hook
	    (lambda ()
	      (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
			  (string= "gimp" exwm-instance-name))
		(exwm-workspace-rename-buffer exwm-class-name))))

  (add-hook 'exwm-update-title-hook
	    (lambda ()
	      (when (or (not exwm-instance-name)
			(string-prefix-p "sun-awt-X11-" exwm-instance-name)
			(string= "gimp" exwm-instance-name))
		(exwm-workspace-rename-buffer exwm-title))))

  (defvar exwm-workspace-switch-wrap t
    "Whether `exwm-workspace-next' and `exwm-workspace-prev' should wrap.")

  (defun exwm-workspace-next ()
    "Switch to next exwm-workspaceective (to the right)."
    (interactive)
    (let* ((only-workspace? (equal exwm-workspace-number 1))
	   (overflow? (= exwm-workspace-current-index
			 (1- exwm-workspace-number))))
      (cond
       (only-workspace? nil)
       (overflow?
	(when exwm-workspace-switch-wrap
	  (exwm-workspace-switch 0)))
       (t (exwm-workspace-switch  (1+ exwm-workspace-current-index))))))

  (defun exwm-workspace-prev ()
    "Switch to next exwm-workspaceective (to the right)."
    (interactive)
    (let* ((only-workspace? (equal exwm-workspace-number 1))
	   (overflow? (= exwm-workspace-current-index 0)))
      (cond
       (only-workspace? nil)
       (overflow?
	(when exwm-workspace-switch-wrap
	  (exwm-workspace-switch (1- exwm-workspace-number))))
       (t (exwm-workspace-switch  (1- exwm-workspace-current-index))))))

  (defun exwm-jump-to-last-exwm ()
    (interactive)
    (exwm-workspace-switch exwm-toggle-workspace))

  (defadvice exwm-workspace-switch (before save-toggle-workspace activate)
    (setq exwm-toggle-workspace exwm-workspace-current-index))

  ;; Check for start-up errors. See ~/.profile.
  (let ((error-logs (directory-files "~" t "errors.*log$")))
    (when error-logs
      (warn "Error during system startup.  See %s." (mapconcat 'identity error-logs ", "))
      (when (daemonp)
	;; Non-daemon Emacs already brings up the *Warning* buffer.
	(setq initial-buffer-choice
	      (lambda () (get-buffer "*Warnings*"))))))


  (defun e/exwm-start-in-char-mode ()
    (when (string-prefix-p (or "emacs" "next") exwm-instance-name)
      (exwm-input-release-keyboard (exwm--buffer->id (window-buffer)))))
  (add-hook 'exwm-manage-finish-hook 'e/exwm-start-in-char-mode)

  (defun e/exwm-get-workspace-number ()
    (interactive)
    (let((workspace (when (frame-parameter (selected-frame) 'exwm-active)
		      (condition-case nil
			  (number-to-string
			   (+ 1
			      (exwm-workspace--position
			       (exwm-workspace--workspace-from-frame-or-index
				(selected-frame)))))
			(error "~")))))
      (message " %s)" workspace)))


  (setq exwm-manage-configurations
	'(((equal exwm-class-name "st-256color") workspace 2)
	  ((equal exwm-class-name "st-256color") fullscreen t)
	  ((equal exwm-class-name "Konsole") workspace 2)
	  ((equal exwm-class-name "mpv") workspace 3)
	  ((equal exwm-class-name "Firefox") workspace 4)
	  ((equal exwm-class-name "Firefox") char-mode t)
	  ((equal exwm-class-name "Firefox-esr") workspace 4)
	  ((equal exwm-class-name "Chromium-browser") workspace 4)
	  ((equal exwm-class-name "Chromium") workspace 4)
	  ((equal exwm-class-name "Telegram") workspace 4)
	  ((equal exwm-class-name "TelegramDesktop") workspace 4)
	  ((equal exwm-class-name "Steam") workspace 5)))

  (defun e/exwm-switch-to-worskpace (n)
    "Switch to N worskpace."
    (interactive)
    (let ((names '((1 . "Codando")
		   (2 . "Leitura")
		   (3 . "Term")
		   (4 . "Midias")
		   (5 . "WWW")
		   (6 . "Misc"))))

      (exwm-workspace-switch (- n 1))
      (message "%s %s" (e/exwm-get-workspace-number) (cdr (assoc n names)))))

  (defun e/exwm-input-line-mode ()
    "Set exwm window to line-mode and show mode line"
    (call-interactively #'exwm-input-grab-keyboard)
    (exwm-layout-show-mode-line))

  (defun e/exwm-input-char-mode ()
    "Set exwm window to char-mode and hide mode line"
    (call-interactively #'exwm-input-release-keyboard)
    (exwm-layout-hide-mode-line))

  (defun e/exwm-input-toggle-mode ()
    "Toggle between line- and char-mode"
    (interactive)
    (with-current-buffer (window-buffer)
      (when (eq major-mode 'exwm-mode)
	(if (equal (second (second mode-line-process)) "line")
	    (e/exwm-input-char-mode)
	  (e/exwm-input-line-mode)))))

  ;; ==================================================
  ;; EXWM - BINDS
  ;; ==================================================

  ;; Switch to Workspaces N
  (exwm-input-set-key (kbd "s-1") (lambda () (interactive)
				    (e/exwm-switch-to-worskpace 1)))
  (exwm-input-set-key (kbd "s-2") (lambda () (interactive)
				    (e/exwm-switch-to-worskpace 2)))
  (exwm-input-set-key (kbd "s-3") (lambda () (interactive)
				    (e/exwm-switch-to-worskpace 3)))
  (exwm-input-set-key (kbd "s-4") (lambda () (interactive)
				    (e/exwm-switch-to-worskpace 4)))
  (exwm-input-set-key (kbd "s-5") (lambda () (interactive)
				    (e/exwm-switch-to-worskpace 5)))
  (exwm-input-set-key (kbd "s-6") (lambda () (interactive)
				    (e/exwm-switch-to-worskpace 6)))

  ;; Last workspace visited
  (exwm-input-set-key (kbd "<s-tab>") (lambda ()
					(interactive)
					(exwm-jump-to-last-exwm)
					(e/exwm-get-workspace-number)))

  ;; Switch Workspaces
  (exwm-input-set-key (kbd "s-n") 'exwm-workspace-next)
  (exwm-input-set-key (kbd "s-p") 'exwm-workspace-prev)

  ;; Move app to x workspace
  (exwm-input-set-key (kbd "s-m") 'exwm-workspace-move-window)

  ;; Workspace Layout
  (exwm-input-set-key (kbd "s-F") 'exwm-layout-toggle-fullscreen)
  (exwm-input-set-key (kbd "s-T") 'exwm-floating-toggle-floating)

  (exwm-input-set-key (kbd "s-x") 'e/sistema-inicia-comando)

  ;; WINDOWS
  ;; jump to buffers with s-[swad]
  (exwm-input-set-key (kbd "s-a") 'windmove-left)
  (exwm-input-set-key (kbd "s-s") 'windmove-down)
  (exwm-input-set-key (kbd "s-w") 'windmove-up)
  (exwm-input-set-key (kbd "s-d") 'windmove-right)

  (exwm-input-set-key (kbd "s-[") 'shrink-window-horizontally)
  (exwm-input-set-key (kbd "s-{") 'shrink-window)
  (exwm-input-set-key (kbd "s-]") 'enlarge-window-horizontally)
  (exwm-input-set-key (kbd "s-}") 'enlarge-window)

  ;; [TERM]
  (exwm-input-set-key (kbd "s-t") '(lambda () (interactive) (ansi-term "/bin/bash")))
  (exwm-input-set-key (kbd "s-e") 'eshell)

  ;; [BUFFERS]
  (exwm-input-set-key (kbd "s-b") 'ido-switch-buffer)
  (exwm-input-set-key (kbd "s-f") 'ido-find-file)
  (exwm-input-set-key (kbd "s-i") 'ibuffer)
  (exwm-input-set-key (kbd "s-r") 'e/recentf-ido-find-file)
  (exwm-input-set-key (kbd "s-K") (lambda () (interactive) (kill-buffer)))

  ;; ==============================
  ;; SYSTEM SOFTWARE
  ;; ==============================

  ;; AUTOSTART APPLICATIONS
  (defun e/autostart-scripts ()
    "Autostart Scripts in XDG-CONFIG-HOME/autostart-scripts"
    (interactive)
    (let* ((scripts-dir (f-join *xdg-config* "autostart-scripts"))
	   (scripts (directory-files-recursively scripts-dir "")))
      (dolist (script scripts)
	(alert (format "Starting: %s" script))
	(start-process "AUTOSTART" "AUTOSTART" (f-join scripts-dir script)))))

  (defconst e/autostart-apps '(udiskie unclutter ibus-daemon dunst
			       nm-applet blueman-applet mate-power-manager
			       diodon pasystray
			       st firefox)
    "List of Apps to auto start in EXWM")

  (defun e/autostart ()
    "Autostart Desktop Applications in XDG-CONFIG-HOME/autostart/*.desktop"
    (interactive)
    (dolist (app e/autostart-apps)
	(message "Starting: %s" app)
	(start-process "AUTOSTART" "AUTOSTART" (format "%s" app))))

  (defun e/sistema-brilho-cima ()
    "Aumenta brilho do sistema."
    (interactive)
    (alert "brilho +")
    (start-process "BRILHO-CIMA" nil "cejo" "ops" "brightness" "up"))

  (defun e/sistema-brilho-baixo ()
    "Aumenta brilho do sistema."
    (interactive)
    (alert "brilho -")
    (start-process "BRILHO-BAIXO" nil "cejo" "ops" "brightness" "down"))

  (defun e/sistema-volume-cima ()
    "Aumenta volume do sistema."
    (interactive)
    (alert (format "Volume: %s" (e/exwm-statusbar-volume)))
    (start-process "VOLUME-CIMA" nil "cejo" "ops" "volume" "up"))

  (defun e/sistema-volume-baixo ()
    "Diminui volume do sistema."
    (interactive)
    (alert (format "Volume: %s" (e/exwm-statusbar-volume)))
    (start-process "VOLUME-BAIXO" nil "cejo" "ops" "volume" "down"))

  (defun e/sistema-volume-alternar ()
    "Mute System audio"
    (interactive)
    (alert (format "Volume alternado!" (e/exwm-statusbar-volume)))
    (start-process "VOLUME-ALTERNAR" nil "cejo" "ops" "volume" "toggle"))

  (defun e/sistema-volume-reload ()
    "Reload System volume backend."
    (interactive)
    (when (executable-find "pulseaudio")
      (start-process "PULSEAUDIO-RELOAD" nil "pulseaudio" "-k" )
      (start-process "PULSEAUDIO-RELOAD" nil "pulseaudio" "-D" )
      (alert "Pulseaudio reloaded")))

  (defun e/sistema-toca ()
    "Play Video"
    (interactive)
    (alert "Opening video")
    (start-process "REBOOT" nil "cejo" "media" "play"))

  (defun e/sistema-pegavideo ()
    "Get Video"
    (interactive)
    (alert "Getting video")
    (start-process "PEGAVIDEO" nil "cejo" "media" "get"))

  (defun e/sistema-pegaaudio ()
    "Pegando Auido"
    (interactive)
    (alert "Pegando audio")
    (start-process "PEGAAUDIO" nil "cejo" "media" "get" "vorbis"))

  (defun e/sistema-reiniciar ()
    "Reiniciando Sistema."
    (interactive)
    (alert "Reiniciando em alguns segundos.")
    (sleep-for 10)
    (start-process "REINICIA" nil "systemctl" "reboot"))

  (defun e/sistema-desliga ()
    "Desligando Sistema."
    (interactive)
    (alert "Desligando em alguns segundos.")
    (sleep-for 10)
    (start-process "DESLIGAR" nil "systemctl" "poweroff"))

  (defun e/sistema-suspender ()
    "Poweroff System"
    (interactive)
    (alert "Suspendendo em alguns segundos.")
    (sleep-for 5)
    (start-process "SUSPENDE" nil "systemctl" "hibernate"))

  (defun e/sistema-sair ()
    "Saindo da sessao do usuario."
    (interactive)
    (alert "Saindo da sessao em alguns segundos.")
    (sleep-for 5)
    (start-process "SAINDO" nil "loginctl" "terminate-user" user-full-name))

  (defun e/sistema-fechar ()
    "Fechando sessao."
    (interactive)
    (start-process "TERMINAL" nil (e/return-exec '("slock" "i3lock"))))

  (defun e/sistema-terminal ()
    "Open Terminal Console"
    (interactive)
    (alert "Opening terminal.")
    (e/exwm-switch-to-worskpace 3)
    (start-process "TERMINAL" nil (e/return-exec '("st" "mate-terminal" "konsole"))))

  (defun e/sistema-browser ()
    "Open Web Browser"
    (interactive)
    (alert "Opening browser.")
    (e/exwm-switch-to-worskpace 5)
    (cl-dolist (app '("firefox" "icecat" "chromium" "google-chrome"))
      (when (executable-find app)
	(cl-return (start-process "BROWSER" nil app)))))

  (defun e/sistema-tirador ()
    (interactive)
    (alert "I shot the sheriff")
    (start-process "SCREENSHOT" nil "cejo" "ops" "screenshot" "full"))

  (defun e/sistema-screenshot-partial ()
    (interactive)
    (alert "Taking partial shot.")
    (start-process "SCREENSHOT" nil "cejo" "ops" "screenshot" "partial"))

  (defun e/sistema-inicia-comando ()
    "Inicia programas como Dmenu & Rofi.
  Tab/C-M-i para completar. n-[b/p] para andar no historico de comandos frente/atras."
    (interactive)
    (require 'subr-x)
    (start-process "INICIA" nil (string-trim-right (read-shell-command "VAI: "))))

  (defun e/exwm-statusbar-memory ()
    "Get laptop memory usage."
    (let ((mem
	   (string-trim
	    (shell-command-to-string
	     "free -h | gawk  '/Mem:/{print $3}'"))))
      mem))

  (defun e/exwm-statusbar-battery ()
    "Get laptop battery current capacity."
    (let ((bat
	   (concat
	    (string-trim
	     (shell-command-to-string
	      "cat /sys/class/power_supply/BAT0/capacity"))
	    "%")))
      bat))

  (defun e/exwm-statusbar-cpu-temperature ()
    "Get CPU current temperature."
    (let ((cpu-temp
	   (concat (substring
		    (string-trim
		     (shell-command-to-string
		      "cat /sys/class/thermal/thermal_zone1/temp"))
		    0 -3)
		   "°C")))
      cpu-temp))

  (defun e/exwm-statusbar-cpu-frequency ()
    "Get CPU current frequency."
    (let ((cpu-freq
	   (concat (number-to-string
		    (let ((a (split-string (shell-command-to-string
					    "grep 'cpu ' /proc/stat"))))
		      (/ (* (+ (string-to-number (nth 1 a))
			       (string-to-number (nth 3 a)))
			    100)
			 (+ (string-to-number (nth 1 a))
			    (string-to-number (nth 3 a))
			    (string-to-number (nth 4 a))))))
		   "%")))
      cpu-freq))

  (defun e/exwm-statusbar-date-time ()
    "Get System current time and date."
    (let ((time
	   (format-time-string "%A %D - %l:%M %p" (current-time))))
      time))

  (defun e/exwm-statusbar-volume ()
    "Get System current audio volume, Pulseaudio."
    (let ((volume (let ((current-volume (shell-command-to-string "amixer get Master")))
		    (string-match "\\([0-9]+%\\)" current-volume)
		    (match-string 0 current-volume))))
      volume))

  (defun e/exwm-statusbar ()
    "Use echo-area to display system stats"
    (e/sysinfo))

  (defun e/exwm-cmd (program &rest program-args)
    (interactive)
    (apply #'make-process
	   (append (list :name "EXWM-CMD" :buffer "EXWM-CMD")
		   (if program
		       (list :command (cons program program-args))))))

  (defun e/exwm-bind-cmd (bind program &rest program-args)
    "Run CMD with ARGS."
    (exwm-input-set-key (kbd bind)
			((lambda (program program-args)
			   (apply #'make-process
				  (append (list :name "EXWM-CMD" :buffer "EXWM-CMD")
					  (if program
					      (list :command (cons program program-args))))))
			 program program-args)))

  ;; Emms
  ;; (exwm-input-set-key (kbd "s-A") 'emms-previous)
  (exwm-input-set-key (kbd "s-D") 'emms-next)
  (exwm-input-set-key (kbd "s-SPC") 'emms-pause)

  (exwm-input-set-key (kbd "s-i") 'bufler)

  (when (require 'counsel nil :noerror)
   (exwm-input-set-key (kbd "s-x") 'counsel-linux-app))

  ;; [System Software bindings]
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") #'e/sistema-volume-cima)
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") #'e/sistema-volume-baixo)
  (exwm-input-set-key (kbd "<XF86AudioMute>") #'e/sistema-volume-alternar)
  (exwm-input-set-key (kbd "<print>") #'e/sistema-tirador)
  (exwm-input-set-key (kbd "s-C-x") 'e/sistema-tirador)
  (exwm-input-set-key (kbd "s-A") 'e/sistema-brilho-cima)
  (exwm-input-set-key (kbd "s-W") 'e/sistema-volume-cima)
  (exwm-input-set-key (kbd "s-S") 'e/sistema-volume-baixo)
  (exwm-input-set-key (kbd "s-E") 'e/sistema-volume-alternar)
  (exwm-input-set-key (kbd "s-P") 'e/sistema-toca)

  (exwm-input-set-key (kbd "s-Q") 'e/sistema-sair)
  (exwm-input-set-key (kbd "s-l") 'e/sistema-fechar)

  (exwm-input-set-key (kbd "s-B") 'e/sistema-browser)
  (exwm-input-set-key (kbd "s-T") 'e/sistema-terminal)
  (exwm-input-set-key (kbd "s-v") 'e/sysinfo)

  (exwm-input-set-key (kbd "s-I") 'e/exwm-input-toggle-mode)
  (exwm-input-set-key (kbd "s-g") 'e/narrow-or-widen-dwim)

  (e/autostart-scripts)
  (e/autostart))

(provide 'init-repository-exwm)
