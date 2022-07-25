;;; package --- Builtin Packages Configurations -*- lexical-binding: t;

;;; Commentary:
;;; Code:

;; =========================
;; * BUILTIN PACKAGES
;; =========================

(after! erc
  (setq erc-nick user-login-name
        erc-server "irc.libera.chat"
	erc-autojoin-channels-alist '(("libera.chat"
                                       "#ruby"
                                       "#guile"
                                       "#scheme"
				       "#emacs"))))

(add-hook 'image-mode-hook
          (lambda ()
            ;; open next/previous image fitted
            (local-set-key (kbd "<right>") (lambda ()
				             (interactive)
				             (image-next-file 1)
				             (image-transform-fit-to-width)))
            (local-set-key (kbd "<left>") (lambda ()
				            (interactive)
				            (image-previous-file 1)
				            (image-transform-fit-to-width)))))

(provide 'init-builtin)
;;; init-builtin.el ends here
