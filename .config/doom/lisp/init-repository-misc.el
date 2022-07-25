;;; init-repository-misc --- Repository Miscellaneous Packages & Libraries -*- lexical-binding: t;

;;; Commentary:

;; ================================================
;;; * REPOSITORY MISCELLANEOUS PACKAGES & LIBRARIES
;; ================================================

;;; Code:

(after! elfeed
  (setq elfeed-db-directory "~/.config/elfeed")
  (setq elfeed-feeds
   '("https://www.fsf.org/static/fsforg/rss/news.xml"
     "https://www.fsf.org/static/fsforg/rss/blogs.xml"
     ;; scheme blogs
     "https://wingolog.org/feed/atom"
     "https://elephly.net/feed.xml"
     "https://guix.gnu.org/feeds/blog.atom"
     "https://dthompson.us/feed.xml"
     ;; misc blogs
     "https://tonarinoyj.jp/rss/series/13932016480028984490"
     ;; distros blogs
     "https://guix.gnu.org/feeds/blog.atom"
     "https://pointieststick.com/feed/"
     ;; prog blogs
     "https://michael.stapelberg.ch/feed.xml"
     "https://edelpero.svbtle.com/feed"
     "https://drewdevault.com/blog/index.xml"
     "https://ambrevar.xyz/atom.xml"
     "https://unixsheikh.com/feed.rss"
     "https://sourcehut.org/blog/index.xml"
     "https://nullprogram.com/feed/"
     "https://billykorando.com/feed/"
     ;; progs blogs br
     "https://leandronsp.com/rss.xml"
     ;; devops
     "https://blog.bobbyallen.me/feed/"
     ;; devops br
     "https://feeds.feedburner.com/ramonduraes"
     "https://knela.dev/index-rally?format=rss"
     ;; FLOSS channels
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCuj_loxODrOPxSsXDfJmpng" ;; Andrew Tropin
     "https://www.youtube.com/feeds/videos.xml?channel_id=UC9NuJImUbaSNKiwF2bdSfAw" ;; FOSDEM
     ;; sql channels
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCAWsBMQY4KSuOuGODki-l7A" ;; Data interview Pro
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCiWbFL7I1PgC5OnhIVkXySQ" ;; Vanny
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCW8Ews7tdKKkBT6GdtQaXvQ" ;; StrataScratch
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCCDA5Yte0itW_Bf6UHpbHug" ;; Postgres Open
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCsJkVvxwoM7R9oRbzvUhbPQ" ;; Postgres Conference
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCkIPoYyNr1OHgTo0KwE9HJw" ;; EDB
     ;; elixir channels
     "https://www.youtube.com/feeds/videos.xml?channel_id=UChlCFJe7gDdED1DMFUmpEJg" ;; Chris McCord
     "https://www.youtube.com/feeds/videos.xml?channel_id=UC0l2QTnO1P2iph-86HHilMQ" ;; ElixirConf
     "https://www.youtube.com/feeds/videos.xml?channel_id=UC47eUBNO8KBH_V8AfowOWOw" ;; Code Sync
     ;; golang channels
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCoOjmHXV2STS3l5yJlFGoig" ;; Aprenda Golang
     ;; prog channels
     "https://www.youtube.com/feeds/videos.xml?channel_id=UC2KfmYEM4KCuA1ZurravgYw" ;; Amigos Code
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCBWbWViVqDHckknir8PIIdg" ;; O Bruno Germano
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCqa6i-EYjkIqVhu1CpsBOPQ" ;; Filho da nuvem
     "https://www.youtube.com/feeds/videos.xml?channel_id=UC9-y-6csu5WGm29I7JiwpnA" ;; Computerphile
     ;; gnu/linux channels
     "https://www.youtube.com/feeds/videos.xml?channel_id=UC629vKGFPRc1rz6VDm6OZiQ" ;; Diolinux clips
     "https://www.youtube.com/feeds/videos.xml?channel_id=UC5UAwBUum7CPN5buc-_N1Fw" ;; Linux Experiment
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCVls1GmFKf6WlTraIb_IaJg" ;; Distro Tube
     ;; devops channels
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCfz8x0lVzJpb_dgWm9kPVrw" ;; DevOps Toolkit
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCR-DXc1voovS8nhAvccRZhg" ;; Jeff Geerling
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCCCuGbB3Vf6AKags3WgW2Cw" ;; souzaxx
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCZPhOgp4kDXkg2W8jL4U7GA" ;; LondonGophers
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCQnpN5AUd36lnMHuIl_rihA" ;; Caio Delgado
     "https://www.youtube.com/feeds/videos.xml?channel_id=UC3yoPZJdjwOjrHWy_iEP08A" ;; Igor Souza
     "https://www.youtube.com/feeds/videos.xml?channel_id=UC-JJ1NXjx8QI-tuG0PMFbhA" ;; Matheus Fidelis
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCdngmbVKX1Tgre699-XLlUA" ;; TechWorld with Nana
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCfz8x0lVzJpb_dgWm9kPVrw" ;; DevOps Toolkit
     "https://www.youtube.com/feeds/videos.xml?channel_id=UConIMoZxnroqKqCc-utZA7A" ;; Carlos Enog
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCPNrIITPNFFLmcU3VfoKuGQ" ;; Gomex
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCJnKVGmXRXrH49Tvrx5X0Sw" ;; Linuxtips br
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCyNp3i0UZeTL11CUBs9mZyA" ;; Punkdevops
     ;; entertainment channels
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCqRraVICLr0asn90cAvkIZQ" ;; Corinthians TV
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCyEd6QBSgat5kkC6svyjudA" ;; Mark Wiens
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCmRFj7s3qBtadUujBd3Tujg" ;; Panelaco
     ;; misc channels
     "https://www.youtube.com/feeds/videos.xml?channel_id=UC0fGGprihDIlQ3ykWvcb9hg" ;; Tese onze
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCQNp5qrzckO45QFyaVPh9Lg" ;; Ju Furno
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCW3nde-8K-5BaHAmQLZ7ycg" ;; Silvio Almeida
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCdKJlY5eAoSumIlcOcYxIGg" ;; Nunca Vi 1 cientista
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCrSM1UV_KLAdTzgxyxl3POw" ;; Ola Ciencia
     ;; vegan channels
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCtdrvQPKPB7dQG5XJsbillQ" ;; Flavio Giusti
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCZ6JHFBaDUJ9wfo41HSII_w" ;; Fabio Chaves
     "https://www.youtube.com/feeds/videos.xml?channel_id=UCEjkioV3LO_OIUaSWRxFZ3A" ;; Cheap Lazy Vegan
     ;; emacs blogs
     "http://sachachua.com/blog/category/emacs/feed"))
  (setq url-queue-timeout 30)
  (setq elfeed-search-filter "@2-week-ago +unread")

  ;; Entries older than 2 weeks are marked as read
  (add-hook 'elfeed-new-entry-hook
	    (elfeed-make-tagger :before "3 weeks ago"
				:remove 'unread))

  (defun e/elfeed-yt-channel-id ()
    "Paste Youtube's channel id to formatted RSS feed for Elfeed or GNUS."
    (interactive)
    (insert (concat "\""
		    "https://www.youtube.com/feeds/videos.xml?channel_id="
		    (nth 4
			 (split-string
			  (read-from-minibuffer
			   "Enter Youtube's channel id: ") "/"))
		    "\" ;; "
		    (read-from-minibuffer "Enter Elfeed entry name: "))))

  (defun e/elfeed-play-with-video-player ()
    "Play entry link with video player - Ambrevar."
    (interactive)
    (let ((entry (if (eq major-mode 'elfeed-show-mode)
		     elfeed-show-entry
		   (elfeed-search-selected :single))))
      (e/play-video (elfeed-entry-link entry))))
  (define-key elfeed-search-mode-map "m" #'e/elfeed-play-with-video-player))

(use-package! define-word
  :config
  (global-set-key (kbd "C-c a d p") 'define-word-at-point)
  (global-set-key (kbd "C-c a d w") 'define-word))

(use-package! org-make-toc
  :hook (org-mode . org-make-toc-mode))

(use-package! guix
  :defer t
  :hook (scheme-mode . guix-devel-mode))

(use-package! zoom
  :defer 1
  :config
  (zoom-mode)
  (setq zoom-ignored-major-modes '(dired-mode ediff-mode)))

(provide 'init-repository-misc)
;;; init-repository-misc.el ends here
