(use-modules (gnu)
	     (gnu system nss)
	     (gnu packages base)
	     (gnu services xorg)
	     (gnu system locale)
	     (rnrs lists))

(use-service-modules desktop networking ssh)

(use-package-modules certs bootloaders python wm display-managers xdisorg
		     version-control suckless tmux pulseaudio gstreamer linux
		     admin compression wget xorg zile vim xorg bash ssh
		     gnome gcc)

(operating-system
  (host-name "guixsd")
  (timezone "America/Sao_Paulo")

  (locale "en_US.utf8")

  (bootloader (bootloader-configuration
	       (bootloader grub-efi-bootloader)
	       (target "/boot/efi"))) ;; In a fresh install, mount as (target "/mnt/boot/efi")

  (file-systems (append
		 (list (file-system
			 (mount-point "/")
			 (type "ext4")
			 (needed-for-boot? #t)
			 (device (file-system-label "root")))
		       (file-system
			 (mount-point "/boot")
			 (type "ext2")
			 (device (file-system-label "boot")))
		       (file-system
			 (mount-point "/home")
			 (type "ext4")
			 (device (file-system-label "home")))
		       (file-system
			 (mount-point "/dados")
			 (type "ext4")
			 (device "/dev/sda5"))
		       (file-system
			 (mount-point "/onda")
			 (type "ext4")
			 (device "/dev/sda6"))
		       (file-system
			 (mount-point "/boot/efi")
			 (type "vfat")
			 (device (file-system-label "EFI"))))
		 %base-file-systems))

  (swap-devices '("/dev/sda7"))

  (users (cons*
	  (user-account
	   (name "elxbarbosa")
	   (comment "o sol seja louvado!")
	   (group "users")
	   (supplementary-groups '("wheel" "netdev" "audio" "video"))
	   (home-directory "/home/elxbarbosa"))
	  %base-user-accounts))

  ;; System-wide packages.
  (packages (cons* nss-certs
		   gvfs
		   openssh
		   awesome
		   git
		   bash bash-completion
		   st
		   tmux
		   gst-plugins-bad
		   gst-plugins-base
		   gst-plugins-good
		   gst-plugins-ugly
		   gst123
		   gstreamer
		   htop
		   openssh
		   p7zip
		   pulseaudio
		   wget
		   alsa-lib
		   alsa-utils
		   xorg-server
		   xf86-input-libinput xf86-input-synaptics
		   zip unzip
		   zile
		   vim
		   glibc-utf8-locales
		   %base-packages))

  ;; System services
  (services (append (list (service slim-service-type(slim-configuration
						     (display ":0")
						     (vt "vt1")))
			  (service special-files-service-type
				   `(("/usr/bin/python" ,(file-append python "/bin/python3"))
				     ("/usr/bin/uname" ,(file-append coreutils "/bin/uname"))
				     ("/usr/bin/Xorg" ,(file-append coreutils "/bin/xorg"))
				     ("/usr/bin/env" ,(file-append coreutils "/bin/env"))
				     ("/bin/cc" ,(file-append gcc "/bin/gcc")))))

		    (remove (service gdm-service-type)
			    %desktop-services)))

  (name-service-switch %mdns-host-lookup-nss))
