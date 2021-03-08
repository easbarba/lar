--- * GTK Themes/Icons

local naughty = require("naughty")

naughty.config.icon_dirs = {
   "/usr/share/pixmaps/",
   "/usr/share/icons/Numix/48/status/",
   "/usr/share/icons/Numix/48/devices/"
}

naughty.config.icon_formats = { "png", "gif", "svg" }
