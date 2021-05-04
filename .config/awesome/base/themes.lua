local gears = require("gears")
local beautiful = require("beautiful")

-- Themes define colours, icons, font and wallpapers.

beautiful.init(gears.filesystem.get_themes_dir() .. "zenburn/theme.lua")
-- beautiful.init(gears.filesystem.get_configuration_dir() .. "myTheme.lua")

-- Wallpaper
beautiful.wallpaper = os.getenv("HOME") .. "/Pictures/wallpapers/communist-flag.jpg"
