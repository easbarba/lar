local gears = require("gears")
local awful = require("awful")
local naughty = require("naughty")

-- Autorun programs
local autostart = "s_wm_autostart" -- gears.filesystem.get_xdg_config_home() .. "autostart-scripts/apps"
-- if gears.filesystem.file_executable(autostart) then
awful.spawn(autostart)
-- else
-- naughty.notify({title = "autostart not found!", text = "xdg:autostart script not found!"})
-- end
