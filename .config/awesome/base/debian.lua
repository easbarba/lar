-- Debian menu entries
if gears.filesystem.file_executable("/usr/bin/apt") then
   local debian = require("debian.menu")
end
