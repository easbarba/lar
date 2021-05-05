--       █████╗ ██╗    ██╗███████╗███████╗ ██████╗ ███╗   ███╗███████╗
--      ██╔══██╗██║    ██║██╔════╝██╔════╝██╔═══██╗████╗ ████║██╔════╝
--      ███████║██║ █╗ ██║█████╗  ███████╗██║   ██║██╔████╔██║█████╗
--      ██╔══██║██║███╗██║██╔══╝  ╚════██║██║   ██║██║╚██╔╝██║██╔══╝
--      ██║  ██║╚███╔███╔╝███████╗███████║╚██████╔╝██║ ╚═╝ ██║███████╗
--      ╚═╝  ╚═╝ ╚══╝╚══╝ ╚══════╝╚══════╝ ╚═════╝ ╚═╝     ╚═╝╚══════╝

pcall(require, "luarocks.loader")

require "awful.autofocus"

-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require "awful.hotkeys_popup.keys"

-- base
require "base.autorun"
require "base.themes"
require "base.errors"
require "base.inputs"
require "base.wibar"
require "base.rules"
require "base.signals"

-- Non-Standard additional libraries
require "modules.gtk-icons"
require "modules.battery-warning"
