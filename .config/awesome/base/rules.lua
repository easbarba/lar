local gears = require("gears")
local awful = require("awful")
local beautiful = require("beautiful")

clientbuttons = gears.table.join(
   awful.button({ }, 1, function (c)
	 c:emit_signal("request::activate", "mouse_click", {raise = true})
   end),
   awful.button({ modkey }, 1, function (c)
	 c:emit_signal("request::activate", "mouse_click", {raise = true})
	 awful.mouse.client.move(c)
   end),
   awful.button({ modkey }, 3, function (c)
	 c:emit_signal("request::activate", "mouse_click", {raise = true})
	 awful.mouse.client.resize(c)
   end)
)

clientkeys = gears.table.join(
   awful.key({ modkey,           }, "f",
      function (c)
	 c.fullscreen = not c.fullscreen
	 c:raise()
      end,
      {description = "toggle fullscreen", group = "client"}),
   awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
      {description = "close", group = "client"}),
   awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
      {description = "toggle floating", group = "client"}),
   awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
      {description = "move to master", group = "client"}),
   awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
      {description = "move to screen", group = "client"}),
   awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
      {description = "toggle keep on top", group = "client"}),
   awful.key({ modkey,           }, "n",
      function (c)
	 -- The client currently has the input focus, so it cannot be
	 -- minimized, since minimized clients can't have the focus.
	 c.minimized = true
      end ,
      {description = "minimize", group = "client"}),
   awful.key({ modkey,           }, "m",
      function (c)
	 c.maximized = not c.maximized
	 c:raise()
      end ,
      {description = "(un)maximize", group = "client"}),
   awful.key({ modkey, "Control" }, "m",
      function (c)
	 c.maximized_vertical = not c.maximized_vertical
	 c:raise()
      end ,
      {description = "(un)maximize vertically", group = "client"}),
   awful.key({ modkey, "Shift"   }, "m",
      function (c)
	 c.maximized_horizontal = not c.maximized_horizontal
	 c:raise()
      end ,
      {description = "(un)maximize horizontally", group = "client"})
)

-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
   -- All clients will match this rule.
   { rule = { },
     properties = { border_width = beautiful.border_width,
		    border_color = beautiful.border_normal,
		    focus = awful.client.focus.filter,
		    raise = true,
		    keys = clientkeys,
		    buttons = clientbuttons,
		    screen = awful.screen.preferred,
		    placement = awful.placement.no_overlap+awful.placement.no_offscreen
     }
   },

   -- Floating clients.
   { rule_any = {
	instance = {
	   "DTA",  -- Firefox addon DownThemAll.
	   "copyq",  -- Includes session name in class.
	   "pinentry",
	},
	class = {
	   "Arandr",
	   "Blueman-manager",
	   "Gpick",
	   "Kruler",
	   "MessageWin",  -- kalarm.
	   "Sxiv",
	   "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
	   "Wpa_gui",
	   "veromix",
	   "xtightvncviewer"},

	-- Note that the name property shown in xprop might be set slightly after creation of the client
	-- and the name shown there might not match defined rules here.
	name = {
	   "Event Tester",  -- xev.
	},
	role = {
	   "AlarmWindow",  -- Thunderbird's calendar.
	   "ConfigManager",  -- Thunderbird's about:config.
	   "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
	}
   }, properties = { floating = true }},

   -- Add titlebars to normal clients and dialogs
   { rule_any = { type = { "normal", "dialog" } }, properties = { titlebars_enabled = false } },

   -- Tags: Apps to always map on the specified tag.
   { rule_any = { class = { "Emacs" }},
     properties = { tag = "mx" } },
   { rule_any = { class = { "Atril", "libreoffice-writer" }},
     properties = { tag = "reader" } },
   { rule_any = { class = { "st", "st-256color", "Mate-terminal" }},
     properties = { tag = "term" } },
   { rule_any = { class = { "IceCat", "Firefox", "Chromium" }},
     properties = { tag = "www" } },
   { rule_any = { class = { "mpv", "vlc", "Clementine", "feh", "Steam", "telegram-desktop", "TelegramDesktop" }},
     properties = { tag = "media" } },


   -- Fullscreen Windows:
   { rule_any = { class = { "Emacs", "emacs", "mpv", "st", "Mate-terminal" }},
     properties = { fullscreen = true }},
   { rule_any = { class = { "mpv", "vlc" }},
     properties = { switchtotag = true }},
   { rule = { class = "dosbox" },
     callback = function(c) awful.spawn('mpc pause') end }
}
