local gears = require("gears")
local awful = require("awful")
local menubar = require("menubar")

-- * Shared Variables
local terminal = "gnome-terminal"
local editor = "emacs"
local lockscreen = function()
    awful.util.spawn("slock")
end

local beautiful = require("beautiful")
local hotkeys_popup = require("awful.hotkeys_popup")

local has_fdo, freedesktop = pcall(require, "freedesktop")

-- * Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
    {
        "hotkeys",
        function()
            hotkeys_popup.show_help(nil, awful.screen.focused())
        end
    },
    {"manual", terminal .. " -e man awesome"},
    -- { "edit config", editor_cmd .. " " .. awesome.conffile },
    {"restart", awesome.restart},
    {
        "quit",
        function()
            awesome.quit()
        end
    }
}

local menu_awesome = {"awesome", myawesomemenu, beautiful.awesome_icon}
local menu_terminal = {"open terminal", terminal}

if has_fdo then
    mymainmenu =
        freedesktop.menu.build(
        {
            before = {menu_awesome},
            after = {menu_terminal}
        }
    )
else
    mymainmenu =
        awful.menu(
        {
            items = {
                menu_awesome,
                {"Lock", lockscreen},
                -- { "Debian", debian.menu.Debian_menu.Debian },
                menu_terminal
            }
        }
    )
end

mylauncher =
    awful.widget.launcher(
    {
        image = beautiful.awesome_icon,
        menu = mymainmenu
    }
)

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout({"br"})

-- Default modkey.
modkey = "Mod4"

-- * Mouse bindings
root.buttons(
    gears.table.join(
        awful.button(
            {},
            3,
            function()
                mymainmenu:toggle()
            end
        ),
        awful.button({}, 4, awful.tag.viewnext),
        awful.button({}, 5, awful.tag.viewprev)
    )
)

-- * Key bindings
globalkeys =
    gears.table.join(
    awful.key({modkey, "Shift"}, "h", hotkeys_popup.show_help, {description = "show help", group = "awesome"}),
    awful.key({modkey}, "Left", awful.tag.viewprev, {description = "view previous", group = "tag"}),
    awful.key({modkey}, "Right", awful.tag.viewnext, {description = "view next", group = "tag"}),
    awful.key({modkey}, "Tab", awful.tag.history.restore, {description = "go back", group = "tag"}),
    -- USER CUSTOM CONFIG - BEGIN

    awful.key({modkey}, "c", lockscreen),
    awful.key(
        {modkey},
        "w",
        function()
            awful.spawn("s_wm_volume --up")
        end
    ),
    awful.key(
        {modkey},
        "s",
        function()
            awful.spawn("s_wm_volume --down")
        end
    ),
    awful.key(
        {modkey},
        "e",
        function()
            awful.spawn("s_wm_volume --toggle")
        end
    ),
    awful.key(
        {modkey},
        "a",
        function()
            awful.spawn("s_wm_backlight --up")
        end
    ),
    awful.key(
        {modkey},
        "d",
        function()
            awful.spawn("s_wm_backlight --down")
        end
    ),
    awful.key(
        {modkey},
        "Print",
        function()
            awful.spawn("s_wm_shot --full")
        end
    ),
    awful.key(
        {modkey, "Shift"},
        "Print",
        function()
            awful.spawn("s_wm_shot --partial")
        end
    ),
    awful.key(
        {modkey},
        "e",
        function()
            awful.spawn("mpc toggle")
        end
    ),
    awful.key(
        {modkey, "Shift"},
        "a",
        function()
            awful.spawn("mpc prev")
        end
    ),
    awful.key(
        {modkey, "Shift"},
        "d",
        function()
            awful.spawn("mpc next")
        end
    ),
    awful.key(
        {modkey, "Shift"},
        "w",
        function()
            awful.spawn("mpc volume +3")
        end
    ),
    awful.key(
        {modkey, "Shift"},
        "s",
        function()
            awful.spawn("mpc volume -3")
        end
    ),
    awful.key(
        {modkey},
        "z",
        function()
            awful.spawn("playerctl play-pause")
        end
    ),
    awful.key(
        {modkey, "Shift"},
        "p",
        function()
            awful.spawn.with_shell("s-media-play")
        end
    ),
    -- USER CUSTOM CONFIG - END

    -- mostra / esconde wibox
    awful.key(
        {modkey},
        "b",
        function()
            myscreen = awful.screen.focused()
            myscreen.mywibox.visible = not myscreen.mywibox.visible
        end,
        {description = "toggle statusbar"}
    ),
    awful.key(
        {modkey},
        "j",
        function()
            awful.client.focus.byidx(1)
        end,
        {description = "focus next by index", group = "client"}
    ),
    awful.key(
        {modkey},
        "k",
        function()
            awful.client.focus.byidx(-1)
        end,
        {description = "focus previous by index", group = "client"}
    ),
    awful.key(
        {modkey, "Shift"},
        "m",
        function()
            mymainmenu:show()
        end,
        {description = "show main menu", group = "awesome"}
    ),
    -- Layout manipulation
    awful.key(
        {modkey, "Shift"},
        "j",
        function()
            awful.client.swap.byidx(1)
        end,
        {description = "swap with next client by index", group = "client"}
    ),
    awful.key(
        {modkey, "Shift"},
        "k",
        function()
            awful.client.swap.byidx(-1)
        end,
        {description = "swap with previous client by index", group = "client"}
    ),
    awful.key(
        {modkey, "Control"},
        "j",
        function()
            awful.screen.focus_relative(1)
        end,
        {description = "focus the next screen", group = "screen"}
    ),
    awful.key(
        {modkey, "Control"},
        "k",
        function()
            awful.screen.focus_relative(-1)
        end,
        {description = "focus the previous screen", group = "screen"}
    ),
    awful.key({modkey}, "u", awful.client.urgent.jumpto, {description = "jump to urgent client", group = "client"}),
    awful.key(
        {modkey},
        "Escape",
        function()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "client"}
    ),
    -- Standard program
    awful.key(
        {modkey},
        "Return",
        function()
            awful.spawn(terminal)
        end,
        {description = "open a terminal", group = "launcher"}
    ),
    awful.key(
        {modkey, "Shift"},
        "Return",
        function()
            awful.spawn(editor)
        end,
        {description = "open editor", group = "editor"}
    ),
    awful.key({modkey, "Control"}, "r", awesome.restart, {description = "reload awesome", group = "awesome"}),
    awful.key({modkey, "Control"}, "q", awesome.quit, {description = "quit awesome", group = "awesome"}),
    awful.key(
        {modkey},
        "l",
        function()
            awful.tag.incmwfact(0.05)
        end,
        {description = "increase master width factor", group = "layout"}
    ),
    awful.key(
        {modkey},
        "h",
        function()
            awful.tag.incmwfact(-0.05)
        end,
        {description = "decrease master width factor", group = "layout"}
    ),
    awful.key(
        {modkey, "Shift"},
        "h",
        function()
            awful.tag.incnmaster(1, nil, true)
        end,
        {description = "increase the number of master clients", group = "layout"}
    ),
    awful.key(
        {modkey, "Shift"},
        "l",
        function()
            awful.tag.incnmaster(-1, nil, true)
        end,
        {description = "decrease the number of master clients", group = "layout"}
    ),
    awful.key(
        {modkey, "Control"},
        "h",
        function()
            awful.tag.incncol(1, nil, true)
        end,
        {description = "increase the number of columns", group = "layout"}
    ),
    awful.key(
        {modkey, "Control"},
        "l",
        function()
            awful.tag.incncol(-1, nil, true)
        end,
        {description = "decrease the number of columns", group = "layout"}
    ),
    awful.key(
        {modkey},
        "space",
        function()
            awful.layout.inc(1)
        end,
        {description = "select next", group = "layout"}
    ),
    awful.key(
        {modkey, "Shift"},
        "space",
        function()
            awful.layout.inc(-1)
        end,
        {description = "select previous", group = "layout"}
    ),
    awful.key(
        {modkey, "Control"},
        "n",
        function()
            local c = awful.client.restore()
            -- Focus restored client
            if c then
                c:emit_signal("request::activate", "key.unminimize", {raise = true})
            end
        end,
        {description = "restore minimized", group = "client"}
    ),
    -- Prompt
    awful.key(
        {modkey},
        "r",
        function()
            awful.screen.focused().mypromptbox:run()
        end,
        {description = "run prompt", group = "launcher"}
    ),
    awful.key(
        {modkey},
        "x",
        function()
            awful.prompt.run {
                prompt = "Run Lua code: ",
                textbox = awful.screen.focused().mypromptbox.widget,
                exe_callback = awful.util.eval,
                history_path = awful.util.get_cache_dir() .. "/history_eval"
            }
        end,
        {description = "lua execute prompt", group = "awesome"}
    ),
    -- Menubar
    awful.key(
        {modkey},
        "p",
        function()
            menubar.show()
        end,
        {description = "show the menubar", group = "launcher"}
    )
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys =
        gears.table.join(
        globalkeys,
        -- View tag only.
        awful.key(
            {modkey},
            "#" .. i + 9,
            function()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    tag:view_only()
                end
            end,
            {description = "view tag #" .. i, group = "tag"}
        ),
        -- Toggle tag display.
        awful.key(
            {modkey, "Control"},
            "#" .. i + 9,
            function()
                local screen = awful.screen.focused()
                local tag = screen.tags[i]
                if tag then
                    awful.tag.viewtoggle(tag)
                end
            end,
            {description = "toggle tag #" .. i, group = "tag"}
        ),
        -- Move client to tag.
        awful.key(
            {modkey, "Shift"},
            "#" .. i + 9,
            function()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:move_to_tag(tag)
                    end
                end
            end,
            {description = "move focused client to tag #" .. i, group = "tag"}
        ),
        -- Toggle tag on focused client.
        awful.key(
            {modkey, "Control", "Shift"},
            "#" .. i + 9,
            function()
                if client.focus then
                    local tag = client.focus.screen.tags[i]
                    if tag then
                        client.focus:toggle_tag(tag)
                    end
                end
            end,
            {description = "toggle focused client on tag #" .. i, group = "tag"}
        )
    )
end

-- Set keys
root.keys(globalkeys)
