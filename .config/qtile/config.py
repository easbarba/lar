#!/usr/bin/env python3

"""Qtile configuration."""

from pathlib import Path
from shutil import which
from dataclasses import dataclass

try:
    from typing import List  # noqa: F401
except ImportError:
    pass

from libqtile.config import Key, Screen, Drag, Click, Match, Group
from libqtile.lazy import lazy
from libqtile import layout, bar, widget, hook


TERMINAL = which("st") or which("kitty")
LOCKER = which("slock") or which("i3lock")
LAUNCHER = which("rofi") or which("dmenu")
EDITOR = which("emacs")
IDE = which("rider") or which("monodevelop") or which("code")
BROWSER = which("google-chrome") or which("chromium") or which("firefox")
HOME = Path.home()
MUSIC = HOME / "Musica"

# ----- KEYS
mod = "mod4"
alt = "mod1"
keys = [
    # BUILTIN
    Key([mod, alt], "e", lazy.layout.shuffle_down()),
    Key([mod, alt], "q", lazy.layout.shuffle_up()),
    Key([mod], "s", lazy.layout.down()),
    Key([mod], "w", lazy.layout.up()),
    Key([mod, alt], "space", lazy.next_layout()),
    Key([mod, alt], "space", lazy.layout.rotate()),
    Key([mod, alt], "Return", lazy.layout.toggle_split()),
    Key([mod, alt], "n", lazy.layout.toggle_maximize()),
    Key([mod], "a", lazy.screen.prev_group()),
    Key([mod], "d", lazy.screen.next_group()),
    Key([mod], "Tab", lazy.screen.togglegroup()),
    Key([mod, alt], "c", lazy.window.kill()),
    Key([mod, alt], "r", lazy.restart()),
    Key([mod, alt], "q", lazy.shutdown()),
    Key([mod, alt], "m", lazy.group.setlayout("max")),
    Key([mod, alt], "t", lazy.group.setlayout("stack")),
    Key([mod, alt], "f", lazy.window.toggle_fullscreen()),
    # MISC
    Key([mod], "x", lazy.spawncmd()),
    Key([mod, alt], "g", lazy.switchgroup()),
    Key([mod, alt], "b", lazy.hide_show_bar("bottom")),
    # SYSTEM APPLICATIONS
    Key([mod], "Return", lazy.spawn(TERMINAL)),
    Key([mod], "l", lazy.spawn(LOCKER)),
    Key([mod], "e", lazy.spawn(EDITOR)),
    Key([mod], "b", lazy.spawn(BROWSER)),
    Key([mod, "shift"], "x", lazy.spawn("rofi -modi run,drun -show drun")),
    Key([mod], "v", lazy.spawn(IDE)),
    Key([mod, "shift"], "p", lazy.spawn("cero media play")),
    Key([mod, "shift"], "v", lazy.spawn("cero media getmedia")),
    Key([mod, "shift"], "a", lazy.spawn("cero media getmedia 'vorbis'")),
    Key([], "Print", lazy.spawn("cero operations shot")),
    # Key([mod], "z", lazy.spawn(f"cero media play {MUSIC}/Pregacao")),
    Key([mod, "shift"], "n", lazy.spawn("mpc next")),
    Key([mod], "space", lazy.spawn("mpc toggle")),
    Key([mod, "shift"], "w", lazy.spawn("cero operations volume up")),
    Key([mod, "shift"], "s", lazy.spawn("cero operations volume down")),
    Key([mod, "shift"], "e", lazy.spawn("cero operations volume toggle")),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("cero operations volume up")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("cero operations volume down")),
    Key([], "XF86AudioMute", lazy.spawn("cero operations volume toggle")),
]

widget_defaults = dict(
    font="sans",
    fontsize=12,
    padding=3,
)
extension_defaults = widget_defaults.copy()
dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
auto_fullscreen = True
focus_on_window_activation = "smart"
wmname = "LG3D"  # required by java software

layouts = [
    layout.Max(),
    layout.Stack(num_stacks=2),
]

mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

floating_layout = layout.Floating(
    float_rules=[
        {"wmclass": "confirm"},
        {"wmclass": "dialog"},
        {"wmclass": "download"},
        {"wmclass": "error"},
        {"wmclass": "file_progress"},
        {"wmclass": "notification"},
        {"wmclass": "splash"},
        {"wmclass": "toolbar"},
        {"wmclass": "confirmreset"},  # gitk
        {"wmclass": "makebranch"},  # gitk
        {"wmclass": "maketag"},  # gitk
        {"wname": "branchdialog"},  # gitk
        {"wname": "pinentry"},  # GPG key password entry
        {"wmclass": "ssh-askpass"},  # ssh-askpass
    ]
)

# ----- GROUPS
groups_names = [
    ("mx", ["Emacs"]),
    ("devel", ["MonoDevelop", "kate", "jetbrains-rider", "Code"]),
    ("term", ["st", "st-256color", "lx-terminal"]),
    ("media", ["libreoffice", "mpv", "thunderbird"]),
    ("www", ["Chromium", "Google-chrome", "Firefox", "next"]),
]
groups = [Group(i, matches=[Match(wm_class=x)]) for i, x in groups_names]
for index, (name, config) in enumerate(groups_names, 1):
    keys.extend(
        [
            Key([mod], str(index), lazy.group[name].toscreen()),
            Key([mod, "shift"], str(index), lazy.window.togroup(name)),
        ]
    )

# ----- BAR
@dataclass
class Colors:
    """Qtile pallet of colors."""

    # Bucks
    black: str = "000000"
    white: str = "f7f0e1"
    red: str = "e81e17"
    blue: str = "172030"
    pink: str = "c6797e"
    purple: str = "DB6E8F"
    yellow: str = "9A7500"
    orange: str = "e3a32d"
    gray: str = "64727d"
    green: str = "00471b"


COLORS = Colors()
screens = [
    Screen(
        bottom=bar.Bar(
            [
                widget.WindowName(
                    fontsize=12,
                    foreground=COLORS.pink,
                    padding=10,
                    show_state=False,
                ),
                widget.Prompt(foreground=COLORS.pink, ignore_dups_history=True),
                widget.GroupBox(
                    highlight_method="block",
                    rounded=False,
                    this_current_screen_border=COLORS.white,
                    border=COLORS.white,
                    inactive=COLORS.pink,
                    active=COLORS.pink,
                    urgent_text=COLORS.blue,
                    background=COLORS.black,
                    foreground=COLORS.white,
                    hide_unused=True,
                ),
                widget.Volume(update_interval=5, foreground=COLORS.pink, padding=10),
                widget.Sep(padding=10),
                widget.Battery(
                    format="{percent:2.0%} {hour:d}:{min:02d}",
                    update_delay=5,
                    foreground=COLORS.pink,
                    low_foreground=COLORS.red,
                ),
                widget.Sep(padding=10),
                widget.Pomodoro(
                    color_active=COLORS.orange,
                    color_break=COLORS.white,
                    color_inactive=COLORS.pink,
                ),
                widget.Sep(padding=10),
                widget.ThermalSensor(foreground=COLORS.pink),
                widget.Sep(padding=10),
                widget.Memory(foreground=COLORS.pink),
                widget.Sep(padding=10),
                widget.Clock(format=" %a %d %b %I:%M %p ", foreground=COLORS.pink),
                widget.Systray(padding=5, foreground=COLORS.pink),
            ],
            size=25,
            background=COLORS.blue,
        ),
    ),
]


@hook.subscribe.startup_once
def autostart():
    """Auto Start Applications at Qtile start."""
    from subprocess import run

    run(["sh", "essential-apps"], cwd=HOME.joinpath("bin"), check=False)
