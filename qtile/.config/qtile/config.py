# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import os
import subprocess
from libqtile import bar, hook, layout
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from qtile_extras import widget

mod = "mod4"
colors = {
        "bg":        "#282828",
        "bg1":       "#3c3836",
        "bg2":       "#504945",
        "bg3":       "#665c54",
        "bg4":       "#7c6f64",
        "gray":      "#928374",
        "fg4":       "#a89984",
        "fg3":       "#bdae93",
        "fg2":       "#d5c4a1",
        "fg":        "#ebdbb2",
        "fg0":       "#fbf1c7",
        "red":       "#cc241d",
        "red_fg":    "#fb4934",
        "green":     "#98971a",
        "green_fg":  "#b8bb26",
        "yellow":    "#d79921",
        "yellow_fg": "#fabd2f",
        "blue":      "#458588",
        "blue_fg":   "#83a598",
        "purple":    "#b16286",
        "purple_fg": "#d3869b",
        "aqua":      "#689d6a",
        "aqua_fg":   "#8ec07c",
        "orange":    "#d65d0e",
        "orange_fg": "#fe8019",
        }

previous_focused = []


@hook.subscribe.client_focus
def client_focused(window):
    global previous_focused
    if len(previous_focused) < 2:
        previous_focused.append(window)
    elif previous_focused[1] != window:
        previous_focused[0] = previous_focused[1]
        previous_focused[1] = window
    # logger.info(f"FOCUSED {window}, {previous_focused}")


@lazy.function
def focus_previous_window(qtile):
    global previous_focused
    if len(previous_focused) == 2:
        group = previous_focused[0].group
        qtile.current_screen.set_group(group)
        # logger.info(f"FOCUS PREVIOUS {previous_focused[0]}")
        group.focus(previous_focused[0])


@lazy.function
def go_to_empty_group(qtile):
    for group in qtile.groups:
        if not group.windows:
            qtile.current_screen.set_group(group)
            break


@lazy.function
def send_to_empty_group(qtile, switch_group: bool = False):
    for group in qtile.groups:
        if not group.windows:
            qtile.current_window.togroup(group.name)
            if switch_group:
                qtile.current_screen.set_group(group)
            break


@hook.subscribe.client_focus
def bring_floating_to_front(window):
    if window.floating:
        window.cmd_bring_to_front()


@hook.subscribe.startup_once
def autostart():
    home = os.path.expanduser('~/.config/autostart.sh')
    subprocess.Popen([home])


keys = [
    Key([mod], "h", lazy.layout.left(),
        desc="Move focus to left"),
    Key([mod], "l", lazy.layout.right(),
        desc="Move focus to right"),
    Key([mod], "j", lazy.layout.down(),
        desc="Move focus down"),
    Key([mod], "k", lazy.layout.up(),
        desc="Move focus up"),
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(),
        desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(),
        desc="Move window to the right"),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(),
        desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(),
        desc="Move window up"),
    Key([mod, "control"], "h", lazy.layout.grow_left(),
        desc="Grow window to the left"),
    Key([mod, "control"], "l", lazy.layout.grow_right(),
        desc="Grow window to the right"),
    Key([mod, "control"], "j", lazy.layout.grow_down(),
        desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(),
        desc="Grow window up"),
    Key([mod], "equal", lazy.layout.normalize(),
        desc="Reset all window sizes"),
    Key([mod], "BackSpace", lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack"),
    Key([mod], "Tab", lazy.group.next_window(),
        desc="Move focus to next window"),
    Key([mod, "shift"], "Tab", lazy.group.prev_window(),
        desc="Move focus to previous window"),
    Key([mod], "comma", lazy.prev_screen(),
        desc="Move focus to previous screen"),
    Key([mod], "period", focus_previous_window(),
        desc="Move to last focused window"),
    Key([mod], "grave", go_to_empty_group(),
        desc="Move focus to empty group"),
    Key([mod, "shift"], "grave", send_to_empty_group(),
        desc="Move focused window to empty group"),
    Key([mod, "control"], "grave", send_to_empty_group(switch_group=True),
        desc="Switch to & move focused window to empty group"),
    Key([mod], "f", lazy.window.toggle_fullscreen(),
        desc="Toggle fullscren"),
    Key([mod, "shift"], "f", lazy.window.toggle_floating(),
        desc="Toggle floating"),

    # Application keybindings
    Key([mod, "shift"], "Return", lazy.spawn("firefox"),
        desc="Launch browser"),
    Key([mod], "Return", lazy.spawn("kitty"),
        desc="Launch terminal"),
    Key([mod], "space",
        lazy.spawn("rofi -modi drun,run -show drun -show-icons"),
        desc="Open application with rofi"),
    Key([mod, "control"], "space", lazy.spawn("rofi -show window -show-icons"),
        desc="Switch window with rofi"),
    Key([mod], "n", lazy.spawn("kitty --class ranger -e ranger"),
        desc="Open ranger"),
    Key([mod, "shift"], "n", lazy.spawn("thunar"),
        desc="Open thunar"),
    Key([mod], "m", lazy.spawn("kitty --class pulsemixer -e pulsemixer"),
        desc="Open pulsemixer"),
    Key([mod, "shift"], "slash", lazy.spawn("kitty --class btop -e btop"),
        desc="Open btop"),

    # Screenshot keybindings
    Key([], "Print", lazy.spawn("scrot '/tmp/%F_%T_$wx$h.png' -e 'xclip -selection clipboard -target image/png -i $f' && notify-send 'Screnshot copied to clipboard'", shell=True),
        desc="Screenshot and copy to clipboard"),
    Key(["shift"], "Print", lazy.spawn("scrot -bs -l mode=edge '/tmp/%F_%T_$wx$h.png' -e 'xclip -selection clipboard -target image/png -i $f' && notify-send 'Screnshot copied to clipboard'", shell=True),
        desc="Screenshot area and copy to clipboard"),
    Key(["control"], "Print", lazy.spawn("scrot -bu '/tmp/%F_%T_$wx$h.png' -e 'xclip -selection clipboard -target image/png -i $f' && notify-send 'Screnshot copied to clipboard'", shell=True),
        desc="Screenshot active window and copy to clipboard"),

    # Media control
    Key([], "XF86AudioRaiseVolume", lazy.spawn("pamixer -i 5"),
        desc="Raise volume"),
    Key([], "XF86AudioLowerVolume", lazy.spawn("pamixer -d 5"),
        desc="Lower volume"),
    Key([], "XF86AudioMute", lazy.spawn("pamixer -t"),
        desc="Mute volume"),
    Key([], "XF86AudioMicMute", lazy.spawn("pamixer --default source -t"),
        desc="Mute microphone"),
    Key([], "XF86AudioPlay",
        lazy.spawn("playerctl play-pause -p spotify,%any"),
        desc="Play/pause media"),
    Key([], "XF86AudioPause",
        lazy.spawn("playerctl play-pause -p spotify,%any"),
        desc="Play/pause media"),
    Key([], "XF86AudioNext", lazy.spawn("playerctl next -p spotify,%any"),
        desc="Next media"),
    Key([], "XF86AudioPrev", lazy.spawn("playerctl previous -p spotify,%any"),
        desc="Previous media"),

    # Screen brightness control
    Key([], "XF86MonBrightnessUp", lazy.spawn("xbacklight -inc 5"),
        desc="Increment screen brightness"),
    Key([], "XF86MonBrightnessDown", lazy.spawn("xbacklight -dec 5"),
        desc="Decrement screen brightness"),

    # Dunst keybindings
    Key(['control'], "space", lazy.spawn("dunstctl close-all"),
        desc="Close all notifications"),
    Key(['control'], "grave", lazy.spawn("dunstctl history-pop"),
        desc="Show last notification"),
    Key(['control', 'shift'], "grave", lazy.spawn("dunstctl context"),
        desc="Open notification context"),

    Key([mod, "shift"], "q", lazy.window.kill(),
        desc="Kill focused window"),
    Key([mod, "control"], "r", lazy.reload_config(),
        desc="Reload the config"),
    Key([mod, "control"], "q", lazy.shutdown(),
        desc="Shutdown Qtile"),
]

groups = [Group(i) for i in "123456789"]

for i in groups:
    keys.extend(
        [
            Key([mod], i.name,
                lazy.group[i.name].toscreen(),
                desc=f"Switch to group {i.name}"),
            Key([mod, "shift"], i.name,
                lazy.window.togroup(i.name),
                desc=f"Move focused window to group {i.name}"),
            Key([mod, "control"], i.name,
                lazy.window.togroup(i.name, switch_group=True),
                desc=f"Switch to & move focused window to group {i.name}"),
        ]
    )

layouts = [
    layout.Columns(border_focus=colors["fg2"],
                   border_focus_stack=colors["yellow"],
                   border_normal=colors["bg2"],
                   border_normal_stack=colors["bg2"],
                   border_width=4),
]

widget_defaults = {
    "font": "Iosevka Term Nerd Font",
    "fontsize": 16,
    "padding": 10,
}
extension_defaults = widget_defaults.copy()

screens = [
    Screen(
        top=bar.Bar(
            [
                widget.Clock(format="%Y-%m-%d %a %I:%M %p",
                             foreground=colors["fg"],
                             fmt=' {}'),
                widget.Spacer(),
                widget.GroupBox(highlight_method="line",
                                active=colors["fg"],
                                this_screen_border=colors["purple"],
                                this_current_screen_border=colors["purple"],
                                other_screen_border=colors["gray"],
                                other_current_screen_border=colors["gray"],
                                padding=8),
                widget.Spacer(),
                widget.Systray(padding=0),
                widget.Net(foreground=colors["fg"],
                           format='{down} ↓↑ {up}'),
                widget.CPU(foreground=colors["fg"],
                           format='  {load_percent}%'),
                widget.ThermalSensor(foreground=colors["fg"],
                                     foreground_alert=colors["red_fg"],
                                     fmt=' {}'),
                widget.PulseVolume(foreground=colors["fg"],
                                   fmt='󰕾 {}',
                                   step=5),
                widget.Backlight(backlight_name='intel_backlight',
                                 foreground=colors["fg"],
                                 fmt='󰛨  {}',
                                 step=5),
                widget.UPowerWidget(foreground=colors["fg"],
                                    border_charge_colour=colors["green_fg"],
                                    border_colour=colors["fg"],
                                    border_critical_colour=colors["red_fg"],
                                    fill_charge=colors["fg"],
                                    fill_critical=colors["red_fg"],
                                    fill_low=colors["orange_fg"],
                                    fill_normal=colors["fg"],
                                    percentage_low=0.3,
                                    margin=4),
                widget.Battery(foreground=colors["fg"],
                               low_foreground=colors["red_fg"],
                               format='{percent:2.0%}',
                               padding=4,
                               unknown_char="",
                               notify_below=10,
                               notification_timeout=0),
            ],
            26,
            background=colors["bg"],
        ),
        wallpaper=os.path.expanduser('~/.config/wallpaper.png')
    ),
    Screen(wallpaper=os.path.expanduser('~/.config/wallpaper.png')),
]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
cursor_warp = True
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm
        # class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ],
    border_width=4,
    border_focus=colors["purple"],
    border_normal=colors["bg2"],
)
auto_fullscreen = False
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
