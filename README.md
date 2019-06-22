# dotfiles

I've organized my dotfiles into 2 directores `user` and `system`. Under each, I keep the same structure as they go on the home directory and root respectively. These get set up using symlinks to make sure I remember to check-in any changes.

These dotfiles are mainly focused around using systemd to manage user processes and using the following tools:

```
i3 (gaps)
polybar
compton
terminator
rofi
dunst
pyenv
zsh
tmux
Google Play Media Desktop Player
git
powerline
```

I also use a variety of tools, scripts, etc to make things work the way I want to. Some scripts I have here:

```shell
# systemd related (most of these have a corresponding .service file)
start_systemd    # ensures necessary ENV vars are set and starts my main user target to which everything is bound
watch_battery    # notify me when battery drops too low
watch_fullscreen # start a fullscreen.target when a window is full screen (used to prevent xautolock)
watch_sleep      # used to start a user level sleep.target (used to lock screen on suspend)
watch_unlock     # used to start a user level target to know if the system is unlocked or not (not currently used)
lock_screen      # used to actually lock screen. gives me a centralized location to change the locking app
set_wallpaper    # set the wallpaper for i3 sessions

# headset/bluetooth related
connect_headset  # currently hardcoded since I only use 1 headset
disconnect_headset # same as above
reset_bluetooth  # re-set bluetooth when things act up
switch_headset   # switch the headset between a2dp and heaset modes (useful for taking video calls)
a2dp-fix         # script to make sure headset works with a2dp when I connect it
a2dp-fix-wrapper # accompanies above script so it can be called by udev rule

# media related
volume           # changes volume only for the currently active pulseaudio sinks
songliker        # like the currently playing song on GPMDP
raiseplayer      # raise the GPMDP player (no longer used since I switched to i3)
now_playing_gpmdp/lastfm # scripts to print out the currently playing song to polybar

# emacs
editor           # start emacsclient using the currently running emacs server. use terminal or X depending on env
sudo_editor      # same as above, but for files that need sudo to edit

# android
enable_helium_carbon # self explanatory
fix_airplane_mode    # make airplane mode only deactivate cell signal (why disable bluetooth and wifi?)

# other
focuswindow      # script to focus a specific window given a name. was used to setting key shortucts, but not used since i3
flip_text        # flip a string of text upside down... because, why not?
todoist_task     # raise a little GTK window with a text prompt to add a new task to my Todoist inbox, used with key shortcut
battery-combined-udev.sh # script to combine battery percentage for polybar when multiple batteries are present (no longer used)
```
