# i3 config file (v4) -*- conf -*-
#
# Please see https://i3wm.org/docs/userguide.html for a complete reference!

set $mod Mod4

# Font for window titles
font pango:DejaVu Sans 12

# Use Mouse+$mod to drag floating windows to their wanted position
floating_modifier $mod

# default layout
workspace_layout tabbed

# start a terminal
bindsym $mod+Return exec urxvtc

# kill focused window
bindsym $mod+Shift+a kill

# menus
## launch application
bindsym $mod+d exec rofi -show drun
## switch to window
bindsym $mod+o exec rofi -show window

# change focus
bindsym $mod+h focus left
bindsym $mod+j focus down
bindsym $mod+k focus up
bindsym $mod+l focus right

# alternatively, you can use the cursor keys:
bindsym $mod+Left focus left
bindsym $mod+Down focus down
bindsym $mod+Up focus up
bindsym $mod+Right focus right

# move focused window
bindsym $mod+Shift+h move left
bindsym $mod+Shift+j move down
bindsym $mod+Shift+k move up
bindsym $mod+Shift+l move right

# alternatively, you can use the cursor keys:
bindsym $mod+Shift+Left move left
bindsym $mod+Shift+Down move down
bindsym $mod+Shift+Up move up
bindsym $mod+Shift+Right move right

# split in horizontal orientation
bindsym $mod+b split h

# split in vertical orientation
bindsym $mod+v split v

# enter fullscreen mode for the focused container
bindsym $mod+f fullscreen toggle

# change container layout (stacked, tabbed, toggle split)
bindsym $mod+s layout stacking
bindsym $mod+z layout tabbed
bindsym $mod+e layout toggle split

# toggle tiling / floating
bindsym $mod+Shift+space floating toggle

# change focus between tiling / floating windows
bindsym $mod+space focus mode_toggle

# focus the parent/child container
bindsym $mod+q focus parent
bindsym $mod+w focus child

# Define names for default workspaces for which we configure key bindings later on.
# We use variables to avoid repeating the names in multiple places.
set $ws1 "1:🌍"
set $ws2 "2:📬"
set $ws3 "3:✍️"
set $ws4 "4:📚"
set $ws5 "5"
set $ws6 "6"
set $ws7 "7"
set $ws8 "8"
set $ws9 "9"
set $ws10 "10"

# switch to workspace
bindsym $mod+ampersand workspace $ws1
bindsym $mod+eacute workspace $ws2
bindsym $mod+quotedbl workspace $ws3
bindsym $mod+apostrophe workspace $ws4
bindsym $mod+parenleft workspace $ws5
bindsym $mod+minus workspace $ws6
bindsym $mod+egrave workspace $ws7
bindsym $mod+underscore workspace $ws8
bindsym $mod+ccedilla workspace $ws9
bindsym $mod+agrave workspace $ws10

# move focused container to workspace
bindsym $mod+Shift+ampersand move container to workspace $ws1
bindsym $mod+Shift+eacute move container to workspace $ws2
bindsym $mod+Shift+quotedbl move container to workspace $ws3
bindsym $mod+Shift+apostrophe move container to workspace $ws4
bindsym $mod+Shift+5 move container to workspace $ws5
bindsym $mod+Shift+minus move container to workspace $ws6
bindsym $mod+Shift+egrave move container to workspace $ws7
bindsym $mod+Shift+underscore move container to workspace $ws8
bindsym $mod+Shift+ccedilla move container to workspace $ws9
bindsym $mod+Shift+agrave move container to workspace $ws10

# reload the configuration file
bindsym $mod+Shift+c reload
# restart i3 inplace (preserves your layout/session, can be used to upgrade i3)
bindsym $mod+Shift+r restart
# exit i3 (logs you out of your X session)
bindsym $mod+Shift+e exec "i3-nagbar -t warning -m 'You pressed the exit shortcut. Do you really want to exit i3? This will end your X session.' -b 'Yes, exit i3' 'i3-msg exit'"

# resize window (you can also use the mouse for that)
mode "resize" {
        # These bindings trigger as soon as you enter the resize mode

        # Pressing left will shrink the window’s width.
        # Pressing right will grow the window’s width.
        # Pressing up will shrink the window’s height.
        # Pressing down will grow the window’s height.
        bindsym h resize shrink width 10 px or 10 ppt
        bindsym j resize grow height 10 px or 10 ppt
        bindsym k resize shrink height 10 px or 10 ppt
        bindsym l resize grow width 10 px or 10 ppt

        # same bindings, but for the arrow keys
        bindsym Left resize shrink width 10 px or 10 ppt
        bindsym Down resize grow height 10 px or 10 ppt
        bindsym Up resize shrink height 10 px or 10 ppt
        bindsym Right resize grow width 10 px or 10 ppt

        # back to normal: Enter or Escape or $mod+r
        bindsym Return mode "default"
        bindsym Escape mode "default"
        bindsym $mod+r mode "default"
}
bindsym $mod+r mode "resize"

# volume
bindsym XF86AudioLowerVolume exec --no-startup-id pactl set-sink-volume 0 -2%
bindsym XF86AudioRaiseVolume exec --no-startup-id pactl set-sink-volume 0 +2%
bindsym XF86AudioMute exec --no-startup-id pactl set-sink-mute 0 toggle

# lock screen
bindsym $mod+Ctrl+l exec --no-startup-id xautolock -locknow
bindsym XF86Sleep exec --no-startup-id xautolock -locknow
exec --no-startup-id exec xautolock -time 5 -locker 'i3lock -f -e -i ~/nextcloud/img/hal.png' -notify 15 -notifier 'notify-send -u low "Locking..."' &

# assign windows to workspaces
assign [class="^Firefox$"] $ws1
assign [class="^Thunderbird$"] $ws2
assign [class="^Emacs$"] $ws3
assign [class="^okular$"] $ws4

# Start i3bar to display a workspace bar
bar {
    font pango:DejaVu Sans 16
    tray_output primary
    tray_padding 0
    status_command i3status
    strip_workspace_numbers yes
    separator_symbol "|"
}

# Starting things with i3
exec --no-startup-id compton -C &
exec --no-startup-id feh --bg-scale ~/nextcloud/img/wallpaper.jpg &
exec --no-startup-id urxvtd -q -o -f &
