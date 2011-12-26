#!/bin/sh

#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
#
xrdb -merge ~/.Xdefaults
xmodmap ~/.Xmodmap
xset +fp /usr/share/fonts/local
xset fp rehash
xcompmgr -r -c -f -F -D 4 &
#exec gnome-session
#exec startkde
# exec startxfce4
#exec openbox-session 
# exec dwm
#exec awesome
#exec xmonad
nitrogen --restore &
exec ck-launch-session xmonad
#exec subtle
xsetroot -cursor_name left_ptr &
