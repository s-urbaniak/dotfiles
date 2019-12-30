export GDK_BACKEND=x11

xrdb -merge ~/.Xresources

dbus-update-activation-environment --systemd DBUS_SESSION_BUS_ADDRESS DISPLAY XAUTHORITY

#export SSH_AUTH_SOCK=$XDG_RUNTIME_DIR/gnupg/S.gpg-agent.ssh
#echo UPDATESTARTUPTTY | gpg-connect-agent

#exec dbus-launch --exit-with-session i3
exec dbus-launch --exit-with-session openbox
