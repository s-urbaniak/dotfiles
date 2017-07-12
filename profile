logger "invoked .profile \$0=\"${0}\" \$DESKTOP_SESSION=\"${DESKTOP_SESSION}\""
if [ "$0" = "/etc/gdm/Xsession" -a "$DESKTOP_SESSION" = "i3" ]; then
    logger "detected gdm Xsession and i3, exporting gnome keyring daemon settings"
    export $(gnome-keyring-daemon -s)
fi

export PATH="$HOME/.cargo/bin:$PATH"
