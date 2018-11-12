#!/bin/bash

GID=1000

pathtoname() {
    udevadm info -p /sys/"$1" | awk -v FS== '/DEVNAME/ {print $2}'
}

stdbuf -oL -- udevadm monitor --udev -s block | while read -r -- _ _ event devpath _; do
        if [ "$event" = add ]; then
            devname="$(pathtoname "$devpath")"
            unset TYPE
            source <(blkid "${devname}" -o export)
            echo "type ${devname} ${TYPE}"

            opts=""
            if [[ "${TYPE}" == *"fat" ]]; then
                opts="uid=1000,gid=1000"
            fi
            echo "mounting with opts ${opts}"

            systemd-mount --options="${opts}" --discover --timeout-idle-sec=3s "${devname}"
        fi
done
