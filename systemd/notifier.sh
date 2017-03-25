#!/usr/bin/env bash

journalctl --lines=0 --follow _PID=1 -o cat | while read line; do
    notify-send --expire-time=10000 "${line}"
done
