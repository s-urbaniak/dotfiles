#!/bin/sh

PAGER=nobs
EDITOR=E
TERM=dumb

[ "$1" = -n ] && { export NAMESPACE=/tmp/ns.$USER.$2; mkdir -p "$NAMESPACE"; shift 2; }
9p ls plumb >/dev/null 2>/dev/null || plumber
9p ls font >/dev/null 2>/dev/null || fontsrv &

exec acme -a -f /mnt/font/SourceCodePro-Regular/13a/font
