#!/bin/sh
[ "$@" ] && emacsclient -cne "(dired \"$@\")" -a "" || emacsclient -cne '(dired "~/")' -a ""
