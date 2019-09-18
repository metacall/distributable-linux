#!/bin/sh

source $GUIX_PROFILE/etc/profile
~root/.config/guix/current/bin/guix-daemon --build-users-group=guixbuild &
exec "$@"
