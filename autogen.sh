#! /bin/sh

set -e

# We could also run "autoreconf -vif -I aclocal" but this is faster.
aclocal --force -I aclocal
autoconf --force -I aclocal
autoheader --force -I aclocal
