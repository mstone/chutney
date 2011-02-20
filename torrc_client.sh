#!/bin/bash
set -eu
./torrc_common.sh
cat <<EOF
SocksPort $SOCKSPORT
EOF
cat dirservers
