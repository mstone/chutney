#!/bin/bash
set -eu
./torrc_common.sh
cat <<EOF
SocksPort $socksport
EOF
cat dirservers
