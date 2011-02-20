#!/bin/bash
set -eu
./torrc_common.sh
cat <<EOF
SocksPort 0
OrPort $orport
Address 127.0.0.1
DirPort $dirport
EOF
cat dirservers
