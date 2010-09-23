#!/bin/bash
set -eu
./torrc_common.sh
cat <<EOF
SocksPort 0
OrPort $ORPORT
Address 127.0.0.1
DirPort $DIRPORT
EOF
cat dirservers
