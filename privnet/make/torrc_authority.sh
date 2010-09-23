#!/bin/bash
set -eu
./torrc_common.sh
cat <<EOF
SocksPort 0
OrPort $ORPORT
Address 127.0.0.1
DirPort $DIRPORT
AuthoritativeDirectory 1
V3AuthoritativeDirectory 1
ContactInfo auth$NUM@test.test
ExitPolicy reject *:*
EOF
cat dirservers
