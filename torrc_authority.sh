#!/bin/bash
set -eu
./torrc_common.sh
cat <<EOF
SocksPort 0
OrPort $orport
Address 127.0.0.1
DirPort $dirport
AuthoritativeDirectory 1
V3AuthoritativeDirectory 1
ContactInfo auth$id@test.test
ExitPolicy reject *:*
EOF
cat dirservers
