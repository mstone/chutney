set -eu
redo-ifchange "$1.orport" "$1.dirport" "$1.id" "dirservers" "torrc_common.sh"
. ./torrc_common.sh
cat <<EOF
SocksPort 0
OrPort $(cat "$1.orport")
Address 127.0.0.1
DirPort $(cat "$1.dirport")
AuthoritativeDirectory 1
V3AuthoritativeDirectory 1
ContactInfo auth$(cat "$1.id")@test.test
ExitPolicy reject *:*
EOF
cat dirservers
