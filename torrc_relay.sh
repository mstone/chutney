set -eu
redo-ifchange "$1.orport" "$1.dirport" "dirservers" "torrc_common.sh"
. ./torrc_common.sh
cat <<EOF
SocksPort 0
OrPort $(cat "$1.orport")
Address 127.0.0.1
DirPort $(cat "$1.dirport")
EOF
cat dirservers
