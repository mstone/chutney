set -eu
redo-ifchange "$1.socksport" "dirservers" "torrc_common.sh"
. ./torrc_common.sh
cat <<EOF
SocksPort $(cat $1.socksport)
EOF
cat dirservers
