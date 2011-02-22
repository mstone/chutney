set -eu
mkdir -p $(dirname $1)
case $1 in
  auths*)
		redo-ifchange torrc_authority.sh;
		. ./torrc_authority.sh;;
  relays*)
		redo-ifchange torrc_relay.sh;
		. ./torrc_relay.sh;;
  clients*)
		redo-ifchange torrc_client.sh;
		. ./torrc_client.sh;;
  *) exit 1;;
esac
