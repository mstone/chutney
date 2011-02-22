redo-ifchange "$1.dir" "$1.pass" "$1.gencert" "$1.lifetime" "$1.addr"
rm -f "$1.dir/identity_key" "$1.dir/authority_signing_key" "$1.dir/authority_certificate"
mkdir -p "$1.dir/keys"
cat "$1.pass" | (cd $(dirname $1); \
	./$(basename "$1.gencert") \
	--create-identity-key \
	-i "$(basename $1).dir/keys/identity_key" \
	-s "$(basename $1).dir/keys/authority_signing_key" \
	-c "$(basename $1).dir/keys/authority_certificate" \
	-m "$(cat $(basename $1.lifetime))" \
	-a "$(cat $(basename $1.addr))" \
	--passphrase-fd 0)
cp "$1.dir/keys/authority_certificate" $3
