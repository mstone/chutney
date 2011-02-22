redo-ifchange "$1.cert"
grep fingerprint "$1.cert" | cut -f2 -d" "
