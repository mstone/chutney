redo-ifchange "$1.nick" "$1.v3id" "$1.orport" "$1.addr" "$1.fp"
echo "DirServer $(cat $1.nick) v3ident=$(cat $1.v3id) orport=$(cat $1.orport) no-v2 $(cat $1.addr) $(cat $1.fp)"
