redo-ifchange "$1.cert" "$1.tor"

(cd $1.dir; \
  echo "DirServer test 127.0.0.1:1 0000000000000000000000000000000000000000" > torrc.tmp; \
  echo "OrPort 1" >> torrc.tmp; \
	"../$(basename $1.tor)" \
	--quiet \
	--list-fingerprint \
	--DataDirectory . \
	-f torrc.tmp \
| cut -f2- -d' ' \
| sed -e 's/ //g')
