redo-ifchange "$1.tordir"
echo "#!/bin/sh" > $3
echo "exec $(cat $1.tordir)/src/tools/tor-gencert \"\$@\"" >> $3
chmod a+x $3
