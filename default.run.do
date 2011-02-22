redo-ifchange "$1.tor" "$1.torrc" "$1.torflags" "$1.dir"
echo "#!/bin/sh" > $3
echo "cd \${0%run}dir" >> $3
echo "../$(basename $1.tor) $(cat $1.torflags) -f $(realpath $1.torrc)" >> $3
chmod a+x $3
