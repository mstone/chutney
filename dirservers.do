redo-ifchange nodes
NODES=$(grep auths nodes)
redo-ifchange $(for N in $NODES; do echo $N.dirserver; done)
cat $(for N in $NODES; do echo $N.dirserver; done)
