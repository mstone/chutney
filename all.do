redo-ifchange types nodes
redo-ifchange $(for n in $(cat nodes); do echo "$n.run"; done) start stop hup log
