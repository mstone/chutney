set -eu
redo-ifchange "$1.dir" "$1.connlimit" "$1.nick" "$1.controlport"
cat <<EOF
TestingTorNetwork 1
DataDirectory .
RunAsDaemon 1
ConnLimit $(cat $1.connlimit)
Nickname $(cat $1.nick)
ShutdownWaitLength 0
PidFile pid
Log notice file $(basename $1.log)
ControlPort $(cat $1.controlport)
EOF
