#!/bin/bash
set -eu
cat <<EOF
TestingTorNetwork 1
DataDirectory $DIR
RunAsDaemon 1
ConnLimit $CONNLIMIT
Nickname $NICK
ShutdownWaitLength 0
PidFile pid
Log notice file $DIR/notice.log
ControlPort $CONTROLPORT
EOF
