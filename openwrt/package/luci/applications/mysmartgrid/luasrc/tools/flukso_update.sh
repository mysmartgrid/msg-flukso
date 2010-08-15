#!/bin/sh
# Simon Birnbach <simon.birnbach@itwm.fraunhofer.de>

cd `dirname "$0"`/..
PATH="$PATH:./tools"
FLUKSO_HOME="./flukso"
LUCI_HOME="."
SSH_OPT="-q -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"

# Some tests
grep "xmlrpcaddress = 'http://dev2-logger.mysmartgrid.de/xmlrpc'" < "$FLUKSO_HOME/flukso.lua" > /dev/null
if [ $? != 0 ]; then
	echo "flukso.lua contains the wrong xmlrpcaddress"
	exit 1
fi

grep "xmlrpcaddress = 'http://dev2-logger.mysmartgrid.de/xmlrpc'" < "$FLUKSO_HOME/heartbeat.lua" > /dev/null
if [ $? != 0 ]; then
	echo "heartbeat.lua contains the wrong xmlrpcaddress"
	exit 1
fi

# Copy the new Flukso scripts on the Flukso
echo "Copying the Flukso scripts..."
flukso_update.exp "scp $SSH_OPT \"$FLUKSO_HOME\"/*  root@192.168.255.1:/usr/share/lua/flukso/"
flukso_update.exp "ssh $SSH_OPT root@192.168.255.1 \"ln -s /tmp/sensor /www && echo 'Create sensor symlink...'\""

# Copy the msg_wizard on the Flukso
echo "Copying the msg_wizard..."
MODEL="\"$LUCI_HOME\"/model/cbi"
CONT="\"$LUCI_HOME\"/controller"
flukso_update.exp "scp $SSH_OPT $MODEL/msg_wizard.lua $MODEL/wlan.lua $MODEL/lan.lua $MODEL/tryit.lua root@192.168.255.1:/usr/lib/lua/luci/model/cbi/mini/"
flukso_update.exp "scp $SSH_OPT $CONT/msg_wizard.lua $CONT/wlan.lua $CONT/lan.lua $CONT/tryit.lua root@192.168.255.1:/usr/lib/lua/luci/controller/mini/"
flukso_update.exp "scp $SSH_OPT \"$LUCI_HOME/index.html\" root@192.168.255.1:/www/"
