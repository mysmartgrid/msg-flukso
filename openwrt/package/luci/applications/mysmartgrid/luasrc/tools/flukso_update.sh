#!/bin/sh
# Simon Birnbach <simon.birnbach@itwm.fraunhofer.de>

cd `dirname "$0"`/..
PATH="$PATH:./tools"
FLUKSO_HOME="./flukso"
LUCI_HOME="."
SSH_OPT="-q -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no"
IP="192.168.255.254"

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

# Install the avahi daemon and copy the configuration file
echo "Install and configure the avahi daemon"
flukso_update.exp "ssh $SSH_OPT root@192.168.255.1 \"route add default gw $IP && echo 'Add default route to $IP'\""
flukso_update.exp "ssh $SSH_OPT root@192.168.255.1 \"echo 'nameserver 8.8.8.8' > /etc/resolv.conf && echo 'Use 8.8.8.8 as temporary nameserver'\""
flukso_update.exp "echo \"((opkg update > /dev/null 2>1 && opkg -V 0 install avahi-daemon > /dev/null 2>1 && echo 'Installation erfolgreich') || echo 'Installation fehlgeschlagen');ln -s /etc/init.d/avahi-daemon /etc/rc.d/S96avahi-daemon; exit \" | ssh $SSH_OPT root@192.168.255.1"
flukso_update.exp "scp $SSH_OPT ./tools/flukso.service root@192.168.255.1:/etc/avahi/services/"

# Copy the firewall.user file to disable the firewall for port 22 and 80 on the wireless interface
flukso_update.exp "scp $SSH_OPT ./tools/firewall.user root@192.168.255.1:/etc/"

# Use the reset button to return to the default network, wireless and firewall config
#flukso_update.exp "scp $SSH_OPT ./tools/system.tail root@192.168.255.1:/tmp/"
flukso_update.exp "scp $SSH_OPT ./tools/net_defaults root@192.168.255.1:/bin"
#grep="`flukso_update.exp "ssh $SSH_OPT root@192.168.255.1 \"grep -Z button /etc/config/system\""`"
flukso_update.exp "ssh $SSH_OPT root@192.168.255.1 \"sed -e 's:etc/init.d/network restart:net_defaults:' < /etc/config/system > /etc/config/system.new && mv /etc/config/system.new /etc/config/system && echo 'Use reset button to return to default settings'\""
