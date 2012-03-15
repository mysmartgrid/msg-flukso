#!/bin/bash

if [ $# -ne 2 ]; then
  /bin/echo "Usage: gen-keys <host> <device id>"
  exit 1
fi

cd /var/www/flukso-api/flukso/var/keys/$1

#Generate the device key
/bin/rm $2_device*
/usr/bin/dropbearkey -t dss -f $2_device_id >> /dev/null
/usr/bin/dropbearkey -f $2_device_id -y | /bin/grep ssh-dss > $2_device_id.pub

#Generate the technician key
/bin/rm $2_tech*
/usr/bin/ssh-keygen -t dsa -f ./$2_tech_id -q -N ""

#Key files - mode and ownership
/bin/chmod 600 $2*
/bin/chown www-data:www-data $2*
