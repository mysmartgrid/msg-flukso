#!/bin/bash

if [ $# -ne 2 ]; then
  echo "Usage: gen-keys <host> <device id>"
  exit 1
fi

cd /var/www/flukso-api/flukso/var/keys/$1

#Generate the device key
rm $2_device*
dropbearkey -t dss -f $2_device_id >> /dev/null
dropbearkey -f $2_device_id -y | /bin/grep ssh-dss > $2_device_id.pub

#Generate the technician key
rm $2_tech*
ssh-keygen -t dsa -f ./$2_tech_id -q -N ""

#Key files - mode and ownership
chmod 600 $2*
chown www-data:www-data $2*
