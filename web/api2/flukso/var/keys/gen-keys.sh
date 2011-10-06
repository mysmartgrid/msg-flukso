#!/bin/bash

if [ $# -ne 1 ]; then
  echo "Usage: gen-keys <device id>"
  exit 1
fi

cd ~flukso/www/api/flukso/var/keys

#Generate the device key
rm $1_device*
dropbearkey -t dss -f $1_device_id >> /dev/null
dropbearkey -f $1_device_id -y | grep "^ssh-dss " > $1_device_id.pub 

#Generate the technician key
rm $1_tech*
ssh-keygen -t dsa -f ./$1_tech_id -q -N ""

#Key files - mode and ownership
chmod 600 $1*
chown flukso:flukso $1*
