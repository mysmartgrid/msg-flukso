#!/bin/bash
cd ~flukso/www/api/flukso/var/keys

rm $1_device*
dropbearkey -t dss -f $1_device_id >> /dev/null
dropbearkey -f $1_device_id -y | grep "^ssh-dss " > $1_device_id.pub 

chmod 600 $1*
chown flukso:flukso $1*
