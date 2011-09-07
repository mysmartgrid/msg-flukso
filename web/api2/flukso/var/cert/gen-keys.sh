#!/bin/bash
cd /home/flukso/www/api/flukso/var/cert

rm $1_dss
dropbearkey -t dss -f $1_dss >> /dev/null
dropbearkey -f $1_dss -y | grep "^ssh-dss " > $1_dss.pub 

chmod 600 $1*
chown flukso:flukso $1*
