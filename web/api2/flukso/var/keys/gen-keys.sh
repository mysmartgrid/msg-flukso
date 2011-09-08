#!/bin/bash
cd /home/flukso/www/api/flukso/var/keys

rm $1_id_dsa
dropbearkey -t dss -f $1_id_dsa >> /dev/null
dropbearkey -f $1_id_dsa -y | grep "^ssh-dss " > $1_id_dsa.pub 

chmod 600 $1*
chown flukso:flukso $1*
