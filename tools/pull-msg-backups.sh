#!/bin/bash

# Configuration
host=flukso@dev2.mysmartgrid.de
sourcedir=/home/flukso/backup
storagedir=/p/hpc/smartenergy/mysmartgrid-bkp

# Remove old files
/usr/bin/find $storagedir/* -mtime +90 -exec /bin/rm {} \;

# Pull backup files
/usr/bin/scp $host:$sourcedir/*.tar.gz $storagedir
/usr/bin/ssh $host "/bin/rm $sourcedir/*.tar.gz"

