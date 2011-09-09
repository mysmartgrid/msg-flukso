#!/bin/bash

cd /home/flukso/www/api/flukso/var/keys

# Remove old keys
find . -name '*_dsa*' -mtime +2 -exec rm {} \;

# Create new Authorized Keys File
files=('*_dsa.pub')
len=${#files[*]}
i=0

while [ $i -lt $len ]; do
	  
  filename=${files[$i]};
  cat $filename >> authorized_keys.new;
  let i++
done

ssh -i ./tech_id_dsa root@localhost '/bin/cp --preserve=mode /home/flukso/www/api/flukso/var/keys/authorized_keys.new /home/support/.ssh/authorized_keys'

rm -f authorized_keys.new
