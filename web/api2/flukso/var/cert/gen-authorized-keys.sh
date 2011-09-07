#!/bin/bash

cd /home/flukso/www/api/flukso/var/cert

# Remove old keys
find . -name '*_dss*' -mtime +2 -exec rm {} \;

# Create new Authorized Keys File
files=('*.pub')
len=${#files[*]}
i=0

while [ $i -lt $len ]; do
	  
  filename=${files[$i]};
  cat $filename >> authorized_keys.new;
  let i++
done

ssh -i ./tech_id_dss root@localhost '/bin/cp --preserve=mode /home/flukso/www/api/flukso/var/cert/authorized_keys.new /home/support/.ssh/authorized_keys'

rm -f authorized_keys.new
