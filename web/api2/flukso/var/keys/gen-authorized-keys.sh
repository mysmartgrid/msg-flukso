#!/bin/bash

cd ~flukso/www/api/flukso/var/keys

# Remove old keys
find . -name '*device*' -mtime +2 -exec rm {} \;

# Create new Authorized Keys File
files=('*device_id.pub')
len=${#files[*]}
i=0

while [ $i -lt $len ]; do
	  
  filename=${files[$i]};
  cat $filename >> authorized_keys.new;
  let i++
done

#TODO: in the future, this file will be copied to the support machine
ssh -i ./tech_id root@localhost '/bin/cp --preserve=mode ~flukso/www/api/flukso/var/keys/authorized_keys.new ~support/.ssh/authorized_keys'

rm -f authorized_keys.new
