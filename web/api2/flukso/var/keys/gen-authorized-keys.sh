#!/bin/bash

if [ $# -ne 2 ]; then
  echo "Usage: gen-authorized-keys <host> <username>"
  exit 1
fi

cd ~flukso/www/api/flukso/var/keys/$1

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

#FIXME: Use JobQueue, and do not use "root"
scp -p -i ./flukso_id ./authorized_keys.new root@$1:/home/$2/.ssh/authorized_keys

rm -f authorized_keys.new
