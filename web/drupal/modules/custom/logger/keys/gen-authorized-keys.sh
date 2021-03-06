#!/bin/bash -x

if [ $# -ne 2 ]; then
  /bin/echo "Usage: gen-authorized-keys <host> <port>"
  exit 1
fi

cd /mnt/fhgfs/keys/$1
/usr/bin/touch authorized_keys.new

# Remove old keys
/usr/bin/find . -name '*device*' -mtime +2 -exec rm {} \;

# Create new Authorized Keys File
files=('*device_id.pub')
len=${#files[*]}
i=0

while [ $i -lt $len ]; do
	  
  filename=${files[$i]};
  /bin/cat $filename >> authorized_keys.new;
  let i++
done

#FIXME: Use JobQueue to deploy this file
/usr/bin/scp -p -i ./flukso_id -P $2 ./authorized_keys.new fluksoadm@$1:authorized_keys

/bin/rm -f authorized_keys.new
