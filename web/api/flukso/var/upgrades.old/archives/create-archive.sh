#!/bin/sh

if [ $# != 2 ]; then
  echo "Usage:"
  echo " create-archive.sh <device id> <current version>\n"
  exit 1
fi

device=$1
current="$2"
pending=0

home=/var/www/flukso-api/flukso/var/upgrades
archivesdir=$home/archives
tmpdir=$archivesdir/$device-tmp
rm -rf $tmpdir
mkdir $tmpdir

cp $archivesdir/install-versions.sh $tmpdir/install.sh

cd $home/versions
for version in $(ls); #Sorted list of versions
do
  if [ $version = $current ]; then
    pending=1
  else
    if [ $pending -eq 1 ]; then
      cp -r $version $tmpdir
    fi
  fi
done

if [ $pending -eq 1 ]; then
  cd $tmpdir
  tar -czvf archive.tar.gz *

  cat $archivesdir/archive-header.sh $tmpdir/archive.tar.gz > $archivesdir/$device
fi

cd ..
rm -rf $tmpdir

exit 0
