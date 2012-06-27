#!/bin/sh
dir=firmware-upgrade
rm -rf $dir
mkdir $dir
tail -n +12 $0 | tar -xzv -C ./$dir
cd $dir
./install.sh
rc=$?
cd ..
rm -rf ./$dir
exit $rc
