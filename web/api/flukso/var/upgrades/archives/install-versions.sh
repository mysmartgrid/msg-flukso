#!/bin/sh
for version in $(ls -p | grep "/");
do
  cd $version
  ./install.sh
  rc=$?
  if [[ $rc != 0 ]]; then
    exit $rc
  fi
  cd ..
done
exit 0
