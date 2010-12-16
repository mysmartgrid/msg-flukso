#!/bin/sh

#Parameters
BASE_DIR=~/www/public/sites/all/modules/logger/data/base
NIGHT_DIR=~/www/public/sites/all/modules/logger/data/night


cd $BASE_DIR;

files=(`ls *.rrd`)
len=${#files[*]}

i=0
while [ $i -lt $len ]; do

  filename=${files[$i]};

  ~/www/public/sites/all/modules/logger/rrdtool resize $filename 0 GROW 9960
  mv "resize.rrd" $filename
  ~/www/public/sites/all/modules/logger/rrdtool resize $filename 1 GROW 2688
  mv "resize.rrd" $filename
  ~/www/public/sites/all/modules/logger/rrdtool resize $filename 2 GROW 120
  mv "resize.rrd" $filename

  let i++
done


cd $NIGHT_DIR;

files=(`ls *.rrd`)
len=${#files[*]}

i=0
while [ $i -lt $len ]; do

  filename=${files[$i]};

  ~/www/public/sites/all/modules/logger/rrdtool resize $filename 0 GROW 120
  mv "resize.rrd" $filename

  let i++
done

cd ~/www/public/sites/all/modules/logger