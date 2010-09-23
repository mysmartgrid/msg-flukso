#!/bin/bash


####################
# Configuration

# Database connection information
dbname="flukso"
dbhost="localhost"
dbuser="flukso"
dbpw="jcRJ8aPxdeeN7JMm"

# Date/Time
datestamp=`date +'%m-%d-%Y'`

#Basedir
basedir=/home/flukso/backup

# Execution directory (script start point)
logfile=$basedir/backup.log

# Temporary Directory
tempdir=$basedir/$datestamp

# DB dump file
dbcontentfilename=dbcontent.sql

# Linux libraries list file
liblistfilename=lib-list.txt

# Backup TAR file name
tarfile=$basedir/mysmartgrid-backup-$datestamp.tar


####################
# Backup

starttime=`date`
echo "Beginning mySmartGrid backup - $starttime" > $logfile

# Create temporary working directory
echo "Creating temp working dir $tempdir" >> $logfile
/bin/rm -rf $tempdir
/bin/mkdir $tempdir
cd $tempdir

# sqldump database information
echo "Dumping drupal database, using user:$dbuser database:$dbname host:$dbhost" >> $logfile
mysqldump --user=$dbuser --host=$dbhost --password=$dbpw --add-drop-table $dbname > $dbcontentfilename

# Collecting list of libraries
/usr/bin/dpkg -l >> $liblistfilename

# TAR files
rm $tarfile*

echo "TARing $dbcontentfilename" >> $logfile
tar cf $tarfile ./$dbcontentfilename

echo "TARing $liblistfilename" >> $logfile
tar rf $tarfile ./$liblistfilename

echo "TARing files at /home" >> $logfile
cd /
tar rf $tarfile ./home --exclude ".$basedir/*.tar*"

echo "TARing files at /usr/local" >> $logfile
tar rf $tarfile ./usr/local

echo "TARing files at /etc" >> $logfile
tar rf $tarfile ./etc

#Compress file
gzip $tarfile


####################
# Cleanup

echo "Removing backup files older than 3 days" >> $logfile
find $basedir/*.tar.gz -mtime +3 -exec rm {} \;

echo "Removing temp dir $tempdir ..." >> $logfile
rm -rf $tempdir

endtime=`date`
echo "Backup completed - $endtime" >> $logfile

