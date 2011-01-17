<?php
$config = "config = {
  IP      = \"$ip\",
  SENSOR  = \"$sensor\",
  TOKEN   = \"$token\",
  DATADIR = \"/tmp/flukso\",
  BINPATH = \"/mnt/storage/usr/bin/\",
  CMD     = \"flukso-getvalues\",
  CONSUMPTION = {
    LOW   = $low,
    MID   = $mid,
    HIGH  = $high
  }
}
";
?>
