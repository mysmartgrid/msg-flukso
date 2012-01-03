<?php
$config = "config = {
  IP      = \"" . $params['ip'] . "\",
  PORT    = \"" . $params['port'] ."\",
  SERVICE = \"" . $params['service'] ."\",
  SENSOR  = \"" . $params['sensor'] . "\",
  TOKEN   = \"" . $params['token'] . "\",
  DATADIR = \"/tmp/flukso\",
  BINPATH = \"/mnt/storage/usr/bin/\",
  CMD     = \"flukso-getvalues\",
  CONSUMPTION = {
    LOW   = " . $params['low'] . ",
    MID   = " . $params['normal'] . ",
    HIGH  = " . $params['high'] . "
  }
}
";
?>
