<?php
$config = "config = {
  IP      = \"" . $params['ip'] . "\",
  PORT    = \"" . $params['port'] ."\",
  SENSOR  = \"" . $params['sensor'] . "\",
  SERVICE = \"" . $params['service'] ."\",
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
