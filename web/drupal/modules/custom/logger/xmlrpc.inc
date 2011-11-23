<?php

/**
 * @file
 * Data and management plane xmlrpc methods for logging metering data alpha
 * version API
 *
 * Copyright (c) 2008-2009 jokamajo.org
 *               2010 Fraunhofer Institut ITWM (www.itwm.fraunhofer.de)
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

/**
 * Implements hook_xmlrpc().
 * Mapping external XML-RPC methods to callback functions.
 * API versioning; logger.flukso.net/xmlrpc.php maps to xmlrpc.inc
 */
function logger_xmlrpc() {
  return array(
    array(
      'logger.heartbeat', // External method name.
      'logger_heartbeat', // Drupal callback function to run.
      array('int', 'string', 'int', 'int', 'int'), // Return value's type, then any parameter types (upgrade, device, version, resets, uptime)
      'Send a heartbeat to the logger.', // Description.
    ),
    array(
      'logger.measurementAdd', // External method name.
      'logger_measurement_add', // Drupal callback function to run.
      array('string', 'array'), // Return value's type, then any parameter types (return, measurements)
      'Submit measurements to the logger.', // Description.
    ),
  );
}

function logger_heartbeat($device, $version, $resets, $uptime) {

  $dev = db_fetch_object(db_query("
    SELECT
      upgrade,
      resets
    FROM
      {logger_devices}
    WHERE
      device = '%s'", $device));

  $dev->resets += $resets;

  db_query("
    UPDATE
      {logger_devices}
    SET
      access = %d,
      version = %d,
      upgrade = %d,
      resets = %d,
      uptime = %d
    WHERE
      device = '%s'", REQUEST_TIME, $version, 0, $dev->resets, $uptime, $device);

  return $dev->upgrade;
}

function logger_measurement_add($logs) {

  require_once DRUPAL_ROOT . '/' . XMLRPC_PATH . '/' . XMLRPC_MODULE . "/logger.rrd.inc";

  $info = 'added 5min interval measurements to the log';

  foreach ($logs as $meter => $measurements) {

    //load the normalisation factor, relative to 1pulse = 1Wh
    $sql = "SELECT uid, device, factor FROM {logger_meters} WHERE meter = '%s'";
    $meterdata = db_fetch_object(db_query("SELECT uid, device, factor FROM {logger_meters} WHERE meter = :meter", array(':meter' => $meter)));

    if ($meterdata->uid < 5) { // only alpha users are allowed to call this API
      ksort($measurements); // sort the key-value pairs in the associative array by key, i.e. the timestamp

      foreach ($measurements as $timestamp => $value) {

        if (is_numeric($timestamp) and is_numeric($value)) {
          $values[$timestamp] = $value * $meterdata->factor;

        }
        else {
          watchdog_xmlrpc('logger.measurementAdd', 'corrupted input data for %meter : %timestamp : %value', array('%meter' => $meter, '%timestamp' => $timestamp, '%value' => $value), WATCHDOG_ERROR);
          notification_log_corrupted_message($meterdata->device);
        }
      }

      $return = logger_rrd_update($meter, $values);

      if ($return == 0) {
        watchdog_xmlrpc('logger.measurementAdd', 'successful update for base rrd: %meter', array('%meter' => $meter), WATCHDOG_NOTICE);

        $sql = "UPDATE {logger_meters} SET access = %d, value = %d WHERE meter = '%s'";
        // TODO Please review the conversion of this statement to the D7 database API syntax.
        /* db_query($sql, REQUEST_TIME, $value, $meter) */
        db_update('logger_meters')
  ->fields(array(
    'access' => REQUEST_TIME,
    'value' => $value,
  ))
  ->condition('meter', $meter)
  ->execute(); //Gets the latest value stored in $value
        //logger_meters is updated with the true metervalue $value, NOT $value*$meterdata->factor since we're not normalising this entry!

      }
      else {
        watchdog_xmlrpc('logger.measurementAdd', 'error updating base rrd. meter: %meter | return: %return ', array('%meter' => $meter, '%return' => $return), WATCHDOG_ERROR);
      }
    }
  }
  return $info;
}

