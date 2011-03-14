<?php

/**
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
 * Implementation of hook_xmlrpc().
 * Mapping external XML-RPC methods to callback functions.
 * API versioning; logger.flukso.net/xmlrpc.php maps to xmlrpc.inc
 */
function logger_xmlrpc() {
  return array(
    array(
      'logger.heartbeat', // External method name.
      '_logger_heartbeat', // Drupal callback function to run.
      array('int', 'string', 'int', 'int', 'int'), // Return value's type, then any parameter types (upgrade, device, version, resets, uptime)
      'Send a heartbeat to the logger.' // Description.
      ),

    array(
      'logger.measurementAdd', // External method name.
      '_logger_measurement_add', // Drupal callback function to run.
      array('string', 'array'), // Return value's type, then any parameter types (return, measurements)
      'Submit measurements to the logger.' // Description.
      ),
  );
}

function _logger_heartbeat($device, $version, $resets, $uptime) {

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
      device = '%s'", time(), $version, 0, $dev->resets, $uptime, $device);

  return $dev->upgrade;
}

function _logger_measurement_add($logs) {

  require_once(XMLRPC_PATH . '/' . XMLRPC_MODULE . "/logger.rrd.inc");

  $info = 'added 5min interval measurements to the log';

  foreach ($logs as $meter => $measurements) {

    //load the normalisation factor, relative to 1pulse = 1Wh
    $sql = "SELECT uid, night, factor FROM {logger_meters} WHERE meter = '%s'";
    $meterdata = db_fetch_object(db_query($sql, $meter));

    if ($meterdata->uid < 5) { // only alpha users are allowed to call this API

      ksort($measurements); // sort the key-value pairs in the associative array by key, i.e. the timestamp

      $corrupted_time = 0;

      foreach ($measurements as $timestamp => $value) {

        if (is_numeric($timestamp) and is_numeric($value)) {
          $values[$timestamp] = $value * $meterdata->factor;

        } else {
          watchdog_xmlrpc('logger.measurementAdd', 'corrupted input data for %meter : %timestamp : %value', array('%meter' => $meter, '%timestamp' => $timestamp, '%value' => $value), WATCHDOG_ERROR);
          $corrupted_time = time();
        }
      }

      _logger_update_meter_integrity($meter, $corrupted_time);

      $return = logger_rrd_update($meter, $values);

      if ($return == 0) {
          watchdog_xmlrpc('logger.measurementAdd', 'successful update for base rrd: %meter', array('%meter' => $meter), WATCHDOG_NOTICE);

        // update the night rrd every day at 6AM local time
        if (time() > $meterdata->night) {

          $midnight = _logger_midnight(time(), $auth['device']);
          $start = $midnight + 7200; // 2AM local time
          $end = $start + 10800; // 3h time interval

          $return = logger_rrd_night_update($meter, $start, $end);

          if ($return == 0) {
            watchdog_xmlrpc('logger.measurementAdd', 'successful update for night rrd: %meter | midnight = %midnight', array('%meter' => $meter, '%midnight' => $midnight), WATCHDOG_NOTICE);

          } else {
            watchdog_xmlrpc('logger.measurementAdd', 'error updating night rrd: %meter | return: %return ', array('%meter' => $meter, '%return' => $return), WATCHDOG_ERROR);
          }
          
          //add an offset of 30h = 6AM local time
          $meterdata->night = $midnight + 108000;
        }

        $sql = "UPDATE {logger_meters} SET access = %d, night = %d, value = %d WHERE meter = '%s'";
        db_query($sql, time(), $meterdata->night, $value, $meter);//Gets the latest value stored in $value
        //logger_meters is updated with the true metervalue $value, NOT $value*$meterdata->factor since we're not normalising this entry!

      } else {
        watchdog_xmlrpc('logger.measurementAdd', 'error updating base rrd. meter: %meter | return: %return ', array('%meter' => $meter, '%return' => $return), WATCHDOG_ERROR);
      }
    }
  }
  return $info;
}

/**
 * Updates the meter integrity flag.
 *
 * @param $meter            The meter being updated.
 * @param $corruption_time  The time when a corrupted heartbeat was received.
 *                          The value 0 means that the heartbeat was sound.
 */
function _logger_update_meter_integrity($meter, $corruption_time) {

  $sql = "UPDATE {logger_meters} SET corrupted = %d WHERE meter = '%s'";
  db_query($sql, $corruption_time, $meter);
}

/**
 * Calculates the most recent midnight for this device based on the user's timezone
 */
function _logger_midnight($timestamp, $device) {

  $timezone = db_result(db_query("
    SELECT
      u.timezone
    FROM
      {logger_devices} ld
      INNER JOIN {users} u ON ld.uid = u.uid
    WHERE
      ld.device = '%s'", $device));

  // add one day to make sure $midnight > $timestamp
  $midnight = floor($timestamp/86400 + 1)*86400 - $timezone;

  // search for first $midnight < $timestamp
  while ($midnight > $timestamp) {
    $midnight -= 86400;
  }

  return $midnight;
}