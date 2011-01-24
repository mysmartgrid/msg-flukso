<?php

/**
 * Data and management plane xmlrpc methods for logging metering
 * data API version 1.
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
 * API versioning; logger.flukso.net/xmlrpc/1 maps to xmlrpc1.inc
 */
function logger_xmlrpc() {
  return array(
    array(
      'logger.auth',            // External method name.
      '_logger_auth',           // Drupal callback function to run + api version 1
      array('string', 'array'), // Return value's type, then any parameter types (accept, auth)
      'Authenticate a device'   // Description.
    ),

    array(
      'logger.heartbeat',               // External method name.
      '_logger_heartbeat',              // Drupal callback function to run  + api version 1
      array('array', 'array', 'array'), // Return value's type, then any parameter types (return, auth, monitor)
      'Send a heartbeat to the logger.' // Description.
    ),

    array(
      'logger.measurementAdd',             // External method name.
      '_logger_measurement_add',           // Drupal callback function to run + api version 1
      array('string', 'array', 'array'),   // Return value's type, then any parameter types (return, auth, logs)
      'Submit measurements to the logger.' // Description.
    ),

    array(
      'logger.brownout',                       // External method name.
      '_logger_brownout',                      // Drupal callback function to run  + api version 1
      array('string', 'array', 'array'),       // Return value's type, then any parameter types (return, auth, message)
      'Send a brownout message to the logger.' // Description.
    ),
  );
}

/**
 * Callback functions registered in the logger_xmlrpc section
 */
function _logger_auth($auth) {

  if (_logger_authenticate_hmac_sha1($auth)) {
    return 'authenticated!';
  
  } else {
    return xmlrpc_error(-31000, t('Authentication failed.'));
  }
}

function _logger_heartbeat($auth, $monitor) {

  if (_logger_authenticate_hmac_sha1($auth, $monitor)) {

    $sql = "
      SELECT
        sha,
        upgrade,
        resets
      FROM
        {logger_devices}
      WHERE
        device = '%s'";
    
    $device = db_fetch_object(db_query($sql, $auth['device']));
    $device->resets += $monitor['reset'];

    $sql = "
      UPDATE
        {logger_devices}
      SET
        access = %d,
        version = %d,
        upgrade = %d,
        resets = %d,
        uptime = %d,
        memtotal = %d,
        memfree = %d,
        memcached = %d,
        membuffers = %d,
        uart_oe = %d
      WHERE
        device = '%s'";

    db_query($sql, time(),
      $monitor['version'], 0, $device->resets, $monitor['uptime'],
      $monitor['memtotal'], $monitor['memfree'], $monitor['memcached'],
      $monitor['membuffers'], $monitor['uart_oe'], $auth['device']);

    $action['upgrade'] = (int) $device->upgrade;
    if ($action['upgrade']) {

      $action['timestamp'] = time();
      $action['signature'] = hash_hmac('sha1',
          $action['timestamp'] . ':' . $action['upgrade'] .':'.
          $device->sha, $device->sha);
    }

    watchdog_xmlrpc('logger.heartbeat', 'heardbeat received: %device', array('%device' => $auth['device']), WATCHDOG_NOTICE);

    return $action;

  } else {
    return xmlrpc_error(-31000, t('Authentication failed.'));
  }
}

function _logger_measurement_add($auth, $logs) {

  require_once(XMLRPC_PATH . '/' . XMLRPC_MODULE . "/logger.rrd.inc");

  if (_logger_authenticate_hmac_sha1($auth, $logs)) {

    $info = 'added 5min interval measurements to the log';

    foreach ($logs as $meter => $measurements) {

      //load the normalisation factor, relative to 1pulse = 1Wh
      $sql = "SELECT device, night, factor FROM {logger_meters} WHERE meter = '%s'";
      $meterdata = db_fetch_object(db_query($sql, $meter));

      if ($meterdata->device == $auth['device']) { // extra security check

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
          watchdog_xmlrpc('logger.measurementAdd', 'successful update of base rrd. meter: %meter', array('%meter' => $meter), WATCHDOG_NOTICE);

          //update the night rrd every day at 6AM local time
          if (time() > $meterdata->night) {

            $midnight = _logger_midnight(time(), $auth['device']);
            $start = $midnight + 2 * HOUR; // 2AM local time
            $end = $start + 3 * HOUR; // 3h time interval

            $return = logger_rrd_night_update($meter, $start, $end);

            if ($return == 0) {
              watchdog_xmlrpc('logger.measurementAdd', 'successful update for night rrd: %meter | midnight = %midnight', array('%meter' => $meter, '%midnight' => $midnight), WATCHDOG_NOTICE);

            } else {
              watchdog_xmlrpc('logger.measurementAdd', 'error updating night rrd: %meter | return: %return ', array('%meter' => $meter, '%return' => $return), WATCHDOG_ERROR);
            }
            $meterdata->night = $midnight + 30 * HOUR; //add an offset of 30h = 6AM local time
          }

          $sql = "UPDATE {logger_meters} SET access = %d, night = %d, value = %d WHERE meter = '%s'";
          db_query($sql, time(), $meterdata->night, $value, $meter);
          //logger_meters is updated with the true metervalue $value, NOT $value*$meterdata->factor since we're not normalising this entry!

        } else {
          watchdog_xmlrpc('logger.measurementAdd', 'error updating base rrd. meter: %meter | return: %return ', array('%meter' => $meter, '%return' => $return), WATCHDOG_ERROR);
        }
      }
    }

    return $info;

  } else {

    return xmlrpc_error(-31000, t('Authentication failed.'));
  }
}

function _logger_brownout($auth, $message) {

  require_once(XMLRPC_PATH . "/notification/notification.module");

  if (_logger_authenticate_hmac_sha1($auth, $message)) {

    notification_log_brownout($auth['device']);

    return 'Brownout message has been received.';

  } else {

    return xmlrpc_error(-31000, t('Authentication failed.'));
  }
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
 * Calculate the most recent midnight for this device based on the user's timezone
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
  $midnight = floor($timestamp / DAY + 1) * DAY - $timezone;

  // search for first $midnight < $timestamp
  while ($midnight > $timestamp) {
    $midnight -= 86400;
  }

  return $midnight;
}

function _logger_authenticate_hmac_sha1($auth, $message) {

  $sql = "SELECT sha FROM {logger_devices} WHERE device = '%s'";
  $auth['key'] = db_result(db_query($sql, $auth['device']));

  if (hash_hmac('sha1', $auth['timestamp'] .':'. _logger_serialise($message) .':'. $auth['key'], $auth['key']) == $auth['signature'] && $auth['timestamp'] > time() - 300) {
    //watchdog_xmlrpc('logger.auth', 'HMAC-SHA1 authentication succeeded for device: %device', array('%device' => $auth['device']), WATCHDOG_NOTICE);
    return TRUE;

  } else {

    watchdog_xmlrpc('logger.auth', 'HMAC-SHA1 authentication failed for device: %device', array('%device' => $auth['device']), WATCHDOG_ERROR);
    return FALSE;
  }
}

function _logger_serialise($data) {

  if (is_array($data)) {

    $sequence = '';
    foreach ($data as $key => $value) {
      $sequence .= (string)$key . _logger_serialise($value);
    }
    return $sequence;

  } else {
    return (string)$data;
  }
}