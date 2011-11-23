<?php

/**
 * @file
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
 * Implements hook_xmlrpc().
 * Mapping external XML-RPC methods to callback functions.
 * API versioning; logger.flukso.net/xmlrpc/1 maps to xmlrpc1.inc
 */
function logger_xmlrpc() {
  return array(
    array(
      'logger.auth', // External method name.
      'logger_auth', // Drupal callback function to run + api version 1
      array('string', 'array'), // Return value's type, then any parameter types (accept, auth)
      'Authenticate a device'   , // Description.
    ),
    array(
      'logger.heartbeat', // External method name.
      'logger_heartbeat', // Drupal callback function to run  + api version 1
      array('array', 'array', 'array'), // Return value's type, then any parameter types (return, auth, monitor)
      'Send a heartbeat to the logger.', // Description.
    ),
    array(
      'logger.measurementAdd', // External method name.
      'logger_measurement_add', // Drupal callback function to run + api version 1
      array('string', 'array', 'array'), // Return value's type, then any parameter types (return, auth, logs)
      'Submit measurements to the logger.', // Description.
    ),
    array(
      'logger.brownout', // External method name.
      'logger_brownout', // Drupal callback function to run  + api version 1
      array('string', 'array', 'array'), // Return value's type, then any parameter types (return, auth, message)
      'Send a brownout message to the logger.', // Description.
    ),
  );
}

/**
 * Callback functions registered in the logger_xmlrpc section
 */
function logger_auth($auth) {

  if (logger_authenticate_hmac_sha1($auth)) {
    return 'authenticated!';

  }
  else {
    return xmlrpc_error(-31000, t('Authentication failed.'));
  }
}

function logger_heartbeat($auth, $monitor) {

  require_once DRUPAL_ROOT . '/' . XMLRPC_PATH . "/notification/notification.module";

  if (logger_authenticate_hmac_sha1($auth, $monitor)) {

    notification_log_heartbeat_received($auth['device']);

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

    db_query($sql, REQUEST_TIME, $monitor['version'], 0, $device->resets, $monitor['uptime'], $monitor['memtotal'], $monitor['memfree'], $monitor['memcached'], $monitor['membuffers'], $monitor['uart_oe'], $auth['device']);

    $action['upgrade'] = (int) $device->upgrade;
    if ($action['upgrade']) {

      $action['timestamp'] = REQUEST_TIME;
      $action['signature'] = hash_hmac('sha1',
          $action['timestamp'] . ':' . $action['upgrade'] . ':' .
          $device->sha, $device->sha);
    }

    watchdog_xmlrpc('logger.heartbeat', 'heardbeat received: %device', array('%device' => $auth['device']), WATCHDOG_NOTICE);

    return $action;

  }
  else {
    return xmlrpc_error(-31000, t('Authentication failed.'));
  }
}

function logger_measurement_add($auth, $logs) {

  require_once DRUPAL_ROOT . '/' . XMLRPC_PATH . '/' . XMLRPC_MODULE . "/logger.rrd.inc";
  require_once DRUPAL_ROOT . '/' . XMLRPC_PATH . "/notification/notification.module";

  if (logger_authenticate_hmac_sha1($auth, $logs)) {

    notification_log_measurement_received($auth['device']);

    $info = 'added 5min interval measurements to the log';

    foreach ($logs as $meter => $measurements) {

      //load the normalisation factor, relative to 1pulse = 1Wh
      $sql = "SELECT device, factor FROM {logger_meters} WHERE meter = '%s'";
      $meterdata = db_fetch_object(db_query("SELECT device, factor FROM {logger_meters} WHERE meter = :meter", array(':meter' => $meter)));

      if ($meterdata->device == $auth['device']) { // extra security check
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
          watchdog_xmlrpc('logger.measurementAdd', 'successful update of base rrd. meter: %meter', array('%meter' => $meter), WATCHDOG_NOTICE);

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
  else {

    return xmlrpc_error(-31000, t('Authentication failed.'));
  }
}

function logger_brownout($auth, $message) {

  require_once DRUPAL_ROOT . '/' . XMLRPC_PATH . "/notification/notification.module";

  if (logger_authenticate_hmac_sha1($auth, $message)) {

    notification_log_brownout($auth['device']);

    return 'Brownout message has been received.';

  }
  else {

    return xmlrpc_error(-31000, t('Authentication failed.'));
  }
}

function logger_authenticate_hmac_sha1($auth, $message) {

  $sql = "SELECT sha FROM {logger_devices} WHERE device = '%s'";
  $auth['key'] = db_query("SELECT sha FROM {logger_devices} WHERE device = :device", array(':device' => $auth['device']))->fetchField();

  if (hash_hmac('sha1', $auth['timestamp'] . ':' . logger_serialise($message) . ':' . $auth['key'], $auth['key']) == $auth['signature'] && $auth['timestamp'] > REQUEST_TIME - 300) {
    //watchdog_xmlrpc('logger.auth', 'HMAC-SHA1 authentication succeeded for device: %device', array('%device' => $auth['device']), WATCHDOG_NOTICE);
    return TRUE;

  }
  else {

    watchdog_xmlrpc('logger.auth', 'HMAC-SHA1 authentication failed for device: %device', array('%device' => $auth['device']), WATCHDOG_ERROR);
    return FALSE;
  }
}

function logger_serialise($data) {

  if (is_array($data)) {

    $sequence = '';
    foreach ($data as $key => $value) {
      $sequence .= (string) $key . logger_serialise($value);
    }
    return $sequence;

  }
  else {
    return (string) $data;
  }
}
