<?php

/**
 * Drush support for the logger module.
 *
 * Copyright (c) 2008-2009 jokamajo.org
 *               2010 flukso.net
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

function logger_drush_command() {

  $items = array();

  $items['logger create node'] = array(
    'callback'    => 'logger_create_node',
    'description' => 'Create a new sensor node entry.',
    'arguments'   => array(
      'serial'    => 'Sensor node serial number.',
      'country'   => 'Destination country.',
      'uid'       => 'User ID.',
    ),  
    'options'     => array(
      '--pipe'    => 'Returns a space delimited list of created entries.',
    ),
  );

  $items['logger assign node'] = array(
    'callback'    => 'logger_assign_node',
    'description' => 'Assign a sensor node to a user.',
    'arguments'   => array(
      'serial'    => 'Sensor node serial number.',
      'uid'       => 'User ID.',
    ),
  );

  $items['logger config meter'] = array(
    'callback'    => 'logger_config_meter',
    'description' => 'Configure a specific meter.',
    'arguments'   => array(
      'meter'     => 'Meter ID.',
      'type'      => 'Meter type.',
      'function'  => 'Meter function.',
      'phase'     => 'Meter phase.',
      'constant'  => 'Meter constant.',
      'unit'      => 'Meter unit.',
    ),
    'options'     => array(
      '--pipe'    => 'Returns ok.'),
  );

  $items['logger token'] = array(
    'callback'      => 'logger_token',
    'description'   => 'Create tokens for meter/sensorIDs.',
    'arguments'     => array(
      'meter'       => 'Create a token for one specific meterID.',
      'permissions' => 'Set non-default permissions for this token.',
    ),
  );

  return $items;
}

function logger_create_node($serial, $country ="", $uid = 0) {

  module_load_include('inc', 'logger', 'logger.rrd');

  //guard against duplicating entries for the same S/N
  $sql = "SELECT COUNT(device) FROM {logger_devices} WHERE serial = %d";
  $count = db_result(db_query($sql, $serial));

  if ($count > 0) {
    drush_set_error(
      'LOGGER_CREATE_SERIAL_DUPLICATE',
      dt('The S/N: @serial already exists.', array('@serial' => $serial)));

  } else {

    if ($uid > 0) {

      $sql = "INSERT INTO {logger_users} (uid, private) VALUES (%d, %d)";
      $result = db_query($sql, $uid, 0);

      if (!$result) {
        drush_set_error('LOGGER_CREATE_USERS_ENTRY', dt('Error creating a user entry for @uid.', array('@uid' => $uid)));
      }
    }

    //create an entry in the {logger_devices} table
    $device  = md5(uniqid(rand(), TRUE));
    $sha     = md5(uniqid(rand(), TRUE));
    $created = time();

    $sql = "INSERT INTO {logger_devices} (device, serial, uid, sha, created, country) VALUES ('%s', %d, %d, '%s', %d, '%s')";
    $result = db_query($sql, $device, $serial, $uid, $sha, $created, $country);

    if (!$result) {
      drush_set_error('LOGGER_CREATE_DEVICE_ENTRY', dt('Error creating a device entry for @device.', array('@device' => $device)));
    }

    if (!drush_get_error()) {
      drush_log(dt('Successfully created the device/key: @device / @key for S/N: @serial', array('@device' => $device, '@key' => $sha, '@serial' => $serial)), 'ok');
      $pipe[] .= 'DEVICE=' . $device;
      $pipe[] .= 'KEY=' . $sha;
    }

    //create an entry in the {logger_meters} table
    for ($i = 0; $i < 4; $i++) {

      $permissions = 62;
      $meter = md5(uniqid(rand(), TRUE));
      $token = md5(uniqid(rand(), TRUE));

      $sql = "INSERT INTO {logger_meters} (meter, uid, device, created) VALUES ('%s', %d, '%s', %d)";
      $return = db_query($sql, $meter, $uid, $device, $created);

      $sql = "INSERT INTO {logger_tokens} (token, meter, permissions) VALUES ('%s', '%s', %d)";
      $insert = db_query($sql, $token, $meter, $permissions);

      if (!($return && $insert)) {
        drush_set_error('LOGGER_CREATE_METER_ENTRY', dt('Error creating meter entry for @meter.', array('@meter' => $meter)));
      }

      // create the meter base rrd
      $return = logger_rrd_base_create($meter);

      if (!($return == 0)) {
        drush_set_error('LOGGER_CREATE_RRD_BASE_ERROR', dt('Error creating the base @meter rrd.', array('@meter' => $meter)));
      }

      if (!drush_get_error()) {
        drush_log(dt('Successfully created the meter: @meter with token: @token', array('@meter' => $meter, '@token' => $token)), 'ok');
        $pipe[] .= 'SENSOR'.$i.'='.$meter;
      }
    }

    // Space delimited list for use by other scripts. Set the --pipe option.
    drush_print_pipe(implode(' ', $pipe));
  }
}

function logger_assign_node($serial, $country, $uid) {

  $sql = "SELECT device FROM {logger_devices} WHERE serial = %d";
  $device = db_result(db_query($sql, $serial));

  //check the existence of S/N
  if ($device == '') {
    drush_set_error('LOGGER_ASSIGN_SERIAL_NON_EXISTENT', dt('The S/N: @serial does not exist.', array('@serial' => $serial)));

  } else {

    $sql = "UPDATE {logger_devices} SET uid = %d, country = '%s' WHERE serial = %d";
    db_query($sql, $uid, $country, $serial);

    $sql = "UPDATE {logger_meters} SET uid = %d WHERE device = '%s'";
    db_query($sql, $uid, $device);

    $sql = "INSERT INTO {logger_users} (uid, private) VALUES (%d, %d)";
    $result = db_query($sql, $uid, 0);

    if (!$result) {
      drush_log(dt('uid: @uid already exists in the {logger_users} table', array('@uid' => $uid)), 'notice');
    }
  }

  if (!drush_get_error()) {
    drush_log(dt('Successfully assigned uid: @uid to S/N: @serial', array('@uid' => $uid, '@serial' => $serial)), 'ok');
  }
}

function logger_config_meter($meter, $type, $function, $phase, $constant, $unit) {

  module_load_include('inc', 'logger', 'logger.admin');

  if (logger_sensor_function_validate($function, $meter)) {

    $sql = "
      UPDATE
        {logger_meters}
      SET
        type = '%s',
        function = '%s',
        phase = %d,
        constant = %d,
        unit = '%s'
      WHERE
        meter = '%s'";

    $result = db_query($sql, $type, $function, $phase, $constant, $unit, $meter);

    drush_log(dt('Successfully updated meter: @meter', array('@meter' => $result)), 'ok');  
    drush_print_pipe('ok');

  } else {
    drush_set_error(
      'LOGGER_NON_UNIQUE_METER_FUNCTION',
      dt('The function name: @function is already been used by another sensor.',
        array('@function' => $function)));
  }
}

function logger_tokens($meter = "", $permissions = 62) {

  if ($meter == "") {

    $sql = "SELECT meter FROM {logger_meters}";
    $result = db_query($sql);

    while ($meter = db_fetch_object($result)) {

      $sql = "SELECT COUNT(meter) FROM {logger_tokens} WHERE meter = '%s'";
      $count = db_result(db_query($sql, $meter->meter));

      if ($count == 0) {

        $token = md5(uniqid(rand(), TRUE));

        $sql = "INSERT INTO {logger_tokens} (token, meter, permissions) VALUES ('%s', '%s', %d)";
        $insert = db_query($sql, $token, $meter->meter, $permissions);

        if (!$insert) {
          drush_set_error('LOGGER_CREATE_TOKEN_ENTRY', dt('Error creating token entry for @meter.', array('@meter' => $meter->meter)));

        } else {
          drush_log(dt('Created an entry in {logger_tokens} with token: @token and meter: @meter', array('@token' => $token , '@meter' => $meter->meter)), 'ok');
        }
      }
    }
  }
}