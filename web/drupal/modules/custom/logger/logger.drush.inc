<?php

/**
 * @file
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
    'callback' => 'logger_create_node',
    'description' => 'Create a new sensor node entry.',
    'arguments' => array(
      'serial' => 'Sensor node serial number.',
      'country' => 'Destination country.',
      'uid' => 'User ID.',
    ),
    'options' => array(
      '--pipe' => 'Returns a space delimited list of created entries.',
    ),
  );

  $items['logger assign node'] = array(
    'callback' => 'logger_assign_node',
    'description' => 'Assign a sensor node to a user.',
    'arguments' => array(
      'serial' => 'Sensor node serial number.',
      'uid' => 'User ID.',
    ),
  );

  $items['logger config meter'] = array(
    'callback' => 'logger_config_meter',
    'description' => 'Configure a specific meter.',
    'arguments' => array(
      'meter' => 'Meter ID.',
      'type' => 'Meter type.',
      'function' => 'Meter function.',
      'phase' => 'Meter phase.',
      'constant' => 'Meter constant.',
      'unit' => 'Meter unit.',
    ),
    'options' => array(
      '--pipe' => 'Returns ok.',
    ),
  );

  $items['logger token'] = array(
    'callback' => 'logger_token',
    'description' => 'Create tokens for meter/sensorIDs.',
    'arguments' => array(
      'meter' => 'Create a token for one specific meterID.',
      'permissions' => 'Set non-default permissions for this token.',
    ),
  );

  return $items;
}

/**
 * Registers a new device. 
 * 
 * @param $serial  The device serial number.
 * @param $country The user country code.
 * @param $uid     The user id.
 */
function logger_create_node($serial, $country = "", $uid = 0) {

  module_load_include('inc', 'logger', 'logger.rrd');

  //guard against duplicating entries for the same S/N
  $sql = "SELECT COUNT(device) FROM {logger_devices} WHERE serial = :serial";
  $count = db_query($sql, array(':serial' => $serial))->fetchField();

  if ($count > 0) {
    drush_set_error(
      'LOGGER_CREATE_SERIAL_DUPLICATE',
      dt('The S/N: @serial already exists.', array('@serial' => $serial)));
  }
  else {

    if ($uid > 0) {

      $result = db_insert('logger_users')
        ->fields(array(
          'uid' => $uid,
          'private' => 0
        ))
        ->execute();

      if (!$result) {
        drush_set_error('LOGGER_CREATE_USERS_ENTRY', dt('Error creating a user entry for @uid.', array('@uid' => $uid)));
      }
    }

    //create an entry in the {logger_devices} table
    $device  = md5(uniqid(rand(), TRUE));
    $sha     = md5(uniqid(rand(), TRUE));
    $created = time();

    $result = $id = db_insert('logger_devices')
      ->fields(array(
        'device' => $device,
        'serial' => $serial,
        'uid' => $uid,
        'sha' => $sha,
        'created' => $created,
        'country' => $country,
      ))
      ->execute();

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

      $inserted = logger_sensor_add($meter, $uid, $device, $created, $token, $permissions);

      if (!$inserted) {
        drush_set_error('LOGGER_CREATE_METER_ENTRY', dt('Error creating meter entry for @meter.', array('@meter' => $meter)));

      } else {
        // create the meter base rrd
        $created = logger_rrd_base_create($meter);

        if (!$created) {
          drush_set_error('LOGGER_CREATE_RRD_BASE_ERROR', dt('Error creating the base @meter rrd.', array('@meter' => $meter)));
        }

        if (!drush_get_error()) {
          drush_log(dt('Successfully created the meter: @meter with token: @token', array('@meter' => $meter, '@token' => $token)), 'ok');
          $pipe[] .= 'SENSOR' . $i . '=' . $meter;
        }
      }
    }

    // Space delimited list for use by other scripts. Set the --pipe option.
    drush_print_pipe(implode(' ', $pipe));
  }
}

/**
 * Assigns a flukso device to a user.
 * 
 * @param $serial  The device serial number.
 * @param $country The user country code.
 * @param $uid     The user id.
 */
function logger_assign_node($serial, $country, $uid) {

  $sql = "SELECT device FROM {logger_devices} WHERE serial = :serial";
  $device = db_query($sql, array(':serial' => $serial))->fetchField();

  //check the existence of S/N
  if ($device == '') {
    drush_set_error('LOGGER_ASSIGN_SERIAL_NON_EXISTENT', dt('The S/N: @serial does not exist.', array('@serial' => $serial)));
  }
  else {

    db_update('logger_devices')
      ->fields(array(
        'uid' => $uid,
        'country' => $country,
      ))
      ->condition('serial', $serial)
      ->execute();

    db_update('logger_meters')
      ->fields(array(
        'uid' => $uid,
      ))
      ->condition('device', $device)
      ->execute();

    $result = db_insert('logger_users')
      ->fields(array(
        'uid' => $uid,
        'private' => 0
      ))
      ->execute();

    if (!$result) {
      drush_log(dt('uid: @uid already exists in the {logger_users} table', array('@uid' => $uid)), 'notice');
    }
  }

  if (!drush_get_error()) {
    drush_log(dt('Successfully assigned uid: @uid to S/N: @serial', array('@uid' => $uid, '@serial' => $serial)), 'ok');
  }
}

/**
 * Updates a sensor.
 * 
 * @param $meter     The sensor id.
 * @param $type      The sensor type.
 * @param $function  The sensor function name.
 * @param $phase     The sensor phase.
 * @param $constant  The sensor constant.
 * @param $unit      The sensor unit.
 */
function logger_config_meter($meter, $type, $function, $phase, $constant, $unit) {

  module_load_include('inc', 'logger', 'logger.admin');

  if (logger_sensor_function_validate($function, $meter)) {

    $result = db_update('logger_meters')
      ->fields(array(
        'type' => $type,
        'function' => $function,
        'phase' => $phase,
        'constant' => $constant,
        'unit' => $unit,
      ))
      ->condition('meter', $meter)
      ->execute();    
    
    drush_log(dt('Successfully updated meter: @meter', array('@meter' => $result)), 'ok');
    drush_print_pipe('ok');
  }
  else {
    drush_set_error(
      'LOGGER_NON_UNIQUE_METER_FUNCTION',
      dt('The function name: @function is already been used by another sensor.',
        array('@function' => $function)));
  }
}

/**
 * Creates sensor tokens.
 * 
 * @param $meter       The sensor id.
 * @param $permissions The user permissions.
 */
function logger_tokens($meter = "", $permissions = 62) {

  if ($meter == "") {

    $sql = "SELECT meter FROM {logger_meters}";
    $result = db_query($sql);

    foreach ($result as $meter) {

      $sql = "SELECT COUNT(meter) FROM {logger_tokens} WHERE meter = :meter";
      $count = db_query($sql, array(':meter' => $meter->meter))->fetchField();

      if ($count == 0) {

        $token = md5(uniqid(rand(), TRUE));

        $insert = db_insert('logger_tokens')
          ->fields(array(
            'token' => $token,
            'meter' => $meter->meter,
            'permissions' => $permissions
          ))
          ->execute();

        if (!$insert) {
          drush_set_error('LOGGER_CREATE_TOKEN_ENTRY', dt('Error creating token entry for @meter.', array('@meter' => $meter->meter)));
        }
        else {
          drush_log(dt('Created an entry in {logger_tokens} with token: @token and meter: @meter', array('@token' => $token, '@meter' => $meter->meter)), 'ok');
        }
      }
    }
  }
}
