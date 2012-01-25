<?php

/**
 * @file
 * Callbacks for logger account and admin pages.
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

/**
 * Builds the user devices page.
 *
 * @return the user devices page.
 */
function logger_userdevices_page() {

  global $user;

  $query = db_select('logger_devices', 'd')
    ->fields('d', array('device', 'serial', 'access', 'resets', 'uptime'))
    ->condition('d.uid', $user->uid, '=')
    ->orderBy('d.serial');
  $query->leftJoin('device_support_request', 'r', 'd.device = r.device');
  $query->addField('r', 'requested', 'support_requested');

  $devices = $query->execute();

  $rows = array();
 
  foreach ($devices as $device) {

    $links = l(t('Remove'), "device/deactivate/$device->device") . '<br>' .
             l(t('History'), "event/log/list/$device->device");

    //TODO: use a version field
    if ($device->serial > 100000000) {
      $support_link = $device->support_requested ?
        l(t('Suspend Support'), "device/support/suspend/$device->device") :
        l(t('Request Support'), "device/support/confirm/$device->device");

      $links .= "<br><span style='white-space: nowrap;'>$support_link</span>";
    }

    $row = array();
    $row[] = $device->serial;
    $row[] = format_hashcode($device->device);
    $row[] = $device->resets;
    $row[] = unixtime_to_localtime($device->access);
    $row[] = seconds_to_dayshours($device->uptime);
    $row[] = $links;

    $rows[] = $row;
  }
 
  $header = array(t('Serial'), t('Device Id'), t('Resets'), t('Last heartbeat'), t('Uptime'), t('Operations'));

  $form = drupal_get_form('logger_deviceactivation_form');
  return drupal_render($form) .

    theme_table(array('header' => $header, 'rows' => $rows, 'caption' => '',
      'empty' => t('No devices available.'), 'sticky' => FALSE, 'colgroups' => array(),
      'attributes' => array('id' => LOGGER_LEGEND_TABLE_ID, 'class' => 'resizable')));
}

/**
 * Builds the user sensors page.
 *
 * @return the user sensors page.
 */
function logger_usersensors_page() {

  global $user;

  $query = db_select('logger_meters', 'lm');
  $query->join('logger_tokens', 'lt', 'lm.meter = lt.meter');
  $query->leftJoin('logger_meter_type', 'st', 'lm.type = st.id');
  $query->addField('st', 'name', 'type');

  $sensors = $query
    ->fields('lm', array('meter', 'function'))
    ->fields('lt', array('token', 'permissions'))
    ->condition('lm.uid', $user->uid, '=')
    ->orderBy('lm.function', 'DESC')
    ->execute();

  $rows = array();
  foreach ($sensors as $sensor) {

    $row = array();
    $row[] = format_hashcode($sensor->meter);
    $row[] = format_hashcode($sensor->token);
    $operations = '';

    if ($sensor->function == NULL) {
      $row[] = t('Inactive');
      $row[] = '';
      $row[] = '';
      $operations = l(t('Activate'), 'sensor/activate/' . $sensor->meter);
    }
    else {
      $row[] = t('Active');
      $row[] = t($sensor->type);
      $row[] = $sensor->function;
      $operations .=
        l(t('Edit'), 'sensor/edit/' . $sensor->meter) . '<br>' .
        l(t('Deactivate'), 'sensor/deactivate/' . $sensor->meter);
    }
    $row[] = $operations;
    $rows[] = $row;
  }

  $header = array(t('Sensor'), t('Token'), t('Status'), t('Type'), t('Function'), t('Operation'));

  return theme_table(array('header' => $header, 'rows' => $rows, 'caption' => '',
   'empty' => t('No sensors available.'), 'sticky' => FALSE, 'colgroups' => array(),
   'attributes' => array('id' => LOGGER_LEGEND_TABLE_ID, 'class' => 'resizable')));
}

/**
 * Builds the sensor edition page.
 *
 * @param $meter  The sensor id.
 * @return the sensor edition page.
 */
function logger_sensor_edition_page($meter) {

  $sensor = db_select('logger_meters', 'm')
    ->fields('m', array('meter', 'type', 'function'))
    ->condition('m.meter', $meter, '=')
    ->execute()
    ->fetchObject();

  $form = drupal_get_form('logger_sensor_form', $meter, $sensor->type, $sensor->function);
  return drupal_render($form);
}

function logger_deviceactivation_form($form, &$form_state) {

  $form['code'] = array(
    '#title' => t('Activation Code'),
    '#type' => 'textfield',
    '#description' => t('Please, inform the activation code of the device to be added. This code is found in the device installation guide.'),
    '#size' => 10,
    '#maxlength' => 10,
    '#required' => TRUE,
  );

  $form['submit'] = array(
    '#type' => 'submit',
    '#value' => t('Add device'),
  );
  return $form;
}

function logger_sensor_form($form, &$form_state, $meter, $type = CONSUMPTION_SENSOR_TYPE, $function = 'main') {

  $form['meter'] = array(
    '#type' => 'textfield',
    '#description' => t("The sensor meter id."),
    '#maxlength' => 32,
    '#size' => 32,
    '#required' => TRUE,
    '#default_value' => $meter,
    '#attributes' => array('readonly' => 'readonly'),
  );

  $form['type'] = array(
    '#type' => 'select',
    '#description' => t('The sensor type.'),
    '#options' => logger_get_sensor_types_options(),
    '#default_value' => $type,
    '#required' => TRUE,
    '#attributes' => array('readonly' => 'readonly'),
  );

  $form['function'] = array(
    '#type' => 'textfield',
    '#description' => t("Enter the sensor's function name."),
    '#maxlength' => 16,
    '#size' => 16,
    '#required' => TRUE,
    '#default_value' => $function,
  );

  $form['submit'] = array(
    '#type' => 'submit',
    '#value' => t("Save"),
  );
  return $form;
}

function logger_userprivacy_form($form, &$form_state) {

  global $user;

  $private = db_select('logger_users', 'u')
    ->fields('u', array('private'))
    ->condition('u.uid', $user->uid, '=')
    ->execute()
    ->fetchField();

  $form['privacy'] = array(
    '#title' => t('Set your preferred privacy mode'),
    '#type' => 'radios',
    '#description' => t("When selecting Private mode, your data stream will not be drawn on other people's graph and all statistics will be marked as 'prv'. Conversely, nobody else's data stream will be drawn on your chart."),
    '#default_value' => (isset($private) ? $private : 0),
    '#options' => array(
      t('Shared within Flukso'),
      t('Private'),
    ),
  );

  $form['submit'] = array(
    '#type' => 'submit',
    '#value' => t('Submit'),
  );
  return $form;
}

function logger_deviceactivation_form_validate($form, &$form_state) {

  $code = $form_state['values']['code'];
  if (strlen($code) == 10) {

    $query = db_select('logger_devices', 'd');
    $query->addExpression('COUNT(*)', 'count');

    $count = $query
      ->condition('d.device', "$code%", 'LIKE')
      ->condition('d.uid', 0, '=')
      ->execute()
      ->fetchField();

    if ($count != 1) {
      form_set_error('code', t('Please specify a valid activation code.'));
    }
  }
  else {
    form_set_error('code', t('Please specify a proper 10-character activation code.'));
  }
}

function logger_sensor_form_validate($form, &$form_state) {

  $function = $form_state['values']['function'];
  $meter = $form_state['values']['meter'];

  if (!logger_sensor_function_validate($function, $meter)) {

    form_set_error('function', t('Another sensor was found with the function name: %function_name. ' .
      'Please inform another name which has not yet been used.', array('%function_name' => $function)));
  }
}

function logger_deviceactivation_form_submit($form, &$form_state) {

  $form_state['redirect'] = 'device/activate/' . $form_state['values']['code'];
}

function logger_sensor_form_submit($form, &$form_state) {

  global $user;

  $function = $form_state['values']['function'];
  $type = $form_state['values']['type'];
  $meter = $form_state['values']['meter'];

  db_update('logger_meters')
    ->fields(array(
      'function' => $function,
      'type' => $type,
    ))
    ->condition('meter', $meter)
    ->execute();

  $form_state['redirect'] = "user/" . $user->uid . "/sensors";
}

function logger_userprivacy_form_submit($form, &$form_state) {

  global $user;

  db_update('logger_users')
    ->fields(array(
      'private' => $form_state['values']['privacy']
    ))
    ->condition('uid', $user->uid)
    ->execute();

  // page redirection to the home page after form submission
  $form_state['redirect'] = 'logger';
}

/**
 * Associates a device with the authenticated user.
 *
 * @param $code  The activation code.
 */
function logger_device_activate($code) {

  global $user;

  db_update('logger_devices')
    ->fields(array(
      'uid' => $user->uid,
    ))
    ->condition('device', "$code%", 'LIKE')
    ->execute();

  $sensors = db_select('logger_meters', 'm')
    ->fields('m', array('meter', 'function'))
    ->condition('m.device', "$code%", 'LIKE')
    ->execute();

  $i = 2;
  foreach ($sensors as $sensor) {

    $function = $sensor->function;

    //Find an unique name for 'function'
    while ($function && !logger_sensor_function_validate($function, $sensor->meter)) {
      $function = "Sensor $i";
      $i++;
    }

    db_update('logger_meters')
      ->fields(array(
        'uid' => $user->uid,
        'function' => $function
      ))
      ->condition('meter', $sensor->meter)
      ->execute();
  }

  $rid = get_role_id('fluksonian');

  // check whether the user is already a true fluksonian
  // if not, generate the proper entries in {users_roles} and {logger_users}
  if (empty($user->roles[$rid])) {

    db_insert('users_roles')
      ->fields(array(
        'uid' => $user->uid,
        'rid' => $rid
      ))
      ->execute();

    db_insert('logger_users')
      ->fields(array(
        'uid' => $user->uid,
        'private' => 0
      ))
      ->execute();
  }
  drupal_set_message(t("The device is now associated with your account."));

  drupal_goto("user/$user->uid/devices");
}

/**
 * Deactivates a device and all its sensors, removing their user association.
 *
 * @param $device  The device id.
 */
function logger_device_deactivate($device) {

  global $user;
  module_load_include('inc', 'logger', 'logger.rrd');

  $sensors = db_select('logger_meters', 'm')
    ->fields('m')
    ->condition('m.device', $device, '=')
    ->execute();

  foreach ($sensors as $sensor) {

    //FIXME: define a hook
    msgdump_remove_by_sensor($sensor->meter);

    db_update('logger_meters')
      ->fields(array(
        'uid' => 0,
      ))
      ->condition('meter', $sensor->meter)
      ->execute();

    logger_rrd_clear($sensor->meter);
  }

  //FIXME: define a hook
  notification_remove_by_device($device);

  db_update('logger_devices')
    ->fields(array(
      'uid' => 0
    ))
    ->condition('device', $device)
    ->execute();

  logger_check_fluksonian($user->uid);

  drupal_goto("user/$user->uid/devices");
}

/**
 * Removes a device and all its sensors, tokens, and logs.
 *
 * @param $device  The device id.
 */
function logger_device_remove($device) {

  module_load_include('inc', 'logger', 'logger.rrd');

  $sensors = db_select('logger_meters', 'm')
    ->fields('m')
    ->condition('m.device', $device, '=')
    ->execute();

  foreach ($sensors as $sensor) {

    //FIXME: define a hook
    msgdump_remove_by_sensor($sensor->meter);

    db_delete('logger_tokens')
      ->condition('meter', $sensor->meter)
      ->execute();

    db_delete('logger_meters')
      ->condition('meter', $sensor->meter)
      ->execute();

    logger_rrd_remove($sensor->meter);
  }

  //FIXME: define a hook
  notification_remove_by_device($device);

  $flukso = db_select('logger_devices', 'd')
    ->fields('d', array('uid'))
    ->condition('d.device', $device, '=')
    ->execute()
    ->fetchObject();

  db_delete('logger_devices')
    ->condition('device', $device)
    ->execute();

  logger_check_fluksonian($flukso->uid);

  return t("Device and all its data have been successfuly removed.");
}

/**
 * Checks whether the user is still a fluksonian. If not, remove entries in users_roles and logger_users.
 *
 * @param $uid The user id.
 */
function logger_check_fluksonian($uid) {

  $sql = "SELECT count(*) FROM {logger_devices} WHERE uid = :uid";
  $count = db_query($sql, array(':uid' => $uid))->fetchField();

  if ($count == 0) {

    $rid = get_role_id('fluksonian');

    db_delete('users_roles')
      ->condition('uid', $uid)
      ->condition('rid', $rid)
      ->execute();

    db_delete('logger_users')
      ->condition('uid', $uid)
      ->execute();
  }
}

/**
 * Deactivates a sensor, setting its function to blank.
 *
 * @param $meter  The sensor id.
 */
function logger_sensor_deactivate($meter) {

  global $user;

  db_update('logger_meters')
    ->fields(array(
      'function' =>  NULL,
      'type' =>  NULL,
    ))
    ->condition('meter', $meter)
    ->execute();

  drupal_goto("user/$user->uid/sensors");
}

/**
 * Validates the uniqueness of a sensor function.
 *
 * @param $function The function to be validated.
 * @param $meter    The meter which currently has the function.
 * @return TRUE if it is unique; FALSE otherwise.
 */
function logger_sensor_function_validate($function, $meter) {

  global $user;

  $count = 0;

  if ($function) {
    $query = db_select('logger_meters', 'm');
    $query->addExpression('COUNT(*)', 'count');
    $count = $query
      ->condition('m.function', $function, '=')
      ->condition('m.uid', $user->uid, '=')
      ->condition('m.meter', $meter, '<>')
      ->execute()
      ->fetchField();
  }
  return $count == 0;
}
