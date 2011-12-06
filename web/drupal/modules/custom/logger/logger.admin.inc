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
 * Builds the user sensors page.
 *
 * @return the user sensors page.
 */
function logger_user_sensors_page() {

  global $user;
  $rows = array();

  $sql = "
    SELECT
      lm.meter,
      lm.function,
      st.name AS type,
      lt.token,
      lt.permissions
    FROM
      {logger_meters} lm
      INNER JOIN {logger_tokens} lt ON lm.meter = lt.meter
      LEFT JOIN {logger_meter_type} st ON lm.type = st.id
    WHERE
      lm.uid = :uid
    ORDER BY
      lm.function DESC";

  $result = db_query($sql, array(':uid' => $user->uid));

  foreach ($result as $sensor) {

    $row = array();
    $row[] = substr($sensor->meter, 0, 15) . '<br>' . substr($sensor->meter, 15);
    $row[] = substr($sensor->token, 0, 15) . '<br>' . substr($sensor->token, 15);

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
  return theme('logger_usersensors_list', array('items' => $rows));
}

/**
 * Builds the sensor edition page.
 *
 * @param $meter  The sensor id.
 * @return the sensor edition page.
 */
function logger_sensor_edition_page($meter) {

  $sql = "SELECT meter, type, function FROM {logger_meters} WHERE meter = :meter";
  $sensor = db_query($sql, array(':meter' => $meter))->fetchObject();

  return drupal_render( drupal_get_form('logger_sensor_form', $meter, $sensor->type, $sensor->function));
}

function logger_userdevices_form($form, &$form_state) {

  $form['#theme'] = 'logger_userdevices';

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

  $sql = "SELECT private FROM {logger_users} WHERE uid = :uid";
  $private = db_query($sql, array(':uid' => $user->uid))->fetchField();

  $form['privacy'] = array(
    '#title' => t('Set your preferred privacy mode'),
    '#type' => 'radios',
    '#description' => t("When selecting Private mode, your data stream will not be drawn on other people's graph and all statistics will be marked as 'prv'. Conversely, nobody else's data stream will be drawn on your chart."),
    '#default_value' => $private,
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

function logger_userdevices_form_validate($form, &$form_state) {

  $code = $form_state['values']['code'];
  if (strlen($code) == 10) {

    $count = db_query("SELECT COUNT(*) FROM {logger_devices} WHERE device LIKE :device AND uid = :uid", array(':device' => "$code%", ':uid' => 0))->fetchField();

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

function logger_userdevices_form_submit($form, &$form_state) {

  global $user;
  $code = $form_state['values']['code'];

  db_update('logger_devices')
    ->fields(array(
      'uid' => $user->uid,
    ))
    ->condition('device', "$code%", 'LIKE')
    ->execute();

  $result = db_query("SELECT meter, function FROM {logger_meters} WHERE device LIKE ':code'", "$code%");
  $i = 2;

  foreach ($result as $sensor) {

    $function = $sensor->function;

    //Find an unique name for 'function'
    while ($function && !logger_sensor_function_validate($function, $sensor->meter)) {
      $function = "Sensor $i";
      $i++;
    }

    if ($function) {
      db_update('logger_meters')
      ->fields(array(
        'uid' => $user->uid,
        'function' => $function,
      ))
      ->condition('meter', $sensor->meter)
      ->execute();
    }
    else {
        
      db_update('logger_meters')
        ->fields(array(
          'function' => $function,
        ))
        ->condition('meter', $sensor->meter)
        ->execute();
    }
  }

  $rid = db_query("SELECT SQL_CACHE rid FROM {role} WHERE name = :name", array(':name' => 'fluksonian'))->fetchField();

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
 * Deactivates a device and all its sensors, removing their user association.
 *
 * @param $device  The device id.
 */
function logger_device_deactivate($device) {

  global $user;

  db_update('logger_devices')
    ->fields(array(
      'uid' => 0,
    ))
    ->condition('device', $device)
    ->execute();
  
  db_update('logger_meters')
    ->fields(array(
      'uid' => 0,
    ))
    ->condition('device', $device)
    ->execute();

  $sql = "SELECT count(*) FROM {logger_devices} WHERE uid = :uid";
  $count = db_query($sql, array(':uid' => $user->uid))->fetchField();

  // check whether the user is still a fluksonian
  // if not, remove entries in {users_roles} and {logger_users}
  if ($count == 0) {

    $sql = "SELECT SQL_CACHE rid FROM {role} WHERE name = :name";
    $rid = db_query($sql, array(':name' => 'fluksonian'))->fetchField();

    db_delete('users_roles')
      ->condition('uid', $user->uid)
      ->condition('rid', $rid)
      ->execute();
    
    db_delete('logger_users')
      ->condition('uid', $user->uid)
      ->condition('private', 0)
      ->execute();
  }

  drupal_goto("user/$user->uid/devices");
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

    $sql = "
      SELECT
        count(*)
      FROM
        {logger_meters}
      WHERE
        function = :function AND
        uid = :uid AND
        meter <> :meter";

    $count = db_query($sql, array(':function' => $function, ':uid' => $user->uid, ':meter' => $meter))->fetchField();
  }
  return $count == 0;
}

/**
 * Theme for displaying the devices form.
 *
 * @param $form The device form array.
 * @return the themed devices form.
 */
function theme_logger_userdevices($variables) {
  $form = $variables['form'];

  global $user;
  $rows = array();

  $sql = "
    SELECT
      d.device,
      d.serial,
      d.access,
      d.resets,
      d.uptime,
      r.requested AS support_requested
    FROM
      {logger_devices} d LEFT JOIN
      {device_support_request} r ON d.device = r.device
    WHERE
      d.uid = :uid
    ORDER BY
      d.serial";
  $result = db_query($sql, array(':uid' => $user->uid));

  foreach ($result as $device) {

    $support_link = $device->support_requested ?
      l(t('Suspend Support'), "device/support/suspend/$device->device") :
      l(t('Request Support'), "device/support/confirm/$device->device");

    //FIXME: Use $device->device instead of $device->serial

    $row = array();
    $row[] = $device->serial;
    $row[] = $device->resets;
    $row[] = logger_unix_to_userlocaltime($device->access);
    $row[] = logger_seconds_to_dayshours($device->uptime);
    $row[] = l(t('Remove'),   "device/deactivate/$device->device") . '<br>' .
             l(t('History'), "event/log/list/$device->serial") . '<br>' .
             $support_link;

    $rows[] = $row;
  }

  return drupal_render_children($form) .
    theme('logger_userdevices_list', array('items' => $rows));
}

/**
 * Converts unix timestamp to user local time.
 * 
 * @param $unix The unix timestamp.
 * @return the converted time.
 */
function logger_unix_to_userlocaltime($unix) {
  global $user;
  $unix_local = gmdate('r', $unix + $user->timezone);
  return substr($unix_local, 0, strlen($unix_local) - 6);
}

/**
 * Formats timestamp as a readable string informing days and hours.
 * 
 * @param $seconds The timestamp in seconds.
 * @return the converted string.
 */
function logger_seconds_to_dayshours($seconds) {
  $days    = intval($seconds / DAY);
  $hours   = intval(($seconds % DAY) / HOUR);
  return sprintf('%d days %d hours', $days, $hours);
}

/**
 * Theme for displaying the list of devices.
 *
 * @param $items  An array of table rows.
 * @return the themed list of devices.
 */
function theme_logger_userdevices_list($variables) {
  $items = $variables['items'];

  if (count($items) > 0) {

    $headers = array(t('Serial'), t('Resets'), t('Last heartbeat'), t('Uptime'), t('Operations'));
    return theme_table(array('header' => $headers, 'rows' => $items));

  }
  else {
    return t('No devices available.');
  }
}

/**
 * Theme for displaying the list of user's sensors.
 *
 * @param $items An array of table rows.
 * @return the themed list of sensors.
 */
function theme_logger_usersensors_list($variables) {
  $items = $variables['items'];

  if (count($items) > 0) {

    $headers = array(t('Sensor'), t('Token'), t('Status'), t('Type'), t('Function'), t('Operation'));
    return theme_table(array('header' => $headers, 'rows' => $items));

  }
  else {
    return t('No sensors available.');
  }
}

/**
 * Define the administration settings form for the logger module
 */
function logger_admin_settings($form, &$form_state) {
  //TODO
}
