<?php

/**
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
function _logger_user_sensors_page() {

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
      lm.uid = %d
    ORDER BY
      lm.function DESC";

  $result = db_query($sql, $user->uid);

  while ($sensor = db_fetch_object($result)) {

    $row = array();
    $row[] = substr($sensor->meter, 0, 15) . '<br>' . substr($sensor->meter, 15);
    $row[] = substr($sensor->token, 0, 15) . '<br>' . substr($sensor->token, 15);

    $operations = '';

    if ($sensor->function == NULL) {
      $row[] = t('Inactive');
      $row[] = '';
      $row[] = '';
      $operations = l(t('Activate'), 'sensor/activate/' . $sensor->meter);

    } else {
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
  return theme('logger_usersensors_list', $rows);
}

/**
 * Builds the sensor edition page.
 *
 * @param $meter  The sensor id.
 * @return the sensor edition page.
 */
function _logger_sensor_edition_page($meter) {

  $sql = "SELECT meter, type, function FROM {logger_meters} WHERE meter = '%s'";
  $sensor = db_fetch_object(db_query($sql, $meter));

  return drupal_get_form('logger_sensor_form', $meter, $sensor->type, $sensor->function);
}

function logger_userdevices_form() {

  $form['#theme'] = 'logger_userdevices';

  $form['code'] = array(
    '#title'        => t('Activation Code'),
    '#type'         => 'textfield',
    '#description'  => t('Please, inform the activation code of the device to be added. This code is found in the device installation guide.'),
    '#size'         => 10,
    '#maxlength'    => 10,
    '#required'     => TRUE,
  );

  $form['submit'] = array(
    '#type'   => 'submit',
    '#value'  => t('Add device'),
  );
  return $form;
}

function logger_sensor_form(&$form_state, $meter, $type = CONSUMPTION_SENSOR_TYPE, $function = 'main') {

  $form['meter'] = array(
    '#type'          => 'textfield',
    '#description'   => t("The sensor meter id."),
    '#maxlength'     => 32,
    '#size'          => 32,
    '#required'      => TRUE,
    '#default_value' => $meter,
    '#attributes'    => array('readonly' => 'readonly')
  );

  $form['type'] = array(
    '#type'          => 'select',
    '#description'   => t('The sensor type.'),
    '#options'       => logger_get_sensor_types_options(),
    '#default_value' => $type,
    '#required'      => TRUE,
    '#attributes'    => array('readonly' => 'readonly')
  );

  $form['function'] = array(
    '#type'          => 'textfield',
    '#description'   => t("Enter the sensor's function name."),
    '#maxlength'     => 16,
    '#size'          => 16,
    '#required'      => TRUE,
    '#default_value' => $function
  );

  $form['submit'] = array(
    '#type'  => 'submit',
    '#value' => t("Save")
  );
  return $form;
}

function logger_userprivacy_form() {

  global $user;

  $sql = "SELECT private FROM {logger_users} WHERE uid = %d";
  $private = db_result(db_query($sql, $user->uid));

  $form['privacy'] = array(
    '#title'         => t('Set your preferred privacy mode'),
    '#type'          => 'radios',
    '#description'   => t("When selecting Private mode, your data stream will not be drawn on other people's graph and all statistics will be marked as 'prv'. Conversely, nobody else's data stream will be drawn on your chart."),
    '#default_value' => $private,
    '#options'       => array(
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

    $count = db_result(db_query("SELECT COUNT(*) FROM {logger_devices} WHERE device LIKE '%s%' AND uid = %d", $code, 0));

    if ($count != 1) {
      form_set_error('code', t('Please specify a valid activation code.'));
    }

  } else {
    form_set_error('code', t('Please specify a proper 10-character activation code.'));
  }
}

function logger_sensor_form_validate($form, &$form_state) {

  $function = $form_state['values']['function'];
  $meter = $form_state['values']['meter'];

  if (!_logger_sensor_function_validate($function, $meter)) {

    form_set_error('function', t('Another sensor was found with the function name: %function_name. ' .
      'Please inform another name which has not yet been used.', array('%function_name' => $function)));
  }
}

function logger_userdevices_form_submit($form, &$form_state) {

  global $user;
  $code = $form_state['values']['code'];

  db_query("UPDATE {logger_devices} SET uid = %d WHERE device LIKE '%s%'", $user->uid, $code);

  $result = db_query("SELECT meter, function FROM {logger_meters} WHERE device LIKE '%s%'", $code);
  $i = 2;

  while($sensor = db_fetch_object($result)) {

    $function = $sensor->function;

    //Find an unique name for 'function'
    while ($function && !_logger_sensor_function_validate($function, $sensor->meter)) {
      $function = "Sensor $i";
      $i++;
    }

    if ($function) {
      db_query("UPDATE {logger_meters} SET uid = %d, function = '%s' WHERE meter = '%s'", $user->uid, $function, $sensor->meter);
    }
  }

  $rid = db_result(db_query("SELECT SQL_CACHE rid FROM {role} WHERE name = '%s'", 'fluksonian'));

  // check whether the user is already a true fluksonian
  // if not, generate the proper entries in {users_roles} and {logger_users}
  if (empty($user->roles[$rid])) {

    db_query("INSERT INTO {users_roles} (uid, rid) VALUES (%d, %d)", $user->uid, $rid);
    db_query("INSERT INTO {logger_users} (uid, private) VALUES (%d, %d)", $user->uid, 0);
  }

  drupal_set_message(t("The device is now associated with your account."));
}

function logger_sensor_form_submit($form, &$form_state) {

  global $user;

  $function = $form_state['values']['function'];
  $type = $form_state['values']['type'];
  $meter = $form_state['values']['meter'];

  $sql = "UPDATE {logger_meters} SET function = '%s', type = %d WHERE meter = '%s'";
  db_query($sql, $function, $type, $meter);

  $form_state['redirect'] = "user/" . $user->uid . "/sensors";
}

function logger_userprivacy_form_submit($form, &$form_state) {

  global $user;

  $private = $form_state['values']['privacy'];

  $sql = "UPDATE {logger_users} SET private = %d WHERE uid = %d";
  db_query($sql, $private, $user->uid);

  // page redirection to the home page after form submission
  $form_state['redirect'] = 'logger';
}

/**
 * Deactivates a device and all its sensors, removing their user association.
 *
 * @param $device  The device id.
 */
function _logger_device_deactivate($device) {

  global $user;

  db_query("UPDATE {logger_devices} SET uid = %d WHERE device = '%s'", 0, $device);
  db_query("UPDATE {logger_meters}  SET uid = %d WHERE device = '%s'", 0, $device);

  $count = db_result(db_query("SELECT count(*) FROM {logger_devices} WHERE uid = %d", $user->uid));

  // check whether the user is still a fluksonian
  // if not, remove entries in {users_roles} and {logger_users}
  if ($count == 0) {

    $rid = db_result(db_query("SELECT SQL_CACHE rid FROM {role} WHERE name = '%s'", 'fluksonian'));

    db_query("DELETE FROM {users_roles} WHERE uid = %d AND rid = %d", $user->uid, $rid);
    db_query("DELETE FROM {logger_users} WHERE uid = %d AND private = 0", $user->uid);
  }

  drupal_goto("user/$user->uid/devices");
}

/**
 * Deactivates a sensor, setting its function to blank.
 *
 * @param $meter  The sensor id.
 */
function _logger_sensor_deactivate($meter) {

  global $user;

  $sql = "UPDATE {logger_meters} SET function = NULL, type = NULL WHERE meter = '%s'";
  db_query($sql, $meter);

  drupal_goto("user/$user->uid/sensors");
}

/**
 * Validates the uniqueness of a sensor function.
 *
 * @param $function The function to be validated.
 * @param $meter    The meter which currently has the function.
 * @return TRUE if it is unique; FALSE otherwise.
 */
function _logger_sensor_function_validate($function, $meter) {

  global $user;
  $count = 0;

  if ($function) {

    $sql = "
      SELECT
        count(*)
      FROM
        {logger_meters}
      WHERE
        function = '%s' AND
        uid = %d AND
        meter <> '%s'";

    $count = db_result(db_query($sql, $function, $user->uid, $meter));
  }
  return $count == 0;
}

/**
 * Theme for displaying the devices form.
 *
 * @param $form The device form array.
 * @return the themed devices form.
 */
function theme_logger_userdevices($form) {

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
      d.uid = %d
    ORDER BY
      d.serial";
  $result = db_query($sql, $user->uid);

  function unix_to_userlocaltime($unix) {
    global $user;
    $unix_local = gmdate('r', $unix + $user->timezone);
    return substr($unix_local, 0, strlen($unix_local) - 6);
  }

  function seconds_to_dayshours($seconds) {
    $days    = intval($seconds / DAY);
    $hours   = intval(($seconds % DAY) / HOUR);
    return sprintf('%d days %d hours', $days, $hours);
  }

  //FIXME: Some roles are in English, others in German
  $rid = db_result(db_query("SELECT SQL_CACHE rid FROM {role} WHERE name = '%s'", 'Techniker'));
  $is_technician = !empty($user->roles[$rid]);
  
  while ($device = db_fetch_object($result)) {

    $support_link = $is_technician ? ('<br>' . (
      $device->support_requested ?
        l(t('Suspend Support'), "device/support/suspend/$device->device") :
        l(t('Request Support'), "device/support/confirm/$device->device"))) : '';
    
    //FIXME: Use $device->device instead of $device->serial
    
    $row = array();
    $row[] = $device->serial;
    $row[] = $device->resets;
    $row[] = unix_to_userlocaltime($device->access);
    $row[] = seconds_to_dayshours($device->uptime);
    $row[] = l(t('Remove'),  "device/deactivate/$device->device") . '<br>' .
             l(t('History'), "event/log/list/$device->serial") .
             $support_link;

    $rows[] = $row;
  }

  return drupal_render($form) .
    theme('logger_userdevices_list', $rows);
}

/**
 * Theme for displaying the list of devices.
 *
 * @param $items  An array of table rows.
 * @return the themed list of devices.
 */
function theme_logger_userdevices_list($items) {
      
  if (count($items) > 0) {

    $headers = array(t('Serial'), t('Resets'), t('Last heartbeat'), t('Uptime'), t('Operations'));
    return theme('table', $headers, $items);

  } else {
    return t('No devices available.');
  }
}

/**
 * Theme for displaying the list of user's sensors.
 *
 * @param $items An array of table rows.
 * @return the themed list of sensors.
 */
function theme_logger_usersensors_list($items) {

  if (count($items) > 0) {

    $headers = array(t('Sensor'), t('Token'), t('Status'), t('Type'), t('Function'), t('Operation'));
    return theme('table', $headers, $items);

  } else {
    return t('No sensors available.');
  }
}

/**
 * Define the administration settings form for the logger module
 */
function _logger_admin_settings() {
  //TODO
}