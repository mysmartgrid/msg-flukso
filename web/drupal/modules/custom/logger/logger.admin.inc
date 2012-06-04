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

  drupal_set_title(t('My Devices'));

  $query = db_select('logger_devices', 'd')
    ->fields('d', array('device', 'serial', 'access', 'firmware_version', 'uptime'))
    ->condition('d.uid', $user->uid, '=')
    ->orderBy('d.serial');
  $query->leftJoin('device_support_request', 'r', 'd.device = r.device');
  $query->addField('r', 'requested', 'support_requested');

  $devices = $query->execute();

  $rows = array();

  foreach ($devices as $device) {

    $links = l(t('Remove'), "device/deactivate/$device->device",
      array('attributes' => array('onclick' => "javascript: return confirm('" .
        t('This device and all its related data is going to be permanently removed. Do you want to proceed?') . "');"))) . '<br>' .

      l(t('History'), "event/log/list/$device->device");

    //TODO: use a version field
    if ($device->serial > 100000000 && user_access('request device remote support', $user)) {

      $support_link = $device->support_requested ?
        l(t('Suspend Support'), "device/support/suspend/$device->device") :
        l(t('Request Support'), "device/support/confirm/$device->device");

      $links .= "<br><span style='white-space: nowrap;'>$support_link</span>";
    }

    $row = array();
    $row[] = $device->serial;
    $row[] = wordwrap($device->device, 16, "\n", TRUE);
    $row[] = $device->firmware_version;
    $row[] = unixtime_to_localtime($device->access);
    $row[] = seconds_to_dayshours($device->uptime);
    $row[] = $links;

    $rows[] = $row;
  }
 
  $header = array(t('Serial'), t('Device Id'), t('Version'), t('Last heartbeat'), t('Uptime'), t('Operations'));
  $options = array('type' => 'inline', 'scope' => 'header');

  drupal_add_js("jQuery(window).resize( function() { resizeLegend('" . LOGGER_LEGEND_TABLE_ID . "'); } );", $options);
  drupal_add_js("jQuery(document).ready( function() { resizeLegend('" . LOGGER_LEGEND_TABLE_ID . "'); } );", $options);

  $form = drupal_get_form('logger_deviceactivation_form');
  return drupal_render($form) . '<br><br><br>' . 

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

  drupal_set_title(t('My Sensors'));

  $query = db_select('logger_meters', 'lm');
  $query->join('logger_tokens', 'lt', 'lm.meter = lt.meter');
  $query->leftJoin('logger_meter_type', 'st', 'lm.type = st.id');
  $query->addField('st', 'name', 'type');

  $sensors = $query
    ->fields('lm', array('meter', 'function', 'virtual'))
    ->fields('lt', array('token', 'permissions'))
    ->condition('lm.uid', $user->uid, '=')
    ->orderBy('lm.function', 'DESC')
    ->execute();

  $rows = array();
  foreach ($sensors as $sensor) {

    $row = array();
    $row[] = wordwrap($sensor->meter, 16, "\n", TRUE);
    $operations = '';

    if ($sensor->function == NULL) {
      $row[] = '';
      $row[] = '';
      $row[] = t('Inactive');
      $operations = l(t('Activate'), 'sensor/edit/' . $sensor->meter);
    }
    else {
      $row[] = $sensor->function;
      $row[] = t($sensor->type);
      $row[] = t('Active');
      $operations .= l(t('Edit'), 'sensor/edit/' . $sensor->meter) . '<br>';

      $operations .= $sensor->virtual ? 
        l(t('Remove'), 'sensor/remove/' . $sensor->meter) :
        l(t('Deactivate'), 'sensor/deactivate/' . $sensor->meter);
    }
    $row[] = wordwrap($sensor->token, 16, "\n", TRUE);
    $row[] = $operations;
    $rows[] = $row;
  }

  $header = array(t('Sensor'), t('Function'), t('Type'), t('Status'), t('Token'), t('Operation'));
  $options = array('type' => 'inline', 'scope' => 'header');

  drupal_add_js("jQuery(window).resize( function() { resizeLegend('" . LOGGER_LEGEND_TABLE_ID . "'); } );", $options);
  drupal_add_js("jQuery(document).ready( function() { resizeLegend('" . LOGGER_LEGEND_TABLE_ID . "'); } );", $options);

  $caption = user_has_role('PV Producer') ? '<p>' . l(t('Add Virtual Sensor'), 'sensor/register') . '<br><br></p>' : '';

  return theme_table(array('header' => $header, 'rows' => $rows, 'caption' => $caption,
   'empty' => t('No sensors available.'), 'sticky' => FALSE, 'colgroups' => array(),
   'attributes' => array('id' => LOGGER_LEGEND_TABLE_ID, 'class' => 'resizable')));
}

/**
 * Builds the sensor edition page.
 *
 * @param $meter  The sensor id.
 * @return the sensor edition page.
 */
function logger_sensor_edition_page($meter = NULL) {

  if ($meter) {
    $sensor = db_select('logger_meters', 'm')
      ->fields('m', array('device', 'meter', 'type', 'function', 'forecast', 'latitude', 'longitude', 'virtual', 'price'))
      ->condition('m.meter', $meter, '=')
      ->execute()
      ->fetchObject();
  }
  else {
    $sensor = new stdClass();
    $sensor->device = NULL;
    $sensor->meter = NULL;
    $sensor->type = CONSUMPTION_SENSOR_TYPE;
    $sensor->function = '';
    $sensor->forecast = 0;
    $sensor->latitude = LOGGER_DEFAULT_LATITUDE;
    $sensor->longitude = LOGGER_DEFAULT_LONGITUDE;
    $sensor->virtual = TRUE;
    $sensor->price = 0.18;
  }

  $form = drupal_get_form('logger_sensor_form', $sensor);

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

function logger_sensor_form($form, &$form_state, $sensor = NULL) {

  $options = logger_get_devices_options();
  $form['device'] = array(
    '#type' => 'select',
    '#title' => t('Device'),
    '#description' => t('Select the device where this sensor belongs.'),
    '#default_value' => $sensor->device,
    '#required' => TRUE
  );
  $form['device']['#options'] = $sensor->virtual ? $options : array($sensor->device => $options[$sensor->device]);

  if ($sensor->meter) {
    $form['meter'] = array(
      '#type' => 'textfield',
      '#title' => t('Id'),
      '#description' => t("The sensor meter id."),
      '#maxlength' => 32,
      '#size' => 32,
      '#required' => TRUE,
      '#default_value' => $sensor->meter,
      '#attributes' => array('readonly' => 'readonly')
    );
  }

  $form['type'] = array(
    '#type' => 'select',
    '#title' => t('Type'),
    '#description' => t('The sensor type.'),
    '#options' => logger_get_sensor_types_options(),
    '#default_value' => $sensor->type,
    '#required' => TRUE,
    '#attributes' => array('readonly' => 'readonly')
  );

  $form['price'] = array(
    '#type' => 'textfield',
    '#title' => t('Energy Price'),
    '#description' => t("Enter the price of the energy measured by this sensor."),
    '#maxlength' => 6,
    '#size' => 6,
    '#field_prefix' => '€ ',
    '#default_value' => $sensor->price,
    '#required' => TRUE,
    '#field_suffix' => ' / kWh'
  );

  $form['function'] = array(
    '#type' => 'textfield',
    '#title' => t('Function'),
    '#description' => t("Enter the sensor's function name."),
    '#maxlength' => 16,
    '#size' => 16,
    '#required' => TRUE,
    '#default_value' => $sensor->function
  );

  if (user_has_role('PV Producer')) {

    //If creating or editing a virtual sensor
    if ($sensor->virtual) {

      $meters_options = logger_get_meters_options(array(CONSUMPTION_SENSOR_TYPE, PRODUCTION_SENSOR_TYPE));
      $aggregated = logger_get_aggregated_sensors($sensor->meter);

      $selected_options = array();
      foreach ($aggregated as $agg_sensor) {
        $selected_options[] = $agg_sensor->meter;
      }

      $form['aggregated_meters'] = array(
        '#type' => 'checkboxes',
        '#title' => t('Aggregated Sensors'),
        '#description' => t('Select the sensors that are aggregated by this virtual sensor.'),
        '#default_value' => $selected_options,
        '#options' => $meters_options,
        '#required' => TRUE
      );
    }
    else {
      //TODO: enable these fields for experts
      $form['prev_forecast'] = array(
        '#type' => 'hidden',
        '#default_value' => array($sensor->forecast)
      );

      $form['forecast'] = array(
        '#type' => 'checkboxes',
        '#title' => t('Forecast'),
        '#description' => t('Check this option if you want to receive measurement forecast for this sensor.'),
        '#options' => array(
          '1' => t('Activate'),
        ),
        '#default_value' => array($sensor->forecast)
      );

      $form['coordinates'] = array(
        '#type' => 'textfield',
        '#theme' => 'logger_geo_location',
        '#title' => t('Location'),
        '#description' => t('Please, click on the map to indicate the exact location of the sensor. Use the icons on the top to control zoom and move the map.'),
        '#default_value' => $sensor->latitude . ',' . $sensor->longitude,
        '#text-align' => 'center'
      );
    }
  }

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

  $in_edition = isset($form_state['values']['meter']);
  $function = $form_state['values']['function'];
  $meter = $in_edition ? $form_state['values']['meter'] : NULL;
  $price = $form_state['values']['price'];

  if (!logger_sensor_function_validate($function, $meter)) {

    form_set_error('function', t('Another sensor was found with the function name: %function_name. ' .
      'Please inform another name which has not yet been used.', array('%function_name' => $function)));
  }

  if (!($price > 0)) {
    form_set_error('price', t('The informed price is invalid.'));
  }

  if (user_has_role('PV Producer')) {

    if (isset($form_state['values']['forecast'])) {
      $forecast = $form_state['values']['forecast'][1];
      $latitude = $_REQUEST['coordinates_latitude'];
      $longitude = $_REQUEST['coordinates_longitude'];
      $type = $form_state['values']['type'];

      if (!($latitude >= -90 && $latitude <= 90 && $longitude >= -90 && $longitude <= 90)) {

        form_set_error('coordinates', t('Invalid geographic coordinates.'));
      }

      if ($forecast && !($latitude > 47.25 && $latitude < 55.09 && $longitude > 5.8 && $longitude < 15.069)) {

        form_set_error('coordinates', t('The forecast service is available only for sensors located in Germany.'));
      }

      if ($forecast && $type != PRODUCTION_SENSOR_TYPE) {

        form_set_error('forecast', t('The forecast service is available only for power production sensors.'));
      }
    }

    if (!$in_edition) {
      $aggregated_meters = get_selected_combobox_options($form_state['values']['aggregated_meters']);

      if (count($aggregated_meters) < 2) {
        form_set_error('aggregated_meters', t('Please, select at least 2 meters for aggregation.'));
      }
    }
  }
}

function logger_deviceactivation_form_submit($form, &$form_state) {

  $form_state['redirect'] = 'device/activate/' . $form_state['values']['code'];
}

function logger_sensor_form_submit($form, &$form_state) {

  //TODO: refactor this function

  global $user;

  $is_procuder = user_has_role('PV Producer');
  $function = $form_state['values']['function'];
  $type = $form_state['values']['type'];
  $price = $form_state['values']['price'];

  //If edition
  if (isset($form_state['values']['meter'])) {

    $meter = $form_state['values']['meter'];

    $fields = array(
      'function' => $function,
      'type' => $type,
      'price' => $price
    );
    if ($is_procuder) {
      $forecast = isset($form_state['values']['forecast']) ? $form_state['values']['forecast'][1] : 0;
      $latitude = isset($_REQUEST['coordinates_latitude']) ? $_REQUEST['coordinates_latitude'] : 0;
      $longitude = isset($_REQUEST['coordinates_longitude']) ? $_REQUEST['coordinates_longitude'] : 0;

      $fields['forecast'] = $forecast;
      $fields['latitude'] = $latitude;
      $fields['longitude'] = $longitude;
    }

    db_update('logger_meters')
      ->fields($fields)
      ->condition('meter', $meter)
      ->execute();
  }
  //If creation
  else {
    $meter = md5(uniqid(rand(), TRUE));
    $token = md5(uniqid(rand(), TRUE));
    $device = $form_state['values']['device'];
    $forecast = isset($form_state['values']['forecast']) ? $form_state['values']['forecast'][1] : 0;
    $latitude = isset($_REQUEST['coordinates_latitude']) ? $_REQUEST['coordinates_latitude'] : 0;
    $longitude = isset($_REQUEST['coordinates_longitude']) ? $_REQUEST['coordinates_longitude'] : 0;

    logger_sensor_add($meter, $user->uid, $device, time(), $token, 62, $function, $type, 1, $forecast, $latitude, $longitude, $price);
  }

  if (isset($form_state['values']['aggregated_meters'])) {

    $aggregated_meters = get_selected_combobox_options($form_state['values']['aggregated_meters']);
    logger_set_virtual_sensor_aggregations($meter, $aggregated_meters);
  }

  if ($is_procuder) {
    $prev_forecast = $form_state['values']['prev_forecast'];

    if ($prev_forecast == 1 && $forecast == 0) {
      pvcast_delete_plant($meter);
    }
    elseif ($prev_forecast == 0 && $forecast == 1) {
      pvcast_create_plant($meter, $latitude, $longitude);
    }
    elseif ($prev_forecast == 1 && $forecast == 1) {
      pvcast_update_plant($meter, $latitude, $longitude);
    }
  }

  $form_state['redirect'] = "sensor/mylist";
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

  // check whether the user is already a true fluksonian
  // if not, generate the proper entries in {users_roles} and {logger_users}
  if (!user_has_role('fluksonian')) {

    db_insert('users_roles')
      ->fields(array(
        'uid' => $user->uid,
        'rid' => get_role_id('fluksonian')
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

  drupal_goto("device/mylist");
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

  drupal_goto("device/mylist");
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

    logger_sensor_remove($sensor->meter);

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

  db_update('logger_meters')
    ->fields(array(
      'function' =>  NULL,
      'type' =>  NULL,
    ))
    ->condition('meter', $meter)
    ->execute();

  drupal_goto("sensor/mylist");
}

/**
 * Removes a sensor and all its related data.
 * 
 * @param $meter     The sensor id.
 * @param $next_url  The URL to go, after removal (optional).
 */
function logger_sensor_remove($meter, $next_url = NULL) {

  db_delete('logger_tokens')
    ->condition('meter', $meter)
    ->execute();

  db_delete('logger_aggregated_meters')
    ->condition(db_or()
      ->condition('meter', $meter)
      ->condition('virtual_meter', $meter)
    )
    ->execute();

  db_delete('logger_meters')
    ->condition('meter', $meter)
    ->execute();

  pvcast_delete_plant($meter);

  if ($next_url) {
    drupal_goto($next_url);
  }
}

/**
 * Adds a new device sensor.
 */
function logger_sensor_add($meter, $uid, $device, $created, $token, $permissions, $function = NULL, $type = CONSUMPTION_SENSOR_TYPE, $virtual = 1, $forecast = 0, $latitude = LOGGER_DEFAULT_LATITUDE, $longitude = LOGGER_DEFAULT_LONGITUDE, $price = 0.18) {

  $error = db_insert('logger_meters')
    ->fields(array(
      'meter' => $meter,
      'uid' => $uid,
      'device' => $device,
      'created' => $created,
      'function' => $function,
      'type' => $type,
      'virtual' => $virtual,
      'unit' => 'watt',
      'forecast' => $forecast,
      'latitude' => $latitude,
      'longitude' => $longitude,
      'price' => $price
    ))
    ->execute();

  if (!$error) {
    $error = db_insert('logger_tokens')
      ->fields(array(
        'token' => $token,
        'meter' => $meter,
        'permissions' => $permissions
      ))
      ->execute();
  }
  return !$error;
}

/**
 * Sets the sensors aggregated by a virtual sensor.
 */
function logger_set_virtual_sensor_aggregations($virtual_meter, $aggregated_meters) {

  db_delete('logger_aggregated_meters')
    ->condition('virtual_meter', $virtual_meter)
    ->execute();

  foreach ($aggregated_meters as $agg_meter) {
    db_insert('logger_aggregated_meters')
      ->fields(array(
        'virtual_meter' => $virtual_meter,
        'meter' => $agg_meter
      ))
      ->execute();
  }
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
    $query->condition('m.function', $function, '=')
      ->condition('m.uid', $user->uid, '=');

    if ($meter) {
      $query->condition('m.meter', $meter, '<>');
    }

    $count = $query->execute()->fetchField();
  }
  return $count == 0;
}
