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
    ->fields('d', array('device', 'serial', 'access', 'uptime', 'type_id'))
    ->condition('d.uid', $user->uid, '=')
    ->orderBy('d.serial');

  $query->join('logger_device_type', 't', 'd.type_id = t.id');
  $query->addField('t', 'remote_support');

  $query->leftJoin('device_support_request', 'r', 'd.device = r.device');
  $query->addField('r', 'requested', 'support_requested');

  $query->join('logger_device_firmware', 'f', 'd.firmware_id = f.id');
  $query->addField('f', 'version', 'firmware_version');

  $devices = $query->execute();

  $rows = array();

  foreach ($devices as $device) {

    $links = l(t('Remove'), "device/deactivate/$device->device",
      array('attributes' => array('onclick' => "javascript: return confirm('" .
        t('This device and all its related data is going to be permanently removed. Do you want to proceed?') . "');"))) . '<br>' .

      l(t('History'), "event/log/list/$device->device") . '<br>' .
      l(t('Edit'), "device/edit/$device->device");

    if ($device->remote_support && user_access('request device remote support', $user)) {

      $support_link = $device->support_requested ?
        l(t('Suspend Support'), "device/support/suspend/$device->device") :
        l(t('Request Support'), "device/support/confirm/$device->device/$device->serial");

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
 
  if (logger_is_mobile_device()) {
    drupal_add_js("jQuery(window).resize( function() { resizeTable('" . LOGGER_TABLE_ID . "'); } );", $options);
    drupal_add_js("jQuery(document).ready( function() { resizeTable('" . LOGGER_TABLE_ID . "'); } );", $options);
  }

  $header = array(t('Serial'), t('Device Id'), t('Version'), t('Last heartbeat'), t('Uptime'), t('Operations'));

  $form = drupal_get_form('logger_deviceactivation_form');
  return drupal_render($form) . '<br><br><br>' . 

    theme_table(array('header' => $header, 'rows' => $rows, 'caption' => '',
      'empty' => t('No devices available.'), 'sticky' => FALSE, 'colgroups' => array(),
      'attributes' => array('id' => LOGGER_TABLE_ID, 'class' => 'resizable')));
}

/**
 * Builds the user sensors page.
 *
 * @return the user sensors page.
 */
function logger_usersensors_page() {

  global $user;

  drupal_set_title(t('My Sensors'));

  $header = array(t('Sensor'), t('Function'), t('Type'), t('Status'), t('Token'), t('Operation'));
  $options = array('type' => 'inline', 'scope' => 'header');
  $output = '<p>' . l(t('Add Virtual Sensor'), 'sensor/register') . '<br></p>';

  $devices = db_select('logger_devices', 'd')
    ->fields('d', array('serial', 'device'))
    ->condition('d.uid', $user->uid)
    ->execute();

  $is_mobile = logger_is_mobile_device();

  foreach ($devices as $device) {

    $table_id = LOGGER_TABLE_ID . "$device->serial";
    if ($is_mobile) {
      drupal_add_js("jQuery(window).resize( function() { resizeTable('" . $table_id . "'); } );", $options);
      drupal_add_js("jQuery(document).ready( function() { resizeTable('" . $table_id . "'); } );", $options);
    }

    $output .= '<br><br>' . t('Device') . ": $device->serial <br><br>";

    $query = db_select('logger_meters', 'lm');
    $query->join('logger_tokens', 'lt', 'lm.meter = lt.meter');
    $query->leftJoin('logger_meter_type', 'st', 'lm.type = st.id');
    $query->addField('st', 'name', 'type');

    $sensors = $query
      ->fields('lm', array('meter', 'function', 'virtual'))
      ->fields('lt', array('token', 'permissions'))
      ->condition('lm.device', $device->device)
      ->orderBy('lm.function', 'DESC')
      ->orderBy('lm.meter', 'DESC')
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
        $operations .= l(t('Activate'), 'sensor/edit/' . $sensor->meter) . '<br>';
      }
      else {
        $row[] = mb_wordwrap($sensor->function, 16, "<br>", TRUE);
        $row[] = t($sensor->type);
        $row[] = t('Active');
        $operations .= l(t('Edit'), 'sensor/edit/' . $sensor->meter) . '<br>';

        $operations .= ($sensor->virtual ?
          l(t('Remove'), 'sensor/remove/' . $sensor->meter) :
          l(t('Deactivate'), 'sensor/deactivate/' . $sensor->meter)) . '<br>';

        $operations .= l(t('Reset Token'), 'token/reset/' . $sensor->meter);
      }
      $row[] = wordwrap($sensor->token, 16, "\n", TRUE);
      $row[] = $operations;
      $rows[] = $row;
    }

    $output .= theme_table(array('header' => $header, 'rows' => $rows, 'caption' => '',
     'empty' => t('No sensors available.'), 'sticky' => FALSE, 'colgroups' => array(),
     'attributes' => array('id' => $table_id, 'class' => 'resizable')));
  }
  return $output;
}

/**
 * Builds the firmware releases page.
 *
 * @return the firmware releases page.
 */
function logger_firmwarereleases_page() {

  drupal_set_title(t('Firmware Releases'));

  $type_id = isset($_REQUEST['device_type_id']) ? $_REQUEST['device_type_id'] : 0;

  $query = db_select('logger_device_firmware', 'f')
    ->fields('f', array('id', 'version', 'release_time', 'build', 'tag', 'device_type_id', 'upgradable'))
    ->orderBy('f.version');

  $query->join('logger_device_type', 't', 'f.device_type_id = t.id');
  $query->addField('t', 'name', 'device_type');

  $query->leftJoin('logger_devices', 'd', 'd.firmware_id = f.id');
  $query->addExpression('COUNT(*)', 'total');
  $query->groupBy('f.id');

  if ($type_id > 0) {
    $query->condition('f.device_type_id', $type_id);
  }
  $releases = $query->execute();

  $header = array(t('Version'), t('Device Type'), t('Time'), 'Build', 'Tag', 'Upgradable', 'Total');
  $rows = array();
  foreach ($releases as $release) {
    $rows[] = array(
      $release->version,
      $release->device_type,
      $release->release_time,
      wordwrap($release->build, 16, "\n", TRUE),
      $release->tag,
      $release->upgradable ? t('Yes') : t('No'),
      $release->upgradable ?
        l($release->total, "firmware/upgrade", array('query' => array(
          'device_type_id' => $release->device_type_id,
          'from_version' => $release->id
        ))) : $release->total
    );
  }

  $form = drupal_get_form('logger_firmwarereleasesfilter_form', $type_id);

  return
    drupal_render($form) .

    theme_table(array('header' => $header, 'rows' => $rows, 'caption' => '',
      'empty' => t('No firmware releases available.'), 'sticky' => FALSE, 'colgroups' => array(),
      'attributes' => array('id' => 'firmwarereleases-list')));
}

/**
 * Builds the firmware upgrade page.
 *
 * @return the firmware upgrade page.
 */
function logger_firmwareupgrade_page() {

  drupal_set_title(t('Device Firmware Upgrade'));

  $form = drupal_get_form('logger_firmwareupgradefilter_form');

  return drupal_render($form);
}

/**
 * Builds the device edition page.
 *
 * @param $id  The device id.
 * @return the device edition page.
 */
function logger_device_edition_page($id) {

  $query = db_select('logger_devices', 'd')
    ->fields('d', array('device', 'serial', 'description'))
    ->condition('d.device', $id);

  $query->join('logger_device_type', 't', 'd.type_id = t.id');
  $query->addField('t', 'name', 'type_name');

  $query->join('logger_device_firmware', 'f', 'd.firmware_id = f.id');
  $query->addField('f', 'version', 'firmware_version');

  $device = $query->execute()->fetchObject();

  $form = drupal_get_form('logger_device_form', $device);

  return drupal_render($form);
}

/**
 * Builds the sensor edition page.
 *
 * @param $meter  The sensor id.
 * @return the sensor edition page.
 */
function logger_sensor_edition_page($meter = NULL) {

  if ($meter) {
    //TODO: use logger_get_sensors
    $query = db_select('logger_meters', 'm')
      ->fields('m', array('device', 'meter', 'external_id', 'type', 'function', 'forecast', 'latitude', 'longitude', 'virtual', 'price'))
      ->condition('m.meter', $meter);

    $query->join('logger_devices', 'd', 'd.device = m.device');
    $query->addField('d', 'type_id', 'device_type_id');

    $query->join('unit', 'un', 'm.unit_id = un.id');
    $query->addField('un', 'symbol', 'unit_symbol');
    $query->addField('un', 'name', 'unit_name');
    $query->addField('un', 'type_id', 'unit_type_id');

    $sensor = $query->execute()
      ->fetchObject();
  }
  else {
    $sensor = new stdClass();
    $sensor->device = NULL;
    $sensor->meter = NULL;
    $sensor->external_id = NULL;
    $sensor->type = ENERGY_CONSUMPTION_SENSOR_TYPE;
    $sensor->function = '';
    $sensor->forecast = 1;
    $sensor->latitude = LOGGER_DEFAULT_LATITUDE;
    $sensor->longitude = LOGGER_DEFAULT_LONGITUDE;
    $sensor->virtual = TRUE;
    $sensor->price = 0.18;
    $sensor->unit_symbol = 'Wh';
    $sensor->unit_name = t('Watt-hour');
    $sensor->unit_type_id = ENERGY_UNIT_TYPE_ID;
    $sensor->device_type_id = FLUKSO2_DEVICE_TYPE_ID;
  }

  $form = drupal_get_form('logger_sensor_form', $sensor);

  return drupal_render($form);
}

function logger_device_form($form, &$form_state, $device) {

  $form['type'] = array(
    '#type' => 'textfield',
    '#title' => t('Type'),
    '#description' => t("The device type."),
    '#maxlength' => 32,
    '#size' => 32,
    '#required' => FALSE,
    '#default_value' => $device->type_name,
    '#attributes' => array('readonly' => 'readonly')
  );

  $form['firmware'] = array(
    '#type' => 'textfield',
    '#title' => t('Firmware Version'),
    '#description' => t("The device firmware version."),
    '#maxlength' => 32,
    '#size' => 32,
    '#required' => FALSE,
    '#default_value' => $device->firmware_version,
    '#attributes' => array('readonly' => 'readonly')
  );

  $form['device'] = array(
    '#type' => 'textfield',
    '#title' => t('Id'),
    '#description' => t("The device id."),
    '#maxlength' => 32,
    '#size' => 32,
    '#required' => TRUE,
    '#default_value' => $device->device,
    '#attributes' => array('readonly' => 'readonly')
  );

  $form['serial'] = array(
    '#type' => 'textfield',
    '#title' => t('Serial'),
    '#description' => t("The device serial number."),
    '#maxlength' => 32,
    '#size' => 32,
    '#required' => TRUE,
    '#default_value' => $device->serial,
    '#attributes' => array('readonly' => 'readonly')
  );

  $form['description'] = array(
    '#type' => 'textarea',
    '#title' => t('Description'),
    '#description' => t("The device description."),
    '#maxlength' => 60,
    '#size' => 60,
    '#required' => TRUE,
    '#default_value' => $device->description
  );

  $form['submit'] = array(
    '#type' => 'submit',
    '#value' => t('Save'),
  );
  return $form;
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

  if ($sensor->device_type_id == RASPBERRY_PI_DEVICE_TYPE_ID || $sensor->device_type_id == LIBKLIO_DEVICE_TYPE_ID) {
    $form['external_id'] = array(
      '#type' => 'textfield',
      '#title' => t('External Id'),
      '#description' => t("The sensor external id."),
      '#maxlength' => 32,
      '#size' => 32,
      '#required' => TRUE,
      '#default_value' => $sensor->external_id,
      '#attributes' => array('readonly' => 'readonly')
    );
  }

  $form['type'] = array(
    '#type' => 'select',
    '#title' => t('Type'),
    '#description' => t('The sensor type.'),
    '#options' => logger_get_sensor_types_options(NULL, $sensor->unit_type_id),
    '#default_value' => $sensor->type,
    '#required' => TRUE,
    '#attributes' => array(
      'readonly' => 'readonly',
      'onchange' => 'showSensorForecastFields()'
    )
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
    '#field_suffix' => ' / kWh',
    '#access' => $sensor->unit_type_id != TEMPERATURE_SENSOR_TYPE //FIXME: find a better way of doing this filter
  );

  $form['function'] = array(
    '#type' => 'textfield',
    '#title' => t('Function'),
    '#description' => t("Enter the sensor's function name."),
    '#maxlength' => 32,
    '#size' => 32,
    '#required' => TRUE,
    '#default_value' => $sensor->function
  );

  $form['unit'] = array(
    '#type' => 'textfield',
    '#title' => t('Unit'),
    '#description' => t("The sensor's unit."),
    '#maxlength' => 32,
    '#size' => 32,
    '#required' => TRUE,
    '#default_value' => $sensor->unit_name . ' (' . $sensor->unit_symbol . ')',
    '#attributes' => array('readonly' => 'readonly')
  );

  //If creating or editing a virtual sensor
  if ($sensor->virtual) {

    $meters_options = logger_get_meters_options(array(ENERGY_CONSUMPTION_SENSOR_TYPE, ENERGY_PRODUCTION_SENSOR_TYPE));
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
    drupal_add_css(LOGGER_REL_PATH . '/js/openlayers/theme/default/style.css');
    drupal_add_css(LOGGER_REL_PATH . '/js/openlayers/style.css');

    $options = array('type' => 'file', 'scope' => 'header', 'group' => JS_LIBRARY, 'weight' => 150);
    drupal_add_js(LOGGER_REL_PATH . '/js/openlayers/OpenLayers-min.js', $options);

    $options['weight']++;
    drupal_add_js(LOGGER_REL_PATH . '/js/maps-min.js', $options);

    $options['weight']++;
    drupal_add_js(LOGGER_REL_PATH . '/js/logger-min.js', $options);

    $form['prev_forecast'] = array(
      '#type' => 'hidden',
      '#default_value' => array($sensor->forecast),
      '#prefix' => '<div id="forecast-options">'
    );

    $form['forecast'] = array(
      '#type' => 'checkboxes',
      '#title' => t('Forecast'),
      '#description' => t('Check this option if you want to see measurement forecast for this sensor. ' . 
        'The forecast data will be available only if this sensor has been sending measurements for at least 4 weeks. ' . 
        'If so, the first forecast data will be available 24 hours after this change.'),
      '#options' => array(
        '1' => t('Activate'),
      ),
      '#default_value' => array($sensor->forecast),
      '#suffix' => '</div>'
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

  $form['submit'] = array(
    '#type' => 'submit',
    '#value' => t("Save"),
  );

  drupal_add_js("jQuery(document).ready( function() { showSensorForecastFields(); } );", array('type' => 'inline', 'scope' => 'footer'));

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

function logger_firmwarereleasesfilter_form($form, &$form_state, $type_id) {

  $types = logger_get_device_types_options();
  $types[0] = t('All');

  $form['device_type_id'] = array(
    '#type' => 'select',
    '#title' => t('Device Type'),
    '#description' => t('Select the device type.'),
    '#options' => $types,
    '#default_value' => $type_id,
    '#required' => TRUE,
    '#attributes' => array('onchange' => "document.getElementById('logger-firmwarereleasesfilter-form').submit();")
  );

  $form['submit'] = array(
    '#type' => 'submit',
    '#value' => t('Filter')
  );
  return $form;
}

function logger_firmwareupgradefilter_form($form, &$form_state) {

  $device_type_id = isset($_REQUEST['device_type_id']) ? $_REQUEST['device_type_id'] : FLUKSO2_DEVICE_TYPE_ID;
  $from_version = isset($_REQUEST['from_version']) ? $_REQUEST['from_version'] : 9; //FIXME: create constant

  //Select device types with upgradable firmware releases
  $upgradable = db_select('logger_device_firmware', 'f')
    ->condition('f.upgradable', 1);
  $upgradable->addExpression('1');
  $upgradable->where('t.id = f.device_type_id');
  $query = db_select('logger_device_type', 't')
    ->fields('t', array('id', 'name'))
    ->orderBy('t.name');
  $query->exists($upgradable);
  $result = $query->execute();

  $device_types = array();
  foreach ($result as $type) {
    $device_types[$type->id] = $type->name;
  }

  //Select upgradable firmware releases of a certain device type
  $result = db_select('logger_device_firmware', 'f')
    ->fields('f', array('id', 'version'))
    ->condition('f.device_type_id', $device_type_id)
    ->condition('f.upgradable', 1)
    ->orderBy('f.version')
    ->execute();

  $from_versions = array();
  foreach($result as $firmware) {
    $from_versions[$firmware->id] = $firmware->version;
  }
  reset($from_versions);
  $from_version = isset($from_versions[$from_version]) ? $from_version : key($from_versions);

  //Select upgradable firmware releases that can be used as target
  $result = db_select('logger_device_firmware', 'f')
    ->fields('f', array('id', 'version'))
    ->condition('f.device_type_id', $device_type_id)
    ->condition('f.upgradable', 1)
    ->condition('f.version', $from_versions[$from_version], '>')
    ->orderBy('f.version')
    ->execute();

  $to_versions = array();
  foreach($result as $firmware) {
    $to_versions[$firmware->id] = $firmware->version;
  }

  //List of devices
  $query = db_select('logger_devices', 'd')
    ->fields('d', array('device', 'serial'))
    ->orderBy('d.serial');
  $query->leftJoin('logger_firmware_upgrade_request', 'r', 'r.device = d.device');
  $query->addField('r', 'requested');
  $query->addField('r', 'approved');

  $query->leftJoin('logger_device_firmware', 'f', 'f.id = r.firmware_id');
  $query->addField('f', 'version', 'to_version');

  $query->join('logger_device_firmware', 'w', 'w.id = d.firmware_id');
  $query->addField('w', 'version', 'from_version');

  if ($from_version > 0) {
    $query->condition('d.firmware_id', $from_version);
  }
  elseif ($device_type_id > 0) {
    $query->condition('w.device_type_id', $device_type_id);
  }

  $devices = $query->execute();

  $header = array(t('Serial'), t('Id'), t('From Version'), t('To Version'), t('Requested'), t('Approved'));
  $rows = array();
  foreach ($devices as $device) {
    $rows[$device->device] = array(
      $device->serial,
      wordwrap($device->device, 16, "\n", TRUE),
      $device->from_version,
      empty($device->to_version) ? '' : $device->to_version,
      $device->requested > 0 ? date('d-M H:i', $device->requested) : '',
      $device->approved > 0 ? date('d-M H:i', $device->approved) : ''
    );
  }

  $submit = "document.getElementById('logger-firmwareupgradefilter-form').submit()";

  $form['device_type_id'] = array(
    '#type' => 'select',
    '#title' => t('Device Type'),
    '#description' => t('Select the device type.'),
    '#options' => $device_types,
    '#default_value' => $device_type_id,
    '#required' => TRUE,
    '#attributes' => array('onchange' =>
      "document.getElementById('edit-from-version').selectedIndex = -1; " .
      "document.getElementById('edit-to-version').selectedIndex = -1; $submit;") 
  );

  $form['from_version'] = array(
    '#type' => 'select',
    '#title' => t('From Version'),
    '#description' => t('Select the current firmware version.'),
    '#options' => $from_versions,
    '#default_value' => $from_version,
    '#attributes' => array('onchange' =>
      "document.getElementById('edit-to-version').selectedIndex = -1; $submit;")
  );

  $form['to_version'] = array(
    '#type' => 'select',
    '#title' => t('To Version'),
    '#description' => t('Select the target firmware version.'),
    '#options' => $to_versions,
    '#empty_option' => t('None'),
  );

  $form['table'] = array(
    '#type' => 'tableselect',
    '#header' => $header,
    '#options' => $rows,
    '#multiple' => TRUE,
    '#empty' => t('No devices found.'),
  );

  $form['submit'] = array(
    '#type' => 'submit',
    '#value' => t('Update')
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

  if (!logger_sensor_function_validate($function, $meter)) {

    form_set_error('function', t('Another sensor was found with the function name: %function_name. ' .
      'Please inform another name which has not yet been used.', array('%function_name' => $function)));
  }

  if (isset($form_state['values']['price']) && !($form_state['values']['price'] > 0)) {
    form_set_error('price', t('The informed price is invalid.'));
  }

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
  }

  if (!$in_edition) {
    $aggregated_meters = get_selected_combobox_options($form_state['values']['aggregated_meters']);

    if (count($aggregated_meters) < 2) {
      form_set_error('aggregated_meters', t('Please, select at least 2 meters for aggregation.'));
    }
  }
}

function logger_firmwareupgradefilter_form_validate($form, &$form_state) {

  if (isset($form_state['values']['from_version']) && isset($form_state['values']['to_version']) && $form_state['values']['from_version'] > 0 && $form_state['values']['to_version'] > 0){

    $from = logger_get_device_firmware($form_state['values']['from_version']);
    $to = logger_get_device_firmware($form_state['values']['to_version']);

    if (!$from || !$to || $from->upgradable != 1 || $to->upgradable != 1 || !($from->version < $to->version) ||
        $from->device_type_id != $form_state['values']['device_type_id'] || $to->device_type_id != $from->device_type_id) {

      form_set_error('to_version', t('Invalid firmware upgrade.'));
    }
  }
}

function logger_get_device_firmware($id) {

  return db_select('logger_device_firmware', 'f')
    ->fields('f', array('id', 'version', 'upgradable', 'device_type_id'))
    ->condition('f.id', $id)
    ->execute()
    ->fetchObject();
}


function logger_device_form_submit($form, &$form_state) {

  db_update('logger_devices')
    ->fields(array(
      'description' => $form_state['values']['description']
    ))
    ->condition('device', $form_state['values']['device'])
    ->execute();

  $form_state['redirect'] = 'device/mylist';
}

function logger_deviceactivation_form_submit($form, &$form_state) {

  $form_state['redirect'] = 'device/activate/' . $form_state['values']['code'];
}

function logger_sensor_form_submit($form, &$form_state) {

  //TODO: refactor this function

  global $user;

  $function = $form_state['values']['function'];
  $type = $form_state['values']['type'];
  $price = isset($form_state['values']['price']) ? $form_state['values']['price'] : 0;

  //If edition
  if (isset($form_state['values']['meter'])) {

    $meter = $form_state['values']['meter'];
    $fields = array('function' => $function, 'type' => $type);

    if ($price > 0) {
      $fields['price'] = $price;
    }

    $forecast = isset($form_state['values']['forecast']) ? $form_state['values']['forecast'][1] : 0;
    $latitude = isset($_REQUEST['coordinates_latitude']) ? $_REQUEST['coordinates_latitude'] : 0;
    $longitude = isset($_REQUEST['coordinates_longitude']) ? $_REQUEST['coordinates_longitude'] : 0;

    $fields['forecast'] = $forecast;
    $fields['latitude'] = $latitude;
    $fields['longitude'] = $longitude;

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

  if (isset($form_state['values']['prev_forecast'])) {

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

function logger_firmwarereleasesfilter_form_submit($form, &$form_state) {

  $form_state['redirect'] = array('firmware/list', array(
    'query' => array(
      'device_type_id' => $form_state['values']['device_type_id'],
  )));
}

function logger_firmwareupgradefilter_form_submit($form, &$form_state) {

  if (isset($form_state['values']['to_version'])) {

    $selected = array_filter($form_state['values']['table']);

    foreach ($selected as $device) {

      db_delete('logger_firmware_upgrade_request')
        ->condition('device', $device)
        ->execute();

      if ($form_state['values']['to_version'] > 0) {

        db_insert('logger_firmware_upgrade_request')
          ->fields(array(
            'device' => $device,
            'firmware_id' => $form_state['values']['to_version'], 
            'requested' => time()
          ))
          ->execute();
      }
    }
  }

  $form_state['redirect'] = array('firmware/upgrade', array('query' => array(
    'device_type_id' => $form_state['values']['device_type_id'],
    'from_version' => $form_state['values']['from_version']
  )));
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

  logger_free_support_slot($device);

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

  logger_remove_firmware_upgrade_request($device);

  //FIXME: define a hook
  notification_remove_by_device($device);

  logger_remove_firmware_upgrade_request($device);

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
 * Creates a copy of a device and its sensors. This function is used for creating demo users' devices.
 * The sensors' RRD files are actually symbolic links that point to the original ones.
 *
 * @param $device_id  The id of the device to be cloned.
 */
function logger_device_clone($device_id) {

  $device = db_select('logger_devices', 'd')
    ->fields('d')
    ->condition('device', $device_id)
    ->execute()
    ->fetchObject();

  $new_device_id = md5(uniqid(rand(), TRUE));
  $new_serial = time() + 5000000000;

  db_insert('logger_devices')
    ->fields(array(
      'device' => $new_device_id,
      'serial' => $new_serial,
      'uid' => 0,
      'sha' => $new_device_id,
      'created' => $device->created,
      'access' => $device->access,
      'version' => $device->version,
      'resets' => $device->resets,
      'country' => $device->country,
      'firmware_id' => 0
    ))
    ->execute();

  $sensors = db_select('logger_meters', 'm')
    ->fields('m')
    ->condition('device', $device_id)
    ->condition('virtual', 0)
    ->execute();

  foreach ($sensors as $sensor) {

    $new_meter = md5(uniqid(rand(), TRUE));

    db_insert('logger_meters')
      ->fields(array(
        'device' => $new_device_id,
        'meter' => $new_meter,
        'uid' => 0,
        'function' => empty($sensor->function) ? NULL : substr("C $sensor->function", 0, 16),
        'created' => $sensor->created,
        'access' => $sensor->access,
        'price' => $sensor->price,
        'forecast' => $sensor->forecast,
        'latitude' => $sensor->latitude,
        'longitude' => $sensor->longitude
      ))
      ->execute();

    if ($sensor->forecast == 1) {
      pvcast_create_plant($new_meter, $sensor->latitude, $sensor->longitude);
    }

    $new_token = md5(uniqid(rand(), TRUE));

    db_insert('logger_tokens')
      ->fields(array(
        'token' => $new_token,
        'meter' => $new_meter,
        'permissions' => 62
    ))
    ->execute();

    exec('/bin/ln -s ' . LOGGER_PATH . "/data/base/$sensor->meter.rrd " . LOGGER_PATH . "/data/base/$new_meter.rrd");
  }

  return t("Device %device has been successfuly cloned. The cloned device id is: %new_device_id.", array(
    '%device' => $device,
    '%new_device_id' => $new_device_id
  ));
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

  db_delete('logger_meter_storage_period')
    ->condition('meter', $meter)
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
function logger_sensor_add($meter, $uid, $device, $created, $token, $permissions, $function = NULL, $type = ENERGY_CONSUMPTION_SENSOR_TYPE, $virtual = 1, $forecast = 0, $latitude = LOGGER_DEFAULT_LATITUDE, $longitude = LOGGER_DEFAULT_LONGITUDE, $price = 0.18) {

  module_load_include('inc', 'logger', 'logger.rrd');

  $error = db_insert('logger_meters')
    ->fields(array(
      'meter' => $meter,
      'uid' => $uid,
      'device' => $device,
      'external_id' => $meter,
      'created' => $created,
      'function' => $function,
      'type' => $type,
      'virtual' => $virtual,
      'unit_id' => WH_UNIT_ID,
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

  if (!$error) {

    foreach (logger_rrd_get_storage_periods() as $resolution => $period) {

      $error = db_insert('logger_meter_storage_period')
        ->fields(array(
          'meter' => $meter,
          'resolution' => $resolution,
          'period' => $period
        ))
        ->execute();

      if ($error) {
        break;        
      }
    }
  }
  return !$error;
}

/**
 * Resets a sensor token.
 */
function logger_token_reset($meter) {

  db_delete('logger_tokens')
    ->condition('meter', $meter)
    ->execute();

  db_insert('logger_tokens')
    ->fields(array(
      'token' => md5(uniqid(rand(), TRUE)),
      'meter' => $meter,
      'permissions' => 62
    ))
    ->execute();

  drupal_goto("sensor/mylist");
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
 * Removes a firmware upgrade request.
 *
 * @param $device The device id.
 */
function logger_remove_firmware_upgrade_request($device) {

  db_delete('logger_firmware_upgrade_request')
    ->condition('device', $device)
    ->execute();
}

/**
 * Approves a firmware upgrade.
 */
function logger_firmware_upgrade_approve($device) {

  db_update('logger_firmware_upgrade_request')
    ->fields(array(
      'approved' => time()
    ))
    ->condition('device', $device)
    ->execute();

  drupal_set_message('The firmware of your device is going to be upgraded. Please, make sure it remains on during the next 2 hours.', 'warning');

  drupal_goto("energy");
}

/**
 * Disapproves a firmware upgrade.
 */
function logger_firmware_upgrade_disapprove($device) {

  logger_remove_firmware_upgrade_request($device);

  drupal_set_message('The firmware upgrade has been rejected.', 'warning');

  drupal_goto("energy");
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
