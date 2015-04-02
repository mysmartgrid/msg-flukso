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
    ->condition('d.uid', $user->uid)
    ->orderBy('d.serial');

  $query->join('logger_device_type', 't', 'd.type_id = t.id');
  $query->addField('t', 'name', 'type_name');
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
    $row[] = $device->type_name;
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

  $header = array(t('Serial'), t('Device Type'), t('Version'), t('Last heartbeat'), t('Uptime'), t('Operations'));

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
          l(t('Remove'), 'sensor/remove/' . $sensor->meter,
            array(
              'query' => array('next_url' => 'sensor/mylist'),
              'attributes' => array('onclick' => "javascript: return confirm('" . t('This virtual sensor is going to be removed. Do you want to proceed?') . "');"))) :

          l(t('Deactivate'), 'sensor/deactivate/' . $sensor->meter,
            array('attributes' => array('onclick' => "javascript: return confirm('" . t('This sensor is going to be deactivated. Do you want to proceed?') . "');")))) . '<br>';
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
function logger_firmwarelist_page() {

  drupal_set_title(t('Firmware Releases'));

  $type_id = isset($_REQUEST['device_type_id']) ? $_REQUEST['device_type_id'] : 0;

  $query = db_select('logger_device_firmware', 'f')
    ->fields('f', array('id', 'version', 'release_time', 'tag', 'device_type_id', 'upgradable'))
    ->orderBy('f.version');

  $query->join('logger_device_type', 't', 'f.device_type_id = t.id');
  $query->addField('t', 'name', 'device_type');

  if ($type_id > 0) {
    $query->condition('f.device_type_id', $type_id);
  }
  $releases = $query->execute();

  $header = array(t('Version'), t('Device Type'), t('Time'), t('Tag'), t('Upgradable'), t('Total'), t('Operations'));
  $rows = array();
  foreach ($releases as $release) {

    $query = db_select('logger_devices', 'd')
      ->condition('d.firmware_id', $release->id);
    $query->addExpression('COUNT(*)');
    $total = $query->execute()->fetchField();

    $rows[] = array(
      $release->version,
      $release->device_type,
      $release->release_time,
      wordwrap($release->tag, 16, "\n", TRUE),
      $release->upgradable ? t('Yes') : t('No'),
      $release->upgradable && $total > 0 ?
        l($total, "firmware/upgrade", array('query' => array(
          'device_type_id' => $release->device_type_id,
          'from_version' => $release->id
        ))) : $total,
      l(t('Edit'), "firmware/edit/$release->id") . ' ' . l(t('Remove'), "firmware/remove/$release->id")
    );
  }

  $form = drupal_get_form('logger_firmwarefilter_form', $type_id);

  return
    drupal_render($form) . '<br>' .
    l(t('Add Firmware Release'), 'firmware/edit') . '<br><br>' .

    theme_table(array('header' => $header, 'rows' => $rows, 'caption' => '',
      'empty' => t('No firmware releases available.'), 'sticky' => FALSE, 'colgroups' => array(),
      'attributes' => array('id' => 'firmware-list')));
}

/**
 * Downloads a firmware log file.
 *
 * @param $device_type_id  The device type id.
 * @param $version         The firmware version.
 */
function logger_firmwarelog_download($device_type_id, $version) {

  $file_name = 'ChangeLog.txt';
  drupal_add_http_header('Pragma', 'public');
  drupal_add_http_header('Content-Type', 'text/plain; charset=utf-8');
  drupal_add_http_header('Content-Disposition', "attachment; filename=$file_name");
  drupal_add_http_header('Cache-Control', 'public');

  $file_path = LOGGER_PATH . '/upgrades/versions/' . $device_type_id . '/' . $version . '/' . $file_name;
  if (file_exists($file_path)) {
    print file_get_contents($file_path);
  }
  exit(0);
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
 * Builds the firmware releases list page.
 *
 * @param $id The release id.
 * @return the firmware releases list page.
 */
function logger_firmware_edition_page($id) {

  if ($id) {
    $release = db_select('logger_device_firmware', 'f')
      ->fields('f', array('id', 'version', 'release_time', 'build', 'tag', 'device_type_id', 'upgradable', 'description'))
      ->condition('f.id', $id)
      ->execute()
      ->fetchObject();
  }
  else {
    $release = new stdClass();
    $release->id = '';
    $release->version = '';
    $release->release_time = '';
    $release->build = '';
    $release->tag = '';
    $release->device_type_id = UNKNOWN_DEVICE_TYPE_ID;
    $release->upgradable = FALSE;
    $release->description = '';
  }

  $form = drupal_get_form('logger_firmwareedition_form', $release);
  return drupal_render($form);
}

/**
 * Builds the device edition page.
 *
 * @param $id  The device id.
 * @return the device edition page.
 */
function logger_device_edition_page($id) {

  $form = drupal_get_form('logger_device_form', logger_get_device($id));
  return drupal_render($form);
}

/**
 * Builds the device assignment page.
 *
 * @return the device assignment page.
 */
function logger_device_assign_page() {

  $callback = isset($_REQUEST['callback']) ? urldecode($_REQUEST['callback']) : '';
  $code = isset($_REQUEST['code']) ? $_REQUEST['code'] : '';
  $output = '';

  if (count($code) > 10 || !(empty($callback) || valid_url($callback))) {
    $output = t("The wizard can not continue. Please contact our support team.");
  }
  else {
    $output = empty($code) ? logger_step(1, 'Welcome to mySmartGrid! You are about to configure a device. ' . 
        'Please inform your user account and the device activation code in the fields below.') : '';

    $form = drupal_get_form('logger_deviceassign_form', $code, $callback);
    $output .= drupal_render($form);
  }
  return $output;
}

/**
 * Builds the device configuration page.
 *
 * @param $id    The device id.
 * @param $page  The configuration wizard page number.
 * @return the device configuration page.
 */
function logger_device_config_page($id, $page) {

  $device = logger_get_device($id);

  $header = array(
    array('style' => 'background-color: white; border: 0;', 'width' => '200px'),
    array('style' => 'background-color: white; border: 0;')
  );
  $rows = array(
    array(t('Device Id'), $device->device),
    array(t('Type'), $device->type_name),
    array(t('Firmware Version'), $device->firmware_version)
  );

  $output = theme_table(array('header' => $header, 'rows' => $rows, 'attributes' => array(),
    'caption' => '<b>' . t('Device') . '</b>', 'colgroups' => array(), 'sticky' => FALSE, 'empty' => '')) . '<br>';

  if ($page == 1) {
    $output .= logger_step(2, 'Please, inform which network interfaces and sensors must be enabled on the device.') . '<br>';

    $form = drupal_get_form('logger_deviceconfig_form', $id, $page);
    $output .= drupal_render($form);
  }
  else {
    if ($device->pending_config) {

      $output .= logger_step(3, 'The configuration below is being sent to the device. ' .
        'Please make sure that the device is turned on and connected to the network during the next 10 minutes.') .
        '<br><div id="progressbar"></div><br><br>';

      $progress = ((REQUEST_TIME - max($device->created, $device->access)) / (10 * MINUTE)) * 100;
      $progress = $progress < 95 ? $progress : 95;

      $options = array('type' => 'file', 'scope' => 'header', 'group' => JS_LIBRARY, 'weight' => 150, 'defer' => FALSE);
      drupal_add_js(EMOS_REL_PATH . '/js/jquery-min.js', $options);
      drupal_add_js(EMOS_REL_PATH . '/js/jquery-ui-min.js', $options);
      drupal_add_css(EMOS_REL_PATH . '/style/jquery-ui.css');
      drupal_add_js("jQuery(\"#progressbar\").progressbar({value: $progress});",
        array('type' => 'inline', 'scope' => 'footer', 'group' => JS_DEFAULT));

      drupal_add_html_head(array('#type' => 'html_tag', '#tag' => 'meta', '#attributes' => array(
        'http-equiv' => 'refresh', 'content' => "10;URL=/device/config/$id/2")), 'device_config');
    }
    else {
      $output .= logger_step(4, 'Congratulations! The new configuration has been successfully applied to the device. ' .
        'Click !here to see the list of active sensors.', array('!here' => l(t('here'), 'sensor/mylist'))) .
        '<div align="center"><img src="/misc/message-24-ok.png" align="center">' . t('Configuration applied.') . '</div><br><br>';
    }

    $config_protocol_options = logger_get_config_protocol_options();

    if ($device->lan_enabled) {
      $rows = array(array(t('Configuration Protocol'), $config_protocol_options[$device->lan_protocol]));
      if ($device->lan_protocol == 'static') {
        $rows[] = array(t('IP address'), $device->lan_ip);
        $rows[] = array(t('Gateway IP address'), $device->lan_gateway);
        $rows[] = array(t('Network mask'), $device->lan_netmask);
        $rows[] = array(t('Nameserver IP address'), $device->lan_nameserver);
      }
      $output .= '<br>' . theme_table(array('header' => $header, 'rows' => $rows, 'attributes' => array(),
          'caption' => '<b>' . t('LAN Interface') . '</b>', 'colgroups' => array(), 'sticky' => FALSE, 'empty' => ''));
    }

    if ($device->wifi_enabled) {
      $rows = array(array(t('Configuration Protocol'), $config_protocol_options[$device->wifi_protocol]));
      if ($device->wifi_protocol == 'static') {
        $rows[] = array(t('IP address'), $device->wifi_ip);
        $rows[] = array(t('Gateway IP address'), $device->wifi_gateway);
        $rows[] = array(t('Network mask'), $device->wifi_netmask);
        $rows[] = array(t('Nameserver IP address'), $device->wifi_nameserver);
      }
      $rows[] = array(t('SSID'), $device->wifi_essid);
      $encoding_protocol_options = logger_get_encoding_protocol_options();
      $rows[] = array(t('Encoding'), $encoding_protocol_options[$device->wifi_enc]);
      $rows[] = array(t('Key or passphrase'), $device->wifi_psk);

      $output .= '<br>' . theme_table(array('header' => $header, 'rows' => $rows, 'attributes' => array(),
        'caption' => '<b>' . t('WIFI Interface') . '</b>', 'colgroups' => array(), 'sticky' => FALSE, 'empty' => ''));
    }

    $sensors = logger_get_sensors(NULL, NULL, FALSE, FALSE, TRUE, array(), NULL, TRUE, $id, TRUE);
    if (!empty($sensors)) {

      $header = array(t('Sensor'), t('Function'), t('Class'), t('Voltage'), t('Current'), t('Constant'));
      $rows = array();

      foreach($sensors as $sensor) {
        $rows[] = array(
          wordwrap($sensor->meter, 16, ' ', TRUE),
          $sensor->function,
          $sensor->class_name,
          ($sensor->class_id == ANALOG_SENSOR_CLASS_ID ? $sensor->voltage . ' V' : '-'),
          ($sensor->class_id == ANALOG_SENSOR_CLASS_ID ? $sensor->current . ' A' : '-'),
          ($sensor->class_id == PULSE_SENSOR_CLASS_ID ? $sensor->constant . ' ' . t('pulses/kWh') : '-')
        );
      }
      $output .= '<br>' . theme_table(array('header' => $header, 'rows' => $rows, 'attributes' => array(),
          'caption' => '<b>' . t('Active Sensors') . '</b>', 'colgroups' => array(), 'sticky' => FALSE, 'empty' => ''));
    }
  }
  return $output;
}

function logger_step($number, $message, $args = array()) {

  return '<p><b>' . t('Step') . " $number - </b> " . t($message, $args) . '</p>';
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
      ->fields('m', array('device', 'meter', 'external_id', 'function', 'forecast', 'latitude', 'longitude', 'virtual', 'price'))
      ->condition('m.meter', $meter);

    $query->addField('m', 'type', 'type_id');

    $query->join('logger_devices', 'd', 'd.device = m.device');
    $query->addField('d', 'type_id', 'device_type_id');

    $query->join('unit', 'un', 'm.unit_id = un.id');
    $query->addField('un', 'symbol', 'unit_symbol');
    $query->addField('un', 'name', 'unit_name');
    $query->addField('un', 'type_id', 'unit_type_id');

    $query->join('logger_tokens', 'k', 'k.meter = m.meter');
    $query->addField('k', 'token', 'token');

    $query->leftJoin('logger_energy_meter', 'em', 'em.meter = m.meter');
    $query->addField('em', 'voltage');
    $query->addField('em', 'current');
    $query->addField('em', 'constant');

    $query->leftJoin('logger_energy_meter_class', 'mc', 'mc.id = em.class_id');   
    $query->addField('mc', 'id', 'class_id');
    $query->addField('mc', 'name', 'class_name');
 
    $sensor = $query->execute()
      ->fetchObject();
  }
  else {
    $sensor = new stdClass();
    $sensor->device = NULL;
    $sensor->meter = NULL;
    $sensor->external_id = NULL;
    $sensor->type_id = ENERGY_CONSUMPTION_SENSOR_TYPE;
    $sensor->function = '';
    $sensor->forecast = 0;
    $sensor->latitude = LOGGER_DEFAULT_LATITUDE;
    $sensor->longitude = LOGGER_DEFAULT_LONGITUDE;
    $sensor->virtual = TRUE;
    $sensor->price = 0.18;
    $sensor->unit_symbol = 'Wh';
    $sensor->unit_name = t('Watt-hour');
    $sensor->unit_type_id = ENERGY_UNIT_TYPE_ID;
    $sensor->device_type_id = FLUKSO2_DEVICE_TYPE_ID;
    $sensor->token = md5(uniqid(rand(), TRUE));
    $class->class_id = NULL;
    $sensor->class_name = NULL;
    $sensor->voltage = NULL;
    $sensor->current = NULL;
    $sensor->constant = NULL;
  }

  $form = drupal_get_form('logger_sensor_form', $sensor);

  return drupal_render($form);
}

function logger_device_form($form, &$form_state, $device, $form_id = "logger-device-form") {

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
    '#default_value' => $device->description
  );

  $space = str_repeat('&nbsp;', 53);

  $form['config_header'] = array(
    '#type' => 'item',
    '#title' => t('Network Configuration'),
  );

  $form['applied_message']['config_applied'] = array(
    '#type' => 'item',
    '#markup' => t('<div class="messages ' . ($device->pending_config ? 'warning' : 'status') . '">The following configuration is' . ($device->pending_config ? " not yet " : " ") . 'applied to your device.</div>'),
  );

  $form['lan_enabled'] = array(
    '#type' => 'checkbox',
    '#title' => t('LAN enabled'),
    '#description' => t('Please inform whether the LAN network interface is enabled on the device.'),
    '#default_value' => $device->lan_enabled,
    '#return_value' => 1,
    '#attributes' => array('onclick' => "javascript: showDeviceLANDiv('$form_id');"), //In IE, onchange does not work
    '#prefix' => "<div class='field-group'>",
  );

  $form['lan_protocol'] = array(
    '#type' => 'select',
    '#title' => t('LAN Configuration'),
    '#description' => t('Please inform how the LAN interface is configured. Dynamic means that all the configuration is obtained automatically via the DHCP protocol. Static means that the configuration is manually informed.'),
    '#default_value' => isset($device->lan_protocol) ? $device->lan_protocol : 'dhcp',
    '#options' => logger_get_config_protocol_options(),
    '#attributes' => array('onchange' => "javascript: showDeviceLANIPDiv('$form_id');"),
    '#prefix' => "<div id='lan-fields-div' style='display: none'>"
  );

  $form['lan_ip'] = array(
    '#type' => 'textfield',
    '#title' => t('LAN IP address'),
    '#default_value' => $device->lan_ip,
    '#maxlength' => 15,
    '#description' => t('Enter the IP address used by the LAN interface (for example: 192.168.1.1).'),
    '#prefix' => "<div id='lan-ip-fields-div' style='display: none'>"
  );

  $form['lan_gateway'] = array(
    '#type' => 'textfield',
    '#title' => t('LAN Gateway IP address'),
    '#default_value' => $device->lan_gateway,
    '#maxlength' => 15,
    '#description' => t('Enter the IP address of the gateway used by the LAN interface (for example: 192.168.1.254).'),
  );

  $form['lan_netmask'] = array(
    '#type' => 'textfield',
    '#title' => t('LAN Network mask'),
    '#default_value' => $device->lan_netmask,
    '#maxlength' => 15,
    '#description' => t('Enter the network mask used by the LAN interface (for example: 255.255.255.0).'),
  );

  $form['lan_nameserver'] = array(
    '#type' => 'textfield',
    '#title' => t('LAN Namserver IP address'),
    '#default_value' => $device->lan_nameserver,
    '#maxlength' => 15,
    '#description' => t('Enter the IP address of the nameserver used by the LAN interface (for example: 62.141.38.230).'),
    '#suffix' => '</div></div></div>'
  );

  $form['wifi_enabled'] = array(
    '#type' => 'checkbox',
    '#title' => t('WIFI enabled'),
    '#description' => t('Please inform whether the WIFI network interface is enabled on the device.'),
    '#default_value' => $device->wifi_enabled,
    '#return_value' => 1,
    '#attributes' => array('onclick' => "javascript: showDeviceWIFIDiv('$form_id');"), //In IE, onchange does not work
    '#prefix' => "<div class='field-group'>"
  );

  $form['wifi_protocol'] = array(
    '#type' => 'select',
    '#title' => t('WIFI Configuration'),
    '#description' => t('Please inform how the WIFI interface is configured. Dynamic means that all the configuration is obtained automatically via the DHCP protocol. Static means that the configuration is manually informed.'),
    '#default_value' => isset($device->wifi_protocol) ? $device->wifi_protocol : 'dhcp',
    '#options' => logger_get_config_protocol_options(),
    '#attributes' => array('onchange' => "javascript: showDeviceWIFIIPDiv('$form_id');"),
    '#prefix' => "<div id='wifi-fields-div' style='display: none'>"
  );

  $form['wifi_ip'] = array(
    '#type' => 'textfield',
    '#title' => t('WIFI IP address'),
    '#default_value' => $device->wifi_ip,
    '#maxlength' => 15,
    '#description' => t('Enter the IP address used by the WIFI interface (for example: 192.168.1.1).'),
    '#prefix' => "<div id='wifi-ip-fields-div' style='display: none'>"
  );

  $form['wifi_gateway'] = array(
    '#type' => 'textfield',
    '#title' => t('WIFI Gateway IP address'),
    '#default_value' => $device->wifi_gateway,
    '#maxlength' => 15,
    '#description' => t('Enter the IP address of the gateway used by the WIFI interface (for example: 192.168.1.254).'),
  );

  $form['wifi_netmask'] = array(
    '#type' => 'textfield',
    '#title' => t('WIFI Network mask'),
    '#default_value' => $device->wifi_netmask,
    '#maxlength' => 15,
    '#description' => t('Enter the network mask used by the WIFI interface (for example: 255.255.255.0).'),
  );

  $form['wifi_nameserver'] = array(
    '#type' => 'textfield',
    '#title' => t('WIFI Namserver IP address'),
    '#default_value' => $device->wifi_nameserver,
    '#maxlength' => 15,
    '#description' => t('Enter the IP address of the nameserver used by the WIFI interface (for example: 62.141.38.230).'),
    '#suffix' => '</div>'
  );

  $form['wifi_essid'] = array(
    '#type' => 'textfield',
    '#title' => t('WIFI SSID'),
    '#default_value' => $device->wifi_essid,
    '#maxlength' => 60,
    '#description' => t('Enter the WIFI network name (for example: WLAN-183729).'),
  );

  $form['wifi_enc'] = array(
    '#type' => 'select',
    '#title' => t('WIFI Encoding'),
    '#description' => t('Please inform which protocol the WIFI network interface uses for encoding the communication.'),
    '#default_value' => isset($device->wifi_enc) ? $device->wifi_enc : 'wpa2',
    '#options' => logger_get_encoding_protocol_options(),
    '#attributes' => array('onchange' => "javascript: showDeviceWIFIEncDiv('$form_id');"),
  );

  $form['wifi_psk'] = array(
    '#type' => 'textfield',
    '#title' => t('WIFI key or passphrase'),
    '#description' => t('Enter the WIFI encryption key or passphrase (for example: 1A648C9FE2).'),
    '#default_value' => $device->wifi_psk,
    '#maxlength' => 60,
    '#prefix' => "<div id='wifi-psk-fields-div' style='display: none'>",
    '#suffix' => '</div></div></div>'
  );

  $form['submit'] = array(
    '#type' => 'submit',
    '#value' => t('Save'),
  );

  drupal_add_js("jQuery(document).ready( function() { showDeviceLANDiv('$form_id'); showDeviceWIFIDiv('$form_id'); showDeviceLANIPDiv('$form_id'); showDeviceWIFIIPDiv('$form_id'); showDeviceWIFIEncDiv('$form_id');} );", array('type' => 'inline', 'scope' => 'footer'));

  return $form;
}

function logger_get_config_protocol_options() {
  return array('static' => t('Static'), 'dhcp' => 'Dynamic (DHCP)');
}

function logger_get_encoding_protocol_options() {
  return array('open' => t('No encryption'), 'wep' => 'WEP', 'wpa' => 'WPA', 'wpa2' => 'WPA2');
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

function logger_deviceassign_form($form, &$form_state, $code, $callback) {

  $space = str_repeat('&nbsp;', 53);
  $form['new_account'] = array(
    '#type' => 'radios',
    '#title' => t('mySmartGrid account'),
    '#description' => t('Please inform whether you already have a mySmartGrid user account, or want to create one now.'),
    '#options' => array(
        0 => $space . t('I already have a mySmartGrid user account.'),
        1 => $space . t('I want to create a mySmartGrid account now.')),
    '#default_value' => 0,
    '#attributes' => array('onchange' => "javascript: toggleDeviceAssignFields();"),
    '#required' => TRUE
  );

  $div_style = !isset($_POST['new_account']) || $_POST['new_account'] == 0 ? '' : "style='display: none'";

  $form['user_name'] = array(
    '#type' => 'textfield',
    '#title' => t('User Name'),
    '#description' => t("Enter your mySmartGrid user name."),
    '#maxlength' => 60,
    '#size' => 20,
    '#prefix' => "<div id='user-fields-div' $div_style>"
  );

  $form['password'] = array(
    '#type' => 'password',
    '#title' => t('Password'),
    '#description' => t("Enter your password."),
    '#maxlength' => 128,
    '#size' => 20,
    '#suffix' => '</div>'
  );

  $div_style = isset($_POST['new_account']) && $_POST['new_account'] == 1 ? '' : "style='display: none'";

  $form['new_user_name'] = array(
    '#type' => 'textfield',
    '#title' => t('User Name'),
    '#description' => t("Enter a name for your mySmartGrid user. Spaces are allowed; punctuation is not allowed except for periods, hyphens, apostrophes, and underscores."),
    '#maxlength' => 60,
    '#size' => 20,
    '#prefix' => "<div id='new-user-fields-div' $div_style>"
  );

  $form['new_user_password'] = array(
    '#type' => 'password',
    '#title' => t('Password'),
    '#description' => t('Enter a password for your user account. Please, do not use birth dates, dictionary words, number sequences. ' .
      'A good password is mix of letters, numbers, and special characters, and contains at least 8 characters.'),
    '#maxlength' => 128,
    '#size' => 20
  );

  $form['new_user_password2'] = array(
    '#type' => 'password',
    '#title' => t('Retype Password'),
    '#description' => t("Please, retype your password."),
    '#maxlength' => 128,
    '#size' => 20
  );

  $form['email'] = array(
    '#type' => 'textfield',
    '#title' => t('E-mail Address'),
    '#description' => t("Enter your e-mail address."),
    '#maxlength' => 60,
    '#size' => 60,
    '#suffix' => '</div>'
  );

  if (empty($code)) {
    $form['code'] = array(
      '#title' => t('Activation Code'),
      '#type' => 'textfield',
      '#description' => t('Please, inform the activation code of the device to be assigned to your account. This code is found in the device installation guide.'),
      '#size' => 10,
      '#maxlength' => 10,
      '#required' => TRUE,
    );
  }
  else {
    $form['code'] = array(
      '#type' => 'hidden',
      '#default_value' => $code,
    );
  }

  $form['callback'] = array(
    '#type' => 'hidden',
    '#default_value' => $callback,
  );

  $form['submit'] = array(
    '#type' => 'submit',
    '#value' => t('Next')
  );
  return $form;
}

function logger_create_energy_sensor_fields(&$form, $sensor, $field_id_suffix = '') {

  $is_dynamic_config = $sensor->device_type_id == FLUKSO2_DEVICE_TYPE_ID && ($sensor->type_id == ENERGY_CONSUMPTION_SENSOR_TYPE || $sensor->type_id == ENERGY_PRODUCTION_SENSOR_TYPE);

  $form["class_id$field_id_suffix"] = array(
    '#type' => 'hidden',
    '#required' => TRUE,
    '#default_value' => $sensor->class_id,
    '#access' => $is_dynamic_config && isset($sensor->class_id)
  );

  $form["class_name$field_id_suffix"] = array(
    '#type' => 'textfield',
    '#title' => t('Class'),
    '#description' => t('The sensor class (either Pulse or Analog).'),
    '#maxlength' => 30,
    '#size' => 10,
    '#default_value' => $sensor->class_name,
    '#attributes' => array('readonly' => 'readonly', 'class' => array('edit-class-name')),
    '#access' => $is_dynamic_config && isset($sensor->class_id)
  );

  $form["voltage$field_id_suffix"] = array(
    '#type' => 'textfield',
    '#title' => t('Voltage'),
    '#description' => t("The sensor voltage in Volts."),
    '#maxlength' => 6,
    '#size' => 6,
    '#required' => TRUE,
    '#default_value' => $sensor->voltage,
    '#field_suffix' => ' V',
    '#attributes' => array('class' => array('edit-voltage')),
    '#access' => $is_dynamic_config && $sensor->class_id == ANALOG_SENSOR_CLASS_ID
  );

  $form["current$field_id_suffix"] = array(
    '#type' => 'select',
    '#title' => t('Electric Current'),
    '#description' => t("The sensor electric current in Amperes. The current is determined by the size of the clamp."),
    '#options' => array(50 => 50, 100 => 100, 250 => 250, 500 => 500),
    '#required' => TRUE,
    '#default_value' => $sensor->current,
    '#field_suffix' => ' A',
    '#access' => $is_dynamic_config && $sensor->class_id == ANALOG_SENSOR_CLASS_ID
  );

  $form["constant$field_id_suffix"] = array(
    '#type' => 'textfield',
    '#title' => t('Constant'),
    '#description' => t("The sensor constant."),
    '#maxlength' => 10,
    '#size' => 10,
    '#required' => TRUE,
    '#default_value' => $sensor->constant,
    '#field_suffix' => ' ' . t('pulses/kWh'),
    '#attributes' => array('class' => array('edit-constant')),
    '#access' => $is_dynamic_config && $sensor->class_id == PULSE_SENSOR_CLASS_ID
  );
}

function logger_deviceconfig_form($form, &$form_state, $device_id) {

  $device = logger_get_device($device_id);
  $sensors = logger_get_sensors(NULL, NULL, FALSE, FALSE, TRUE, array(), NULL, FALSE, $device_id, TRUE);
  $form_id = "logger-deviceconfig-form";

  $form = logger_device_form($form, $form_state, $device, $form_id);
  $form['device']['#type'] = 'hidden';
  unset($form['submit']);
  unset($form['type']);
  unset($form['firmware']);
  unset($form['serial']);
  unset($form['description']);

  foreach($sensors as $sensor) {

    if (isset($sensor->class_id)) {
      $div_id = "sensor$sensor->meter-fields-div";

      $enabled = !empty($sensor->function);
      $form['enabled' . $sensor->meter] = array(
        '#type' => 'checkbox',
        '#title' => t('Sensor enabled'),
        '#description' => t('Please inform whether the sensor @meter should be enabled on the device.', array('@meter' => $sensor->meter)),
        '#default_value' => $enabled,
        '#field_suffix' => $sensor->meter,
        '#return_value' => 1,
        '#attributes' => array('onclick' => "javascript: showSensorDiv('$form_id', '$sensor->meter');"), //In IE, onchange does not work
        '#prefix' => "<div class='field-group'>",
        '#suffix' => "<div id='$div_id' " . ($enabled ? '' : "style='display: none'") . ">"
      );

      logger_create_energy_sensor_fields($form, $sensor, $sensor->meter);

      $form['function' . $sensor->meter] = array(
        '#type' => 'textfield',
        '#title' => t('Name'),
        '#description' => t("Enter the sensor's name."),
        '#default_value' => $sensor->function,
        '#maxlength' => 32,
        '#size' => 15,
        '#required' => TRUE,
        '#suffix' => '</div></div>'
      );
    }
  }
  $form['submit'] = array(
    '#type' => 'submit',
    '#value' => t('Save')
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
    '#required' => TRUE,
    '#options' => $sensor->virtual ? $options : array($sensor->device => $options[$sensor->device])
  );

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

    $form['token'] = array(
      '#type' => 'hidden',
      '#required' => TRUE,
      '#default_value' => $sensor->token
    );

    $form['reset_token'] = array(
      '#type' => 'radios',
      '#title' => t('Token'),
      '#options' => array(
          0 => t('Keep current token: !token', array('!token' => str_repeat('&nbsp;', 20) . "$sensor->token")),
          1 => t('Generate a new token.')),
      '#description' => t('Select which authentication token to use.'),
      '#default_value' => 0,
      '#weight' => 101
    );
  }

  if ($sensor->device_type_id == RASPBERRY_PI_DEVICE_TYPE_ID || $sensor->device_type_id == LIBKLIO_DEVICE_TYPE_ID) {
    $form['external_id'] = array(
      '#type' => 'textfield',
      '#title' => t('External Id'),
      '#description' => t("The sensor external id."),
      '#maxlength' => 64,
      '#size' => 64,
      '#required' => TRUE,
      '#default_value' => $sensor->external_id,
      '#attributes' => array('readonly' => 'readonly'),
      '#access' => ($sensor->device_type_id == RASPBERRY_PI_DEVICE_TYPE_ID || $sensor->device_type_id == LIBKLIO_DEVICE_TYPE_ID)
    );
  }

  $form['type_id'] = array(
    '#type' => 'select',
    '#title' => t('Type'),
    '#description' => t('The sensor type.'),
    '#options' => logger_get_sensor_types_options(NULL, $sensor->unit_type_id),
    '#default_value' => $sensor->type_id,
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

  logger_create_energy_sensor_fields($form, $sensor);

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
      '#default_value' => ($sensor->forecast ? array(1 => 1) : array()),
      '#suffix' => '</div>'
    );
    if(!msgcommons_user_has_role('PVSpy')) {
      $form['forecast']['#attributes']= array('onclick' => "javascript: return permissionSensorForecastFields('". t('This feature cost. Please ask the MySmartGrid for the appropriated rights') ."');");
    }


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
    '#weight' => 200
  );

  drupal_add_js("jQuery(document).ready( function() { showSensorForecastFields(); } );", array('type' => 'inline', 'scope' => 'footer'));

  return $form;
}

function logger_userprivacy_form($form, &$form_state) {

  global $user;

  $private = db_select('logger_users', 'u')
    ->fields('u', array('private'))
    ->condition('u.uid', $user->uid)
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

function logger_firmwarefilter_form($form, &$form_state, $type_id) {

  $types = logger_get_device_types_options();
  $types[0] = t('All');

  $form['device_type_id'] = array(
    '#type' => 'select',
    '#title' => t('Device Type'),
    '#description' => t('Select the device type.'),
    '#options' => $types,
    '#default_value' => $type_id,
    '#required' => TRUE,
    '#attributes' => array('onchange' => "document.getElementById('logger-firmwarefilter-form').submit();")
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

  $header = array(t('Serial'), t('Device Id'), t('From Version'), t('To Version'), t('Requested'), t('User Approval'));
  $rows = array();
  foreach ($devices as $device) {
    $rows[$device->device] = array(
      l($device->serial, "deployment/checkup/serial/$device->serial"),
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

  $form['approved'] = array(
    '#type' => 'checkbox',
    '#title' => t('Pre-approved'),
    '#description' => t("Check if you do not want to wait for the user's approval."),
    '#default_value' => 0
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
    '#value' => t('Request')
  );
  return $form;
}

function logger_firmwareedition_form($form, &$form_state, $release) {

  drupal_set_title(t('Firmware Release Edition'));

  //If edition mode
  if (is_numeric($release->id)) {
    $form['id'] = array(
      '#type' => 'hidden',
      '#default_value' => $release->id,
      '#required' => TRUE
    );
  }

  $types = logger_get_device_types_options();

  $form['device_type_id'] = array(
    '#type' => 'select',
    '#title' => t('Device Type'),
    '#description' => t('Select the device type.'),
    '#options' => $types,
    '#default_value' => $release->device_type_id,
    '#required' => TRUE
  );

  $form['version'] = array(
    '#type' => 'textfield',
    '#title' => t('Version'),
    '#description' => t('Please, inform the firmware release version, using format: 9.9.9-9.'),
    '#default_value' => $release->version,
    '#maxlength' => 11,
    '#size' => 11,
    '#required' => TRUE
  );

  $form['release_time'] = array(
    '#type' => 'textfield',
    '#title' => t('Release Time'),
    '#description' => t('Please, inform the release time of the release using format: YYYYMMDD_HHmm.'),
    '#default_value' => $release->release_time,
    '#maxlength' => 13,
    '#size' => 13,
    '#required' => TRUE
  );

  $form['tag'] = array(
    '#type' => 'textfield',
    '#title' => t('Tag'),
    '#description' => t('Please, inform the tag identification of the release.'),
    '#default_value' => $release->tag,
    '#maxlength' => 32,
    '#size' => 32,
    '#required' => TRUE
  );

  $form['build'] = array(
    '#type' => 'textfield',
    '#title' => t('Build'),
    '#description' => t('Please, inform the build identification of the release.'),
    '#default_value' => $release->build,
    '#maxlength' => 40,
    '#size' => 40,
    '#required' => TRUE
  );

  $form['upgradable'] = array(
    '#type' => 'checkbox',
    '#title' => t('Upgradable'),
    '#description' => t("Please inform whether the release can be remotely upgraded."),
    '#default_value' => $release->upgradable 
  );

  $form['description'] = array(
    '#type' => 'textarea',
    '#title' => t('Description'),
    '#description' => t('Please, inform the description of the release.'),
    '#default_value' => $release->description,
    '#maxlength' => 60,
    '#size' => 60,
    '#required' => TRUE
  );

  $form['submit'] = array(
    '#type' => 'submit',
    '#value' => t('Save')
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
      ->condition('d.uid', 0)
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

function logger_deviceassign_form_validate($form, &$form_state) {

  //TODO: simplify this function

  //User already has an account
  if ($form_state['values']['new_account'] == 0) {

    if (empty($form_state['values']['user_name'])) {
      form_set_error('user_name', t('Please, inform your user name.'));
    }
    elseif (empty($form_state['values']['password'])) {
      form_set_error('password', t('Please, type the password.'));
    }
    else {
      $form_state['values']['uid'] = logger_authenticate_user($form_state['values']['user_name'], $form_state['values']['password']);

      if ($form_state['values']['uid']) {
        $account = user_load($form_state['values']['uid']);
      }
      else {
        form_set_error('password', t('User authentication failed.'));
      }
    }
  }
  else {
    //New user
    if (empty($form_state['values']['new_user_name'])) {
      form_set_error('new_user_name', t('Please inform a user name.'));
    }
    elseif (user_load_by_name($form_state['values']['new_user_name'])) {
      form_set_error('new_user_name', t('This user name is taken. Please inform another one.'));
    }
    elseif (empty($form_state['values']['email'])) {
      form_set_error('email', t('Please, inform the e-mail address.'));
    }
    elseif (user_validate_mail($form_state['values']['email'])) {//TRUE when invalid
      form_set_error('email', t('Please, inform a valid email.'));
    }
    elseif (empty($form_state['values']['new_user_password'])) {
      form_set_error('new_user_password', t('Please, type the password.'));
    }
    elseif (strlen($form_state['values']['new_user_password']) < 6) {
      form_set_error('new_user_password', t('The password must contain at least 6 characters.'));
    }
    elseif (empty($form_state['values']['new_user_password2'])) {
      form_set_error('new_user_password2', t('Please, retype the password.'));
    }
    elseif ($form_state['values']['new_user_password2'] != $form_state['values']['new_user_password']) {
      form_set_error('new_user_password2', t('The password has not been retyped correctly.'));
    }
    else {
      $account = new stdClass();
      $account->is_new = TRUE;
      $account->name = $form_state['values']['new_user_name'];
      $account->pass = $form_state['values']['new_user_password'];
      $account->mail = $form_state['values']['email'];
      $account->status = 1;
      $account->timezone = date_default_timezone_get();

      if (user_save($account)) {
        $account = user_load_by_name($account->name);
        user_save($account, array('pass' => $account->pass));

        $form_state['values']['uid'] = logger_authenticate_user($form_state['values']['new_user_name'], $form_state['values']['new_user_password']);
        if (!$form_state['values']['uid']) {
          form_set_error('new_user_password', t('User authentication failed.'));
        }
      }
      if (!isset($form_state['values']['uid']) || !$form_state['values']['uid']) {
        form_set_error('User could not be created. Please, try again later.');
      }
    }
  }

  if ($form_state['values']['uid']) {

    $uid = $form_state['values']['uid'];
    $code = $form_state['values']['code'];
    $device = db_select('logger_devices', 'd')
      ->fields('d', array('type_id'))
      ->condition('d.device', "$code%", 'LIKE')
      ->condition('d.uid', array(0, $uid), 'IN')
      ->execute()
      ->fetchObject();

    if (!$device) {
      form_set_error('code', t('The device can not be linked to your user account. Please make sure it is not already linked to another account.'));
    }
    elseif ($device->type_id == RASPBERRY_PI_DEVICE_TYPE_ID) {

      //FIXME: get rid of the role 'EMOS'
      $rid = get_role_id('EMOS');

      db_delete('users_roles')
        ->condition('uid', $uid)
        ->condition('rid', $rid)
        ->execute();

      db_insert('users_roles')
        ->fields(array(
          'uid' => $uid,
          'rid' => $rid
        ))
        ->execute();
    }
  }
}

function logger_deviceconfig_form_validate($form, &$form_state) {

  logger_device_form_validate($form, &$form_state);
}

function logger_sensor_form_validate($form, &$form_state) {

  $in_edition = isset($form_state['values']['meter']);
  $function = $form_state['values']['function'];
  $meter = $in_edition ? $form_state['values']['meter'] : NULL;

  if (isset($form_state['values']['price']) && !($form_state['values']['price'] > 0)) {
    form_set_error('price', t('The informed price is invalid.'));
  }

  if (isset($form_state['values']['forecast'])) {
    $forecast = $form_state['values']['forecast'][1];
    $latitude = $_REQUEST['coordinates_latitude'];
    $longitude = $_REQUEST['coordinates_longitude'];

    if (!($latitude >= -90 && $latitude <= 90 && $longitude >= -90 && $longitude <= 90)) {

      form_set_error('coordinates', t('Invalid geographic coordinates.'));
    }

    if ($forecast && !($latitude > 47.25 && $latitude < 55.09 && $longitude > 5.8 && $longitude < 15.069)) {

      form_set_error('coordinates', t('The forecast service is available only for sensors located in Germany.'));
    }
  }

  if ($in_edition) {

    $token = isset($form_state['values']['reset_token']) && $form_state['values']['reset_token'] ? NULL : $form_state['values']['token'];
    logger_token_save($meter, $token);

    if (isset($form_state['values']['class_id'])) {
      $class_id = $form_state['values']['class_id'];

      if ($class_id == ANALOG_SENSOR_CLASS_ID) {
        if ($form_state['values']['voltage'] < 100 || $form_state['values']['voltage'] > 1000) {
          form_set_error('voltage', t('Invalid voltage.'));
        }
        if (!in_array($form_state['values']['current'], array(50, 100, 250, 500))) {
          form_set_error('voltage', t('Invalid electric current.'));
        }
      }
      elseif ($class_id == PULSE_SENSOR_CLASS_ID && ($form_state['values']['constant'] < 0 || $form_state['values']['constant'] > 1000)) {
        form_set_error('voltage', t('Invalid sensor constant.'));
      }
    }
  }
  else {
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

function logger_firmwareedition_form_validate($form, &$form_state) {

  if (!preg_match('/[0-9]+\.[0-9]+\.[0-9]+[\-][0-9]+/', $form_state['values']['version'])) {
    form_set_error('version', t('The firmware release version is using the wrong format.'));
  }
  if (!preg_match('/[0-9]{8}[_][0-9]{4}/', $form_state['values']['release_time'])) {
    form_set_error('release_time', t('The firmware release time is using the wrong format.'));
  }
}

function logger_device_form_validate($form, &$form_state) {

  if (isset($form_state['values']['description']) && !preg_match('/^[\x20-\x7e\xc2a1-\xc3bf€]*$/', $form_state['values']['description'])) {
    form_set_error('description', t('The device description contains non-printable characters.'));
  }

  validate_network_fields($form_state['values'], 'lan_enabled', 'lan_ip', 'lan_gateway', 'lan_netmask', 'lan_nameserver', 'lan_protocol');
  validate_network_fields($form_state['values'], 'wifi_enabled', 'wifi_ip', 'wifi_gateway', 'wifi_netmask', 'wifi_nameserver', 'wifi_protocol');

  if ($form_state['values']['wifi_enabled']) {
    if (empty($form_state['values']['wifi_essid'])) {
      form_set_error('wifi_essid', t('The WIFI network name must be informed.'));
    }
    if ($form_state['values']['wifi_enc'] != 'open') {
      if (empty($form_state['values']['wifi_psk'])) {
        form_set_error('wifi_psk', t('The WIFI encryption key or passphrase must be informed.'));
      }
    }
  }
}

function logger_device_form_submit($form, &$form_state) {

  db_delete('logger_device_network')
    ->condition('device', $form_state['values']['device'])
    ->execute();

  $lan_enabled = isset($form_state['values']['lan_enabled']) && $form_state['values']['lan_enabled'] == 1 ? 1 : 0;
  $wifi_enabled = isset($form_state['values']['wifi_enabled']) && $form_state['values']['wifi_enabled'] == 1 ? 1 : 0;

  if ($lan_enabled || $wifi_enabled) {

    $fields = array(
      'device' => $form_state['values']['device'],
      'lan_enabled' => $lan_enabled,
      'wifi_enabled' => $wifi_enabled
    );

    if ($lan_enabled) {
      $fields['lan_protocol'] = $form_state['values']['lan_protocol'];
      if ($form_state['values']['lan_protocol'] == 'static') {
        $fields['lan_ip'] = $form_state['values']['lan_ip']; 
        $fields['lan_netmask'] = $form_state['values']['lan_netmask'];
        $fields['lan_gateway'] = $form_state['values']['lan_gateway'];
        $fields['lan_nameserver'] = $form_state['values']['lan_nameserver'];
      }
    }

    if ($wifi_enabled) {
      $fields['wifi_protocol'] = $form_state['values']['wifi_protocol'];
      if ($form_state['values']['wifi_protocol'] == 'static') {
        $fields['wifi_ip'] = $form_state['values']['wifi_ip'];
        $fields['wifi_netmask'] = $form_state['values']['wifi_netmask'];
        $fields['wifi_gateway'] = $form_state['values']['wifi_gateway'];
        $fields['wifi_nameserver'] = $form_state['values']['wifi_nameserver'];
      }
      $fields['wifi_essid'] = $form_state['values']['wifi_essid'];
      $fields['wifi_enc'] = $form_state['values']['wifi_enc'];
      if ($form_state['values']['wifi_enc'] != 'open') {
        $fields['wifi_psk'] = $form_state['values']['wifi_psk'];
      }
    }

    db_insert('logger_device_network')
      ->fields($fields)
      ->execute();
  }

  db_update('logger_devices')
    ->fields(array(
      'description' => $form_state['values']['description'],
      'pending_config' => 1
    ))
    ->condition('device', $form_state['values']['device'])
    ->execute();

  $form_state['redirect'] = 'device/mylist';
}

function logger_deviceactivation_form_submit($form, &$form_state) {

  $form_state['redirect'] = 'device/activate/' . $form_state['values']['code'];
}

function logger_deviceassign_form_submit($form, &$form_state) {

  module_load_include('inc', 'logger', 'logger.admin');

  $code = $form_state['values']['code'];
  logger_device_assign($code);

  if (empty($form_state['values']['callback'])) {

    $device = db_select('logger_devices', 'd')
      ->fields('d', array('device'))
      ->condition('d.device', "$code%", 'LIKE')
      ->execute()
      ->fetchField();

    $form_state['redirect'] = "device/config/$device/1";
  }
  else {
    $form_state['redirect'] = array($form_state['values']['callback'], array('external' => TRUE));
  }
}

function logger_deviceconfig_form_submit($form, &$form_state) {

  foreach($form_state['values'] as $key => $value) {

    if (strpos($key, "function") === 0) {
      $meter = substr($key, 8);
      $function = $form_state['values']["function$meter"];

      if (!empty($function)) {

        db_update('logger_energy_meter')
          ->fields(array(
            'voltage' => (isset($form_state['values']["voltage$meter"]) ? $form_state['values']["voltage$meter"] : NULL),
            'current' => (isset($form_state['values']["current$meter"]) ? $form_state['values']["current$meter"] : NULL),
            'constant' => (isset($form_state['values']["constant$meter"]) ? $form_state['values']["constant$meter"] : NULL)
          ))
          ->condition('meter', $meter)
          ->execute();

        db_update('logger_meters')
          ->fields(array(
            'function' => $function
          ))
          ->condition('meter', $meter)
          ->execute();
      }
    }
  }
  $form_state['values']['description'] = '';
  logger_device_form_submit($form, &$form_state);
  $form_state['redirect'] = 'device/config/' . $form_state['values']['device'] . '/2';
}

function logger_sensor_form_submit($form, &$form_state) {

  //TODO: refactor this function

  global $user;

  $function = $form_state['values']['function'];
  $type_id = $form_state['values']['type_id'];
  $price = isset($form_state['values']['price']) ? $form_state['values']['price'] : 0;
  $device = $form_state['values']['device'];

  //If edition
  if (isset($form_state['values']['meter'])) {

    $meter = $form_state['values']['meter'];
    $fields = array('function' => $function, 'type' => $type_id);

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

    if (isset($form_state['values']['voltage'])) {

      db_update('logger_energy_meter')
        ->fields(array(
          'meter' => $meter,
          'voltage' => $form_state['values']['voltage'],
          'current' => $form_state['values']['current'],
          'constant' => $form_state['values']['constant']
        ))
        ->condition('meter', $meter)
        ->execute();
    }

    db_update('logger_devices')
      ->fields(array(
        'pending_config' => 1
      ))
      ->condition('device', $device)
      ->execute();
  }
  //If creation
  else {
    $meter = md5(uniqid(rand(), TRUE));
    $token = md5(uniqid(rand(), TRUE));
    $forecast = isset($form_state['values']['forecast']) ? $form_state['values']['forecast'][1] : 0;
    $latitude = isset($_REQUEST['coordinates_latitude']) ? $_REQUEST['coordinates_latitude'] : 0;
    $longitude = isset($_REQUEST['coordinates_longitude']) ? $_REQUEST['coordinates_longitude'] : 0;

    logger_sensor_add($meter, $device, time(), $token, 62, $function, $type_id, 1, $forecast, $latitude, $longitude, $price);
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

function logger_firmwarefilter_form_submit($form, &$form_state) {

  $form_state['redirect'] = array('firmware/list', array(
    'query' => array(
      'device_type_id' => $form_state['values']['device_type_id'],
  )));
}

function logger_firmwareupgradefilter_form_submit($form, &$form_state) {

  if (isset($form_state['values']['to_version'])) {

    $selected = array_filter($form_state['values']['table']);

    foreach ($selected as $device) {

      logger_remove_firmware_upgrade_request($device);

      if ($form_state['values']['to_version'] > 0) {

        db_insert('logger_firmware_upgrade_request')
          ->fields(array(
            'device' => $device,
            'firmware_id' => $form_state['values']['to_version'], 
            'requested' => time(),
            'approved' => ($form_state['values']['approved'] > 0 ? time() : 0)
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

function logger_firmwareedition_form_submit($form, &$form_state) {

  //If edition
  if (isset($form_state['values']['id'])) {

    db_update('logger_device_firmware')
      ->fields(array(
        'device_type_id' => $form_state['values']['device_type_id'],
        'version' => $form_state['values']['version'],
        'release_time' => $form_state['values']['release_time'],
        'tag' => $form_state['values']['tag'],
        'build' => $form_state['values']['build'],
        'upgradable' => $form_state['values']['upgradable'],
        'description' => $form_state['values']['description']
      ))
      ->condition('id', $form_state['values']['id'])
      ->execute();
  }
  //If creation
  else {

    db_insert('logger_device_firmware')
      ->fields(array(
        'device_type_id' => $form_state['values']['device_type_id'],
        'version' => $form_state['values']['version'],
        'release_time' => $form_state['values']['release_time'],
        'tag' => $form_state['values']['tag'],
        'build' => $form_state['values']['build'],
        'upgradable' => $form_state['values']['upgradable'],
        'description' => $form_state['values']['description']
      ))
      ->execute();
  }

  $form_state['redirect'] = "firmware/list";
}

/**
 * Authenticates a user.
 *
 * @param $login    The user login name.
 * @param $password The user password.
 * @return the user id.
 */
function logger_authenticate_user($login, $password) {

  $uid = user_authenticate($login, $password);

  if ($uid) {
    $account = user_load_by_name($login);
    user_save($account, array('pass' => $password));

    $fields = array('uid' => $uid);
    user_login_submit(array(), $fields);
  }
  return $uid;
}

/**
 * Associates a device with the authenticated user.
 *
 * @param $code The activation code.
 */
function logger_device_assign($code) {

  global $user;

  db_update('logger_devices')
    ->fields(array(
      'uid' => $user->uid,
    ))
    ->condition('device', "$code%", 'LIKE')
    ->execute();

  // check whether the user is already a true fluksonian
  // if not, generate the proper entries in {users_roles} and {logger_users}
  if (!msgcommons_user_has_role('fluksonian')) {

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
}

/**
 * Associates a device with the authenticated user and return to the devices list page.
 *
 * @param $code The activation code.
 */
function logger_device_activate($code) {

  logger_device_assign($code);

  drupal_set_message(t("The device is now associated with your account."));

  drupal_goto('device/mylist');
}

/**
 * Deactivates a device and all its sensors, removing their user association.
 *
 * @param $device  The device id.
 */
function logger_device_deactivate($device) {

  global $user;

  module_load_include('inc', 'logger', 'logger.rrd');

  $device_type_id = db_select('logger_devices', 'd')
    ->fields('d', array('type_id'))
    ->condition('d.device', $device)
    ->execute()
    ->fetchField();

  $sensors = db_select('logger_meters', 'm')
    ->fields('m')
    ->condition('m.device', $device)
    ->execute();

  foreach ($sensors as $sensor) {

    //FIXME: define a hook
    msgdump_remove_by_sensor($sensor->meter);
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
    ->fields('m', array('meter'))
    ->condition('m.device', $device)
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
    ->condition('d.device', $device)
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
        'function' => empty($sensor->function) ? NULL : substr("C $sensor->function", 0, 16),
        'created' => $sensor->created,
        'access' => $sensor->access,
        'price' => $sensor->price,
        'forecast' => $sensor->forecast,
        'latitude' => $sensor->latitude,
        'longitude' => $sensor->longitude,
        'external_id' => $sensor->external_id,
        'unit_id' => $sensor->unit_id,
        'type' => $sensor->type
      ))
      ->execute();

    if ($sensor->forecast == 1 && msgcommons_user_has_role('PVSpy')) {
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
 */
function logger_sensor_remove($meter) {

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

  db_delete('logger_energy_meter')
    ->condition('meter', $meter)
    ->execute();

  db_delete('logger_meters')
    ->condition('meter', $meter)
    ->execute();

  pvcast_delete_plant($meter);

  if (isset($_REQUEST['next_url'])) {

    drupal_goto($_REQUEST['next_url']);
  }
}

/**
 * Adds a new device sensor.
 */
function logger_sensor_add($meter, $device, $created, $token, $permissions, $function = NULL, $type = ENERGY_CONSUMPTION_SENSOR_TYPE, $virtual = 1, $forecast = 0, $latitude = LOGGER_DEFAULT_LATITUDE, $longitude = LOGGER_DEFAULT_LONGITUDE, $price = 0.18) {

  module_load_include('inc', 'logger', 'logger.rrd');

  $error = db_insert('logger_meters')
    ->fields(array(
      'meter' => $meter,
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
 * Saves a sensor token.
 */
function logger_token_save($meter, $token = NULL) {

  if (!$token) {
    $token = md5(uniqid(rand(), TRUE));
  }

  db_update('logger_tokens')
    ->fields(array(
      'token' => $token
    ))
    ->condition('meter', $meter)
    ->execute();
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
 * Removes a device firmware release.
 *
 * @param $id The id of the release to be removed.
 */
function logger_firmware_remove($id) {

  db_delete('logger_device_firmware')
    ->condition('id', $id)
    ->execute();

  drupal_goto("firmware/list");
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

  drupal_set_message(t('The firmware of your device is going to be upgraded. Please, make sure it remains up and running during the next 2 hours.'), 'warning');

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
