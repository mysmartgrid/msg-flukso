<?php

/**
 * @file
 * Callbacks for accessing RRDs.
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

//Constants
define('RRDTOOL_PATH', DRUPAL_ROOT . '/sites/all/modules/logger');
define('RRDTOOL', RRDTOOL_PATH . '/rrdtool');
define('METER', 'meter');

/**
 * Creates an RRD for storing sensors' measurements in minute resolution.
 *
 * @param $meter  The sensor's id.
 * @return the creation command execution code.
 */
function logger_rrd_base_create($meter) {

  $archives = logger_rrd_get_storage_periods();

  return logger_rrd_create($meter, MINUTE, 'DERIVE', $archives);
}

/**
 * Returns an array of default measurement storage periods, indexed by time resolution.
 *
 * @return the array of storage periods.
 */
function logger_rrd_get_storage_periods() {
  return array(
    //resolution   storage period
    1 * MINUTE => 1 * WEEK,
    15 * MINUTE => 1 * MONTH,
    1 * HOUR => 2 * MONTH,
    1 * DAY => 6 * MONTH,
    1 * WEEK => 10 * YEAR,
  );
}

/**
 * Returns the measurements storage period of a sensor in the informed time resolution.
 *
 * @param $meter The sensor id.
 * @param $step  The time resolution.
 * @return the storage period.
 */
function logger_rrd_get_storage_period($meter, $step) {

  //FIXME: simplify this function

  $cid = "storage_period_$meter";
  $cache = cache_get($cid, 'cache_page');

  if ($cache) {
    $period = $cache->data;
  }
  else {
    $objects = db_select('logger_meter_storage_period', 's')
      ->fields('s', array('resolution', 'period'))
      ->condition('s.meter', $meter)
      ->execute();

    $period = array();
    foreach ($objects as $object) {
      $period[$object->resolution] = $object->period;
    }
    if (empty($period)) {
      $period = logger_rrd_get_storage_periods();
    }

    cache_set($cid, $period, 'cache_page', REQUEST_TIME + DAY);
  }
  return $period[$step];
}

/**
 * Adjusts the period of time of a query to the RRA limitations.
 * 
 * @param $meter   The sensor id.
 * @param $step    The time resolution.
 * @param $period  The period of time to be adjusted, as an array (start => start timestamp, end => end timestamp).
 * @return the adjusted timestamp.
 */
function logger_rrd_adjust_period($meter, $step, $period) {

  $storage_period = logger_rrd_get_storage_period($meter, $step);
  $min_time = REQUEST_TIME - $storage_period;

  if ($period['end'] < $min_time) {
    $period['end'] = REQUEST_TIME;
  }

  if ($period['start'] >= $period['end']) {
    $period['start'] = $min_time;
  }

  $window = $period['end'] - $period['start'];
  $max_window = $storage_period - (REQUEST_TIME - $period['end']);

  //RRD requires the time window to be covered by the RRA
  if ($window > $max_window) {
    $window = $max_window;
  }
  $period['start'] = $period['end'] - $window;

  return $period;
}

/**
 * Creates an RRD for storing sensors' measurements.
 *
 * @param $meter     The sensor's id.
 * @param $step      The base interval in seconds with which data will be fed into the RRD.
 * @param $ds_type   The RRD DS type.
 * @param $archives  An array of RRD archives' properties formatted as (resolution => storage period).
 * @return the creation command execution code.
 */
function logger_rrd_create($meter, $step, $ds_type, $archives) {

  $return = 0;
  $file_path = logger_rrd_file($meter);
  $start = 1199487600; //Fri, 04 Jan 2008 23:00:00 GMT

  $ds = logger_rrd_meter_ds($ds_type);

  if (!file_exists($file_path)) {

    $command = RRDTOOL . " create $file_path " .
      "--start $start " .
      "--step $step " .
      "$ds " .
      logger_rrd_rras($step, $archives);

    $output = array();
    exec($command, $output, $return);
  }
  return $return == 0;
}

/**
 * Removes an RRD file.
 *
 * @param $meter     The sensor's id.
 * @return TRUE if successful, FALSE otherwise. 
 */
function logger_rrd_remove($meter) {

  $file_path = logger_rrd_file($meter);

  if (file_exists($file_path)) {
    return unlink($file_path);
  }
  else {
    return FALSE;
  }
}

/**
 * Recreates an RRD file.
 *
 * @param $meter     The sensor's id.
 * @return TRUE if successful, FALSE otherwise.
 */
function logger_rrd_clear($meter) {

  return logger_rrd_remove($meter) && logger_rrd_base_create($meter);
}

/**
 * Creates a RRD DS definition.
 *
 * @param $type  The DS type.
 * @return the DS definition.
 */
function logger_rrd_meter_ds($type) {

  $name = METER;
  $timeout = 100 * DAY;
  $min = 0;
  $max = 20;

  return "DS:$name:$type:$timeout:$min:$max";
}

/**
 * Creates a list of RRD archives' definitions.
 *
 * @param $step      The base interval in seconds with which data will be fed into the RRD.
 * @param $archives  The array of RRD archives' properties formatted as (resolution => storage period).
 * @return the list of RRD archives' definitions.
 */
function logger_rrd_rras($step, $archives) {

  $rras = '';
  foreach ($archives as $resolution => $storage_period) {

    $rows = $resolution / $step;
    $slots = $storage_period / $resolution;

    $rras .= "RRA:AVERAGE:0.5:$rows:$slots ";
  }
  return $rras;
}

/**
 * Feeds the sensors' RRDs with measurements.
 *
 * @param $meter   The sensor's id.
 * @param $values  An array of measurements formatted as (timestamp => value).
 * @return the update command execution code.
 */
function logger_rrd_update($meter, $values) {

  $return = 0;
  $file_path = logger_rrd_file($meter);

  $command = RRDTOOL . " update $file_path ";

  ksort($values);

  foreach ($values as $timestamp => $value) {
    $command .= " $timestamp:$value";
  }

  $output = array();
  exec($command, $output, $return);

  return $return == 0;
}

/**
 * Queries the RRDs for sensor's measurements.
 *
 * @param $sensor       The sensor.
 * @param $unit         The power unit.
 * @param $period       An array containing a period of time to be queried.
 * @param $step         The amount of time aggregated in a single point.
 * @param $price        The price of kWh, when the unit id either 'Euro' or 'Euro/year'.
 * @return the array of sensor's measurements.
 */
function logger_rrd_query_sensor($sensor, $unit, $period, $step, $price = NULL) {

  $def = logger_rrd_sensor_def(METER, $sensor, $unit, $price);
  if ($def == NULL) {
    return array();
  }

  $latest = logger_rrd_latest_timestamp($sensor);
  $period = logger_rrd_adjust_period($sensor->meter, $step, $period);

  return logger_rrd_export($def, METER, $latest, TRUE, $period, $step);
}

/**
 * Returns the timestamp of the latest update of a sensor or an appliance.
 *
 * @param $sensor       The sensor.
 * @paran $appliance_id The appliance id, if any.
 * @return the latest update timestamp.
 */
function logger_rrd_latest_timestamp($sensor, $appliance_id = NULL) {

  $file_path = logger_rrd_file($sensor->meter, $appliance_id);
  if (file_exists($file_path)) {

    $command = RRDTOOL . " last $file_path";
    $lines = array();
    $error = 0;
    exec($command, $lines, $error);

    if ($error != 0) {
      watchdog('logger.rrd.last', "Error executing rrd command: last. Return code: %error", array('%error' => $error), WATCHDOG_ERROR);
      return 0;
    }

    return $lines[0];
  }
  else {
    return 0;
  }
}

/**
 * Queries the RRDs for sensors' aggregated measurements.
 *
 * @param $sensors      The array of sensors.
 * @param $unit         The power unit.
 * @param $period       An array containing a period of time to be queried.
 * @param $step         The amount of time aggregated in a single point.
 * @param $price        The price of kWh, when the unit id either 'Euro' or 'Euro/year'. 
 * @return the array of sensor's measurements.
 */
function logger_rrd_query_agg($sensors, $unit, $period, $step, $price = NULL) {

  //Latest measurements are not considered, in order to tolerate message delays.
  $end_time = REQUEST_TIME - 5 * MINUTE;
  $def = "";
  $variables = "";

  $i = 1;
  foreach ($sensors as $sensor) {

    $sensor_cdef = METER . $i;

    $sensor_def = logger_rrd_sensor_def($sensor_cdef, $sensor, $unit, $price);

    if ($sensor_def) {

      $def .= $sensor_def .

        //Considers unknown measurements to be zero
        "CDEF:completeseries$i=" . "$sensor_cdef,UN,0,$sensor_cdef,IF " .

        //Do not consider latest minutes
        "CDEF:filtered$i=TIME,$end_time,GT,UNKN,completeseries$i,IF ";

      $variables .= ($i > 1 ? ',' : '') . "filtered$i";
      $i++;
    }
  }

  if ($i > 1) {
    $operators = str_repeat(",+", $i - 2);
    $series_id = METER . '0';

    //Sum all measurements of a particular time
    $def .= "CDEF:$series_id=$variables$operators ";

    $period = logger_rrd_adjust_period('none', $step, $period);

    return logger_rrd_export($def, $series_id, REQUEST_TIME, FALSE, $period, $step);
  }
  else {
    return array();
  }
}

/**
 * Queries the total energy consumption measured by a set of sensors during the specified time period, in watt-hour.
 *
 * @param $sensors    The sensors whose measurements are to be summed up.
 * @param $unit       The energy unit.
 * @param $period     An array containing a period of time to be queried.
 * @param $step       The amount of time aggregated in a single point.
 * @param $prices     The prices of kWh, indexed by sensor type, when the unit id either 'Euro' or 'Euro/year'.
 * @return the summed energy consumption.
 */
function logger_rrd_query_total_energy($sensors, $unit, $period, $step, $prices = NULL) {

  $measurements = logger_rrd_query_energy($sensors, $unit, $period, $step, $prices);
  return array_sum($measurements);
}

/**
 * Queries the energy consumption of a set of sensors during the specified time period, in watt-hour.
 *
 * @param $sensors    The sensors whose measurements are to be summed up, for each timestamp.
 * @param $unit       The energy unit.
 * @param $period     An array containing a period of time to be queried.
 * @param $step       The amount of time aggregated in a single point.
 * @param $prices     The prices of kWh, indexed by sensor type, when the unit id either 'Euro' or 'Euro/year'.
 * @return the array of energy consumption, indexed by timestamp.
 */
function logger_rrd_query_energy($sensors, $unit, $period, $step, $prices = NULL) {

  $measurements = array();

  foreach ($sensors as $sensor) {
    $price = isset($prices[$sensor->type_id]) ? $prices[$sensor->type_id] : NULL;
    $m = logger_rrd_query_sensor($sensor, $unit, $period, $step, $price);

    foreach ($m as $timestamp => $value) {
      $measurements[$timestamp] = (isset($measurements[$timestamp]) ? $measurements[$timestamp] : 0) + $value * $step;
    }
  }
  return $measurements;
}

/**
 * Queries the RRDs for appliance detection flags.
 *
 * @param $sensor       The sensor.
 * @param $period       An array containing a period of time to be queried.
 * @param $appliance_id The appliance id.
 * @param $step         The amount of time aggregated in a single point.
 * @return the array associating timestamps with detection flags: either 0 (not detected) or 1 (detected).
 */
function logger_rrd_query_appliance($sensor, $period, $appliance_id, $step) {

  $def = logger_rrd_appliance_def(METER, $appliance_id, $sensor, $step);

  if ($def == NULL) {
    return array();
  }

  $latest = logger_rrd_latest_timestamp($sensor, $appliance_id);

  return logger_rrd_export($def, METER, $latest, TRUE, $period, $step);
}

/**
 * Creates a DEF tag to represent sensor's measurements plotted in a chart series.
 *
 * @param $series_id   The series id.
 * @param $sensor      The sensor.
 * @param $unit        The unit.
 * @param $price       The price of kWh, when the unit id either 'Euro' or 'Euro/year'.
 * @return the DEF tag.
 */
function logger_rrd_sensor_def($series_id, $sensor, $unit, $price = NULL) {

  global $user;

  if (isset($sensor->private) && $sensor->private > 0 && $sensor->uid != $user->uid) {
    return NULL;
  }

  $price = isset($price) ? $price : $sensor->price;

  $factor = logger_rrd_get_unit_factor($unit, $price, $sensor->device_type_id);

  $file_path = logger_rrd_file($sensor->meter);
  if (file_exists($file_path)) {

    return "DEF:data$series_id=$file_path:meter:AVERAGE CDEF:$series_id=data$series_id,$factor,* ";
  }
  else {
    return NULL;
  }
}

/**
 * Creates a DEF tag to represent a series of appliance detection flags plotted in a chart series.
 *
 * @param $series_id    The series id.
 * @param $appliance_id The appliance id.
 * @param $sensor       The sensor.
 * @param $step         The amount of time aggregated in a single point.
 * @return the DEF tag.
 */
function logger_rrd_appliance_def($series_id, $appliance_id, $sensor, $step) {

  global $user;

  if (isset($sensor->private) && $sensor->private > 0 && $sensor->uid != $user->uid) {
    return NULL;
  }

  $file_path = logger_rrd_file($sensor->meter, $appliance_id);
  if (file_exists($file_path)) {

    if ($step == MINUTE) {
      $function = 'AVERAGE';
    }
    elseif ($step == 15 * MINUTE) {
      $function = 'MIN';
    }
    else {
      $function = 'MAX';
    }
    return "DEF:data$series_id=$file_path:meter:$function CDEF:$series_id=data$series_id";
  }
  else {
    return NULL;
  }
}

/**
 * Composes RRD file path.
 *
 * @param $meter        The sensor id.
 * @param $appliance_id The appliance id, if any.
 * @return the RRD file path.
 */
function logger_rrd_file($meter, $appliance_id = NULL) {

  $dir = $appliance_id == NULL ? 'base' : "appliances/$appliance_id";

  return RRDTOOL_PATH . "/data/$dir/$meter.rrd";
}

/**
 * Exports the sensors' measurements from the RRDs to an array.
 *
 * @param $def           The rrd def tag.
 * @param $exported_cdef The exported cdef.
 * @param $latest        The latest update time.
 * @param $include_nan   Whether NaN values should be included in the result.
 * @param $period        An array containing a period of time to be queried.
 * @param $step          The amount of time aggregated in a single point.
 * @return the array of sensors' measurements.
 */
function logger_rrd_export($def, $exported_cdef, $latest, $include_nan, $period, $step) {

  global $user;
 
  if (!$def) {
    return array();
  }

  //time period MUST be within the RRA
  if ($step >= DAY) {
    //RRD aggregations operate in UTC time
    $offset = get_timezone_offset();
    $period['start'] += $offset;
    $period['end'] += $offset;
  }
  $period['start'] -= $period['start'] % $step;
  $period['end'] -= $period['end'] % $step + 1; //Until the last second

  $maxrows = 1 + ($period['end'] - $period['start']) / $step;
  $maxrows = $maxrows < 10 ? 10 : floor($maxrows); //RRDTool exports at least 8 rows

  $command = RRDTOOL . ' xport ' .
    " --start " . $period['start'] .
    " --end " . $period['end'] .
    " --step $step" .
    " --maxrows $maxrows" .
    " $def" .
    " XPORT:$exported_cdef";

  //Function exec() is unable to execute long commands
  if (strlen($command) > 102400) {
    $script_path = '/tmp/rrd.' . $user->uid . '.' . REQUEST_TIME . '.sh';
    save_file($script_path, "#!/bin/bash\n" . $command);
    $command = "/bin/sh $script_path";
  }

  //FIXME
  append_file("/tmp/rrd.command." . $user->uid . '.' . REQUEST_TIME . '.txt', "\n" . $command);

  $lines = array();
  $error = 0;
  exec($command, $lines, $error);

  if (isset($script_path)) {
    unlink($script_path);
  }

  if ($error != 0) {
    watchdog('logger.rrd.export', "Error executing rrd. Return code: %error . Command: %command", array('%error' => $error, '%command' => $command), WATCHDOG_ERROR);
    return array();
  }

  return logger_rrd_parse_exported_lines($lines);
}

/**
 * Parses output of command rrdtool xport into an array.
 *
 * @param $lines   The lines to be exported.
 * @return the array of sensors' measurements.
 */
function logger_rrd_parse_exported_lines($lines) {

  $data = array();

  foreach ($lines as $line) {
    $line = strtolower($line);

    if (strpos($line, '<row>') > 0) {

      $line = str_replace(array('<row><t>', '</v></row>'), '',   $line);
      $line = str_replace('</t><v>', ':', $line);
      $div_pos = strpos($line, ':');

      $timestamp = trim(substr($line, 0, $div_pos));
      $value = trim(substr($line, $div_pos + 1));

      if ($value != 'nan' && is_numeric($value)) {
        $data[$timestamp] = number_format($value, 11, '.', '');
      }
    }
  }
  return $data;
}

/**
 * Returns the unit conversion factor.
 * 
 * @param $unit           the unit.
 * @param $price          the price (if the unit is Monetary).
 * @param $device_type_id the device type id (optional).
 * @return the unit conversion factor.
 */
function logger_rrd_get_unit_factor($unit, $price = NULL, $device_type_id = 0) {//FIXME: argument

  $result = db_select('unit', 'u')
      ->fields('u', array('factor'))
      ->condition('u.symbol', $unit)
      ->execute();

  $factor = count($result) == 1 ? $result->fetchField() : 1;

  //TODO: use unit id instead
  if (strpos($unit, '€') != FALSE && $price) {
    $factor *= $price; 
  }

  //TODO: get rid of this test later
  return $factor * ($device_type_id == LIBKLIO_DEVICE_TYPE_ID || $device_type_id == RASPBERRY_PI_DEVICE_TYPE_ID ? 1 : 1000);
}
