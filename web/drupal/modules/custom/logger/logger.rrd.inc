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
//Some constants need to be redefined because this file can be called from xmlrpc.php
//drupal_get_path does not work, because xmlrpc.php is called from xmlrpc.php.
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

  $step = 1 * MINUTE;
  $archives = logger_rrd_get_storage_periods();

  return logger_rrd_create($meter, $step, 'DERIVE', $archives);
}

/**
 * Returns an array of measurement storage periods, indexed by time resolution.
 *
 * @return the array of storage periods.
 */
function logger_rrd_get_storage_periods() {
  return array(
    //resolution   storage period
    1 * MINUTE => 1 * WEEK,
    15 * MINUTE => 1 * MONTH,
    1 * DAY => 6 * MONTH,
    1 * WEEK => 10 * YEAR,
  );
}

/**
 * Returns the storage period of measurements in the informed time resolution.
 *
 * @param $step  The time resolution.
 * @return the storage period.
 */
function logger_rrd_get_storage_period($step) {
  $periods = logger_rrd_get_storage_periods();
  return $periods[$step];
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
  }
  return $return;
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

  system($command, $return);

  return $return;
}

/**
 * Queries the RRDs for sensor's measurements.
 *
 * @param $interval     The time interval. This argument is used to select the RRA.
 * @param $sensor       The sensor.
 * @param $unit         The power unit.
 * @param $offset       The user timezone offset.
 * @param $period       An array containing a period of time to be queried.
 * @param $step         The amount of time aggregated in a single point.
 * @return the array of sensor's measurements.
 */
function logger_rrd_query_sensor($interval, $sensor, $unit, $offset, $period, $step) {

  $series_id = METER;
  $def = logger_rrd_sensor_def($interval, $series_id, $sensor, $unit);

  $latest = logger_rrd_latest_timestamp($interval, $sensor);

  return logger_rrd_export($interval, $def, $series_id, $latest, $offset, TRUE, $period, $step);
}

/**
 * Returns the timestamp of the latest update of a sensor.
 *
 * @param $interval  The time interval.
 * @param $sensor    The sensor.
 * @return the latest update timestamp.
 */
function logger_rrd_latest_timestamp($interval, $sensor) {

  $file_path = logger_rrd_file($sensor->meter);

  $command = RRDTOOL . " last $file_path";
  exec($command, $lines);

  return $lines[0];
}

/**
 * Queries the RRDs for sensors' aggregated measurements.
 *
 * @param $interval     The time interval.
 * @param $sensors      The array of sensors.
 * @param $unit         The power unit.
 * @param $offset       The user timezone offset.
 * @param $period       An array containing a period of time to be queried.
 * @param $step         The amount of time aggregated in a single point.
 * @return the array of sensor's measurements.
 */
function logger_rrd_query_agg($interval, $sensors, $unit, $offset, $period, $step) {

  //Latest measurements are not considered, in order to tolerate heartbeat delays.
  $end_time = REQUEST_TIME - 15 * MINUTE;
  $def = "";
  $variables = "";

  $i = 1;
  foreach ($sensors as $sensor) {

    $sensor_cdef = METER . $i;

    $def .= logger_rrd_sensor_def($interval, $sensor_cdef, $sensor, $unit) .

      //Considers unknown measurements to be zero
      "CDEF:completeseries$i=" . "$sensor_cdef,UN,0,$sensor_cdef,IF " .

      //Do not consider latest minutes
      "CDEF:filtered$i=TIME,$end_time,GT,UNKN,completeseries$i,IF ";

    $variables .= ($i > 1 ? ',' : '') . "filtered$i";
    $i++;
  }

  $operators = str_repeat(",+", count($sensors) - 1);

  $series_id = METER . '0';

  //Sum all measurements of a particular time
  $def .= "CDEF:$series_id=$variables$operators ";

  return logger_rrd_export($interval, $def, $series_id, REQUEST_TIME, $offset, FALSE, $period, $step);
}

/**
 * Queries the total energy consumption during the specified time period, in watt-hour.
 *
 * @param $sensors    The sensors whose measurements are to be summed up.
 * @param $unit       The energy unit.
 * @param $interval   The time interval menu option.
 * @param $offset     The user timezone offset.
 * @param $period     An array containing a period of time to be queried.
 * @param $step       The amount of time aggregated in a single point.
 * @return the summed power consumption.
 */
function logger_rrd_query_energy($sensors, $unit, $interval, $offset, $period, $step) {

  $total = 0;

  foreach ($sensors as $sensor) {
    $measurements = logger_rrd_query_sensor($interval, $sensor, $unit, $offset, $period, $step);

    foreach ($measurements as $timestamp => $value) {
      $total += $value * $step;
    }
  }
  return $total;
}

/**
 * Creates a DEF tag to represent sensor's measurements plotted in a chart series.
 *
 * @param $interval    The time interval.
 * @param $series_id   The series id.
 * @param $sensor      The sensor.
 * @param $unit        The unit.
 * @return the DEF tag.
 */
function logger_rrd_sensor_def($interval, $series_id, $sensor, $unit) {

  global $user;
  $factor = logger_rrd_get_unit_factor($unit, isset($sensor->price) ? $sensor->price : 0);//FIXME

  if (isset($sensor->private) && $sensor->private > 0 && $sensor->uid != $user->uid) {
    return NULL;
  }
  $file_path = logger_rrd_file($sensor->meter);

  return "DEF:data$series_id=" . "$file_path:meter:AVERAGE " .
    "CDEF:$series_id=" . "data$series_id," . $factor . ",* ";
}

/**
 * Composes RRD file path.
 *
 * @param $meter     The sensor id.
 * @return the RRD file path.
 */
function logger_rrd_file($meter) {

  return RRDTOOL_PATH . "/data/base/$meter.rrd";
}

/**
 * Exports the sensors' measurements from the RRDs to an array.
 *
 * @param $def           The rrd def tag.
 * @param $exported_cdef The exported cdef.
 * @param $latest        The latest update time.
 * @param $offset        The user timezone offset.
 * @param $include_nan   Whether NaN values should be included in the result.
 * @param $period        An array containing a period of time to be queried.
 * @param $step          The amount of time aggregated in a single point.
 * @return the array of sensors' measurements.
 */
function logger_rrd_export($interval, $def, $exported_cdef, $latest, $offset, $include_nan, $period, $step) {

  if (!$def) {
    return array();
  }

  $start = $period['start'];
  $end = $period['end'];

  //time period MUST be within the RRA and both $start and $end must be multiples of $step
  $start = $start - ($start % $step);
  $end = $end - ($end % $step);

  $storage_period = logger_rrd_get_storage_period($step);

  if ((REQUEST_TIME - $end) > $storage_period) {
    return array();
  }

  $maxrows = 1 + ($end - $start) / $step;
  $maxrows = $maxrows < 10 ? 10 : $maxrows; //RRDTool exports at least 8 rows

  $command = RRDTOOL . ' xport ' .
    "--start $start " .
    "--end $end " .
    "--step $step " .
    "--maxrows $maxrows " .
    "$def " .
    "XPORT:$exported_cdef";

  exec($command, $lines);

  return logger_rrd_parse_exported_lines($lines, $offset);
}

/**
 * Parses output of command rrdtool xport into an array.
 *
 * @param $lines   The lines to be exported.
 * @param $offset  The user timezone offset.
 * @return the array of sensors' measurements.
 */
function logger_rrd_parse_exported_lines($lines, $offset) {

  $data = array();

  foreach ($lines as $line) {
    $line = strtolower($line);

    if (strpos($line, '<row>') > 0) {

      $line = str_replace(array('<row><t>', '</v></row>'), '',   $line);
      $line = str_replace('</t><v>', ':', $line);
      $div_pos = strpos($line, ':');

      $timestamp = trim(substr($line, 0, $div_pos)) + $offset;
      $value = trim(substr($line, $div_pos + 1));

      if ($value != 'nan') {
        $data[$timestamp] = $value;
      }
    }
  }
  return $data;
}

/**
 * Returns the unit conversion factor.
 * 
 * @param $unit  the unit.
 * @param $price the price (if the unit is Euro).
 * @return the unit conversion factor.
 */
function logger_rrd_get_unit_factor($unit, $price = NULL) {

  $factors['eur'] = 0.018; // € 0.18 / kWh
  $factors['Wh']  = 1;
  $factors['kWh'] = 0.001;

  $factors['watt']     = 3600; // 1Wh/s = 3600 W
  $factors['kWh/year'] = 31536; //($factors['watt'] / 1000) * 24 * 365;
  $factors['eur/year'] = $factors['kWh/year'] * $factors['eur'] * 10; // € 0.18 / kWh

  if ($unit == 'eur' && $price) {
    return $price * $factors['kWh'];

  }
  elseif ($unit == 'eur/year' && $price) {
    return $price * $factors['kWh/year'];

  }
  else {
    return $factors[$unit];
  }
}
