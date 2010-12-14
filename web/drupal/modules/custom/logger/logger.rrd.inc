<?php
/**
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
define('ROOT_PATH',  'sites/all/modules/logger');
define('RRDTOOL',    ROOT_PATH . '/rrdtool');
define('DATA_PATH',  ROOT_PATH . '/data');

define('MINUTE', 60);
define('HOUR',   60 * MINUTE);
define('DAY',    24 * HOUR);
define('WEEK',    7 * DAY);
define('YEAR',  520 * WEEK);

define('METER', 'meter');

/**
 * Creates an RRD for storing sensors' measurements in minute resolution.
 *
 * @param $meter  The sensor's id.
 * @return the creation command execution code.
 */
function logger_rrd_base_create($meter) {

  $step = 1 * MINUTE;
  $archives = array(
     //resolution   storage period
     1 * MINUTE =>  2 * HOUR,
    15 * MINUTE =>  2 * DAY,
     1 * HOUR   =>  8 * WEEK,
     1 * WEEK   => 10 * YEAR);

  return _logger_rrd_create($meter, $step, 'DERIVE', $archives);
}

/**
 * Creates an RRD for storing sensors' night measurements in daily resolution.
 *
 * @param $meter  The sensor's id.
 * @return the creation command execution code.
 */
function logger_rrd_night_create($meter) {

  $step = 24 * HOUR;
  $archives = array(
    //resolution    storage period
    24 * HOUR   =>  8 * WEEK,
    1 * WEEK    => 10 * YEAR);

  return _logger_rrd_create($meter, $step, 'GAUGE', $archives, 'night');
}

/**
 * Creates an RRD for storing sensors' measurements.
 *
 * @param $meter     The sensor's id.
 * @param $step      The base interval in seconds with which data will be fed into the RRD.
 * @param $ds_type   The RRD DS type.
 * @param $archives  An array of RRD archives' properties formatted as (resolution => storage period).
 * @param $subdir    The subdirectory where the rrd file should be placed. (NULL means the default location)
 * @return the creation command execution code.
 */
function _logger_rrd_create($meter, $step, $ds_type, $archives, $subdir) {

  $return = 0;
  $file_path = _logger_rrd_file($meter, $subdir);
  $start = 1199487600; //Fri, 04 Jan 2008 23:00:00 GMT

  $ds = _logger_rrd_meter_ds($ds_type);

  if (!file_exists($file_path)) {

    $command = RRDTOOL . " create $file_path " .
      "--start $start " .
      "--step $step " .
      "$ds " .
      _logger_rrd_rras($step, $archives);

    system($command, $return);
  }
  return $return;
}

/**
 * Creates a RRD DS definition.
 *
 * @param $type  The DS type.
 * @return the DS definition.
 */
function _logger_rrd_meter_ds($type) {

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
function _logger_rrd_rras($step, $archives) {
  
  $rras = '';
  foreach($archives as $resolution => $storage_period) {

    $steps = $resolution / $step;
    $slots = $storage_period / $resolution;

    $rras .= "RRA:AVERAGE:0.5:$steps:$slots ";
  }
  return $rras;
}

/**
 * Feeds the sensors' RRDs with night consumption.
 *
 * @param $meter  The sensor's id.
 * @param $start  The night period start time.
 * @param $end    The night period end time.
 * @return the update command execution code.
 */
function logger_rrd_night_update($meter, $start, $end) {

  $file_path = _logger_rrd_file($meter);
  $resolution = 15 * MINUTE;

  $command = RRDTOOL . " fetch $file_path " .
    "AVERAGE " .
    "--resolution $resolution " .
    "--start $start " .
    "--end $end " .

    "| tail -n 12 | awk -F': ' '{SUM += $2} END {print SUM/12}'";

  $value = (float) shell_exec($command);

  return logger_rrd_update($meter, array($end => $value), 'night');
}

/**
 * Feeds the sensors' RRDs with measurements.
 *
 * @param $meter   The sensor's id.
 * @param $values  An array of measurements formatted as (timestamp => value).
 * @param $subdir  The subdirectory where the rrd file is placed.
 * @return the update command execution code.
 */
function logger_rrd_update($meter, $values, $subdir = NULL) {

  $return = 0;
  $file_path = _logger_rrd_file($meter, $subdir);

  $command = RRDTOOL . " update $file_path ";

  foreach($values as $timestamp => $value) {
    $command .= " $timestamp:$value";
  }

  system($command, $return);

  return $return;
}

/**
 * Queries the RRDs for sensor's measurements.
 *
 * @param $interval       The time interval.
 * @param $sensor         The sensor.
 * @param $factor         The unit conversion factor.
 * @param $start          The start time.
 * @param $offset         The user timezone offset.
 * @return the array of sensor's measurements.
 */
function logger_rrd_query_sensor($interval, $sensor, $factor, $start, $offset) {

  $series_id = METER;
  $def = _logger_rrd_sensor_def($interval, $series_id, $sensor, $factor);

  return _logger_rrd_query($interval, $start, $def, $series_id, $offset);
}

/**
 * Queries the RRDs for sensors' aggregated measurements.
 *
 * @param $interval       The time interval.
 * @param $sensors        The array of sensors.
 * @param $factor         The unit conversion factor.
 * @param $start          The start time.
 * @param $offset         The user timezone offset.
 * @return the array of sensor's measurements.
 */
function logger_rrd_query_agg($interval, $sensors, $factor, $start, $offset) {

  //Latest measurements are not considered, in order to tolerate heartbeat delays.
  $end_time = time() - 30 * MINUTE;
  $def = "";
  $variables = "";

  $i = 1;
  foreach ($sensors as $sensor) {

    $sensor_cdef = METER . $i;

    $def .= _logger_rrd_sensor_def($interval, $sensor_cdef, $sensor, $factor) .

      //Considers unknown measurements to be zero
      "CDEF:completeseries$i=" . "$sensor_cdef,UN,0,$sensor_cdef,IF " .

      //Do not consider latest 30 minutes
      "CDEF:filtered$i=TIME,$end_time,GT,UNKN,completeseries$i,IF ";

    $variables .= ($i > 1 ? ',' : '') . "filtered$i";
    $i++;
  }

  $operators = str_repeat(",+", count($sensors) - 1);

  $series_id = METER . '0';

  //Sum all measurements of a particular time
  $def .= "CDEF:$series_id=$variables$operators ";

  return _logger_rrd_query($interval, $start, $def, $series_id, $offset, false);
}

/**
 * Creates a DEF tag to represent sensor's measurements plotted in a chart series.
 *
 * @param $interval       The time interval.
 * @param $series_id      The series id.
 * @param $sensor         The sensor.
 * @param $factor         The unit conversion factor.
 * @return the DEF tag.
 */
function _logger_rrd_sensor_def($interval, $series_id, $sensor, $factor) {

  global $user;

  if($sensor->private > 0 && $sensor->uid != $user->uid) {
    return null;
  }

  $file_path = _logger_rrd_file($sensor->meter, $interval);

  return "DEF:data$series_id=" . "$file_path:meter:AVERAGE " .
    "CDEF:$series_id=" . "data$series_id," . $factor . ",* ";
}

/**
 * Composes RRD file path.
 *
 * @param $meter     The sensor id.
 * @param $interval  The time interval.
 * @return the RRD file path.
 */
function _logger_rrd_file($meter, $interval) {

  $path = DATA_PATH . ($interval == 'night'? '/night' : '/base');
  return "$path/$meter.rrd";
}

/**
 * Exports the sensors' measurements from the RRDs to an array.
 *
 * @param $interval       The time interval.
 * @param $start          The start time.
 * @param $def            The rrd def tag.
 * @param $exported_cdef  The exported cdef.
 * @param $offset         The user timezone offset.
 * @param $include_nan    Whether NaN values should be included in the result.
 * @return the array of sensors' measurements.
 */
function _logger_rrd_query($interval, $start, $def, $exported_cdef, $offset, $include_nan = true) {

  if (!$def) {
    return array();
  }

  $maxrows = array('hour' => 120, 'night' => 60, 'day' => 96, 'month' => 60, 'year' => 60);
  $step    = array('hour' => 1,   'night' => 1,  'day' => 1,  'month' => 1,  'year' => 604800);

  $command = RRDTOOL . ' xport ' .
    "--start $start " .
    "--step $step[$interval] " .
    "--maxrows $maxrows[$interval] " .
    "$def " .
    "XPORT:$exported_cdef";

  exec($command, $lines);

  $data = array();

  foreach($lines as $line) {
    $line = strtolower($line);

    if (strpos($line, '<row>') > 0) {

      $line = str_replace(array('<row><t>', '</v></row>'), '',  $line);
      $line = str_replace('</t><v>', ':', $line);
      $div_pos = strpos($line, ':');

      $timestamp = trim(substr($line, 0, $div_pos)) + $offset;
      $value = trim(substr($line, $div_pos + 1));

      if ($value == 'nan') {

        if ($include_nan) {
          $data[$timestamp] = null;
        }

      } else {
        $data[$timestamp] = $value;
      }
    }
  }
  return $data;
}