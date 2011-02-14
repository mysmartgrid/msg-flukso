/**
 * Javascript functions used by module logger.
 *
 * Copyright (c) 2010 Fraunhofer Institut ITWM (www.itwm.fraunhofer.de)
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

function formatDate(d) {
  return '' +
    (d.getDate()  < 10 ? '0' : '') + d.getDate() + '/' +
    (d.getMonth() <  9 ? '0' : '') + (d.getMonth() + 1) + '/' +
    d.getFullYear();
}

function formatTime(d) {
  return '' +
    (d.getHours()   < 10 ? '0' : '') + d.getHours() + ':' +
    (d.getMinutes() < 10 ? '0' : '') + d.getMinutes();
}

function parseDate(datestr, timestr) {

  var dateParts = datestr.split('/');
  var timeParts = timestr.split(':');

  return new Date(
      dateParts[2],
      dateParts[1] - 1,
      dateParts[0],
      timeParts[0],
      timeParts[1],
      0,
      0
    );
}

function updateControlForm(dygraph) {

  var form = document.getElementById('logger-control-form');

  var value = Math.round(dygraph.yAxisRange(0)[0]);
  form.elements['yvalue1'].value = value;

  value = Math.round(dygraph.yAxisRange(0)[1]);
  form.elements['yvalue2'].value = value;

  value = new Date(Math.round(dygraph.xAxisRange(0)[0]));
  form.elements['xvalue1date'].value = formatDate(value);
  form.elements['xvalue1time'].value = formatTime(value);

  value = new Date(Math.round(dygraph.xAxisRange(0)[1]));
  form.elements['xvalue2date'].value = formatDate(value);
  form.elements['xvalue2time'].value = formatTime(value);
}

function updateMainChart(dygraph, initial) {

  if (!initial) {
    
     updateControlForm(dygraph);

    var form = document.getElementById('logger-control-form');
    form.submit();
  }
}

function jumpToDate(xvalue) {

  var form = document.getElementById('logger-control-form');

  var xvalue1 = parseDate(form.elements['xvalue1date'].value, form.elements['xvalue1time'].value);
  var xvalue2 = parseDate(form.elements['xvalue2date'].value, form.elements['xvalue2time'].value);
  var drift = (xvalue2.getTime() - xvalue1.getTime()) / 2;

  var date = new Date(xvalue - drift);
  form.elements['xvalue1date'].value = formatDate(date);
  form.elements['xvalue1time'].value = formatTime(date);

  date = new Date(xvalue + drift);
  form.elements['xvalue2date'].value = formatDate(date);
  form.elements['xvalue2time'].value = formatTime(date);

  form.submit();
}

function removeChartSeries(uid) {

  var form = document.getElementById('logger-control-form');
  form.elements['removed_user'].value = uid;
  form.submit();
}

function highlightChartArea(canvas, area, dygraph, xvalue1, xvalue2) {

  var point1 = dygraph.toDomCoords(xvalue1, 0)[0];
  var point2 = dygraph.toDomCoords(xvalue2, 0)[0];
  var width = area.w + area.x - point1;

  canvas.fillStyle = "#a5d2d9";
  canvas.fillRect(point1, 0, width, point2);
}