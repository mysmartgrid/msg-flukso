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

function removeChartSeries(uid) {

  var form = document.getElementById('logger-control-form');
  form.elements['removed_user'].value = uid;
  form.submit();
}