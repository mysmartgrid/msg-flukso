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

var mainChart;
var sliderChart;

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

function updateControlForm(chart) {

  var form = document.getElementById('logger-control-form');

  if (form) {
    var value = Math.round(chart.yAxisRange(0)[0]);
    form.elements['yvalue1'].value = value;

    value = Math.round(chart.yAxisRange(0)[1]);
    form.elements['yvalue2'].value = value;

    value = new Date(Math.round(chart.xAxisRange(0)[0]));
    form.elements['xvalue1date'].value = formatDate(value);
    form.elements['xvalue1time'].value = formatTime(value);

    value = new Date(Math.round(chart.xAxisRange(0)[1]));
    form.elements['xvalue2date'].value = formatDate(value);
    form.elements['xvalue2time'].value = formatTime(value);

    value = (chart.getValue(1, 0) - chart.getValue(0, 0)) / 1000;
    form.elements['resolution'].value = value;
  }

  updateLegend(chart);
}

function removeChartSeries(uid) {

  var form = document.getElementById('logger-control-form');
  form.elements['removed_user'].value = uid;
  form.submit();
}

function updateMainChart(xvalue1, xvalue2, yvalues) {

  mainChart.updateOptions({dateWindow: [xvalue1, xvalue2]});
  sliderChart.updateOptions({dateWindow: null});

  updateControlForm(mainChart);
}

function updateSliderChart(xvalue1, xvalue2, yvalues) {

  sliderChart.updateOptions({dateWindow: sliderChart.xAxisRange()});
  updateControlForm(mainChart);
}

function jumpToDate(event, center) {

  var xvalue1 = mainChart.xAxisRange(0)[0];
  var xvalue2 = mainChart.xAxisRange(0)[1];
  var drift = (xvalue2 - xvalue1) / 2;
  
  xvalue1 = center - drift;
  xvalue2 = center + drift;

  mainChart.updateOptions({dateWindow: [xvalue1, xvalue2]});
  sliderChart.updateOptions({dateWindow: sliderChart.xAxisRange()});

  updateControlForm(mainChart);
}

function highlightChartArea(canvas, area, sliderChart) {

  var xvalue1 = mainChart.xAxisRange(0)[0];
  var xvalue2 = mainChart.xAxisRange(0)[1];
  var yvalue1 = sliderChart.yAxisRange(0)[0];
  var yvalue2 = sliderChart.yAxisRange(0)[1];

  var point1 = sliderChart.toDomCoords(xvalue1, 0)[0];
  var point2 = sliderChart.toDomCoords(xvalue2, 0)[0];
  var width = point2 - point1;
  var height = yvalue2 - yvalue1;

  canvas.fillStyle = "#a5d2d9";
  canvas.fillRect(point1, area.y, width, height);
}

function hideZeroY(value) {
  return value == 0 ? "" : value;
}

function updateLegend(chart) {

  var minVisibleDate = chart.xAxisRange(0)[0];
  var maxVisibleDate = chart.xAxisRange(0)[1];
  var avg;
  var value;

  //sensors
  for (var s = 1; s < chart.numColumns(); s++) {

    var max = Number.MIN_VALUE;
    var min = Number.MAX_VALUE;
    var sum = 0;
    var total = 0;

    //sensor's values
    for(var v = 0; v < chart.numRows(); v++) {
      
      var timestamp = chart.getValue(v, 0);

      if (timestamp >= minVisibleDate && timestamp <= maxVisibleDate) {

        value = chart.getValue(v, s);

        if (value > 0) {
          max = value > max ? value : max;
          min = value < min ? value : min;
          sum += value;
          total++;
        }
      }
    }

    if(total > 0) {
      avg = sum / total;

    } else {
      max = null;
      min = null;
      avg = null;
      value = null;
    }

    updateLegendValue("max", s, max);
    updateLegendValue("min", s, min);
    updateLegendValue("avg", s, avg);
    updateLegendValue("last", s, value);
  }
}

function updateLegendValue(name, i, value) {

  var div = document.getElementById(name + --i);
  div.innerHTML = value > 0 ? Math.round(value * 10) / 10 : '';
}