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

var powerChart;
var sliderChart;
var relativeChart;
var energyChart;

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

function formatCVSTimeStamp(d) {
  return '' +
    d.getFullYear() + '-' +
    (d.getMonth() <  9 ? '0' : '') + (d.getMonth() + 1) + '-' +
    (d.getDate()  < 10 ? '0' : '') + d.getDate() + ' ' +
    (d.getHours()   < 10 ? '0' : '') + d.getHours() + ':' +
    (d.getMinutes() < 10 ? '0' : '') + d.getMinutes();
}

function hideZero(value) {
  return value == 0 ? "" : value.toFixed(2);
}

function updateControlForm(chart) {

  var form = document.getElementById('logger-control-form');

  if (form) {
    var yvalue1 = Math.round(chart.yAxisRange(0)[0]);
    var yvalue2 = Math.round(chart.yAxisRange(0)[1]);

    if (yvalue2 == 0 && yvalue1 == 0) {
      yvalue1 = '';
      yvalue2 = '';
    }

    form.elements['yvalue1'].value = yvalue1;
    form.elements['yvalue2'].value = yvalue2;

    var value = new Date(Math.round(chart.xAxisRange(0)[0]));
    form.elements['xvalue1date'].value = formatDate(value);
    form.elements['xvalue1time'].value = formatTime(value);

    value = new Date(Math.round(chart.xAxisRange(0)[1]));
    form.elements['xvalue2date'].value = formatDate(value);
    form.elements['xvalue2time'].value = formatTime(value);

    value = (chart.getValue(1, 0) - chart.getValue(0, 0)) / 1000;
    form.elements['resolution'].value = value;
  }

  updatePowerLegend(chart);
}

function removePowerSeries(uid, i, username) {

  var table = document.getElementById('logger-legend-table');
  table.rows[i + 1].style.display = 'none';

  powerChart.setVisibility(i, false);
  sliderChart.setVisibility(i, false);

  var form = document.getElementById('logger-control-form');
  var field = form.elements['new_user'];
  var found = false;

  for (var p = 0; !found && p < field.options.length; p++) {
    found = (field.options[p].text == username);
  }

  if (!found) {
    field.options[field.length] = new Option(username, uid);

    Array.prototype.sort.call(
      field.options,
      function (option1, option2) {
        var text1 = option1.text.toLowerCase();
        var text2 = option2.text.toLowerCase();
        return text1 < text2 ? -1 : text1 > text2 ? 1 : 0;
      }
    );

    $.get('/logger/remove/user/' + uid);
  }
}

function updateSmoothingLevel(fieldId, step) {

  var field = document.getElementById(fieldId);
  var level = field.value * 1 + step;
  level = level < 1 ? 1 : level;
  field.value = level;

  powerChart.setAnnotations(new Array());

  powerChart.updateOptions({rollPeriod: level});
  sliderChart.updateOptions({rollPeriod: level});

  $.get('/logger/smoothinglevel/' + level);

  return true;
}

function updatePowerChart(xvalue1, xvalue2, yvalues) {

  powerChart.updateOptions({dateWindow: [xvalue1, xvalue2]});
  sliderChart.updateOptions({dateWindow: null});

  updateControlForm(powerChart);
}

function updateSliderChart(xvalue1, xvalue2, yvalues) {

  sliderChart.updateOptions({dateWindow: sliderChart.xAxisRange()});
  updateControlForm(powerChart);
}

function slidePowerChart(event, center) {

  var xvalue1 = powerChart.xAxisRange(0)[0];
  var xvalue2 = powerChart.xAxisRange(0)[1];
  var drift = (xvalue2 - xvalue1) / 2;

  xvalue1 = center - drift;
  xvalue2 = center + drift;

  powerChart.updateOptions({dateWindow: [xvalue1, xvalue2]});
  sliderChart.updateOptions({dateWindow: sliderChart.xAxisRange()});

  updateControlForm(powerChart);
}

function highlightPowerChart(canvas, area, sliderChart) {

  var xvalue1 = powerChart.xAxisRange(0)[0];
  var xvalue2 = powerChart.xAxisRange(0)[1];
  var yvalue1 = sliderChart.yAxisRange(0)[0];
  var yvalue2 = sliderChart.yAxisRange(0)[1];

  var point1 = sliderChart.toDomCoords(xvalue1, 0)[0];
  var point2 = sliderChart.toDomCoords(xvalue2, 0)[0];
  var width = point2 - point1;
  var height = yvalue2 - yvalue1;

  canvas.fillStyle = "#a5d2d9";
  canvas.fillRect(point1, area.y, width, height);
}

function updatePowerLegend(chart) {

  var minVisibleDate = chart.xAxisRange(0)[0];
  var maxVisibleDate = chart.xAxisRange(0)[1];
  var value;
  var total;
  var sum;
  var max;
  var min;
  var avg;
  var last;

  //sensors
  for (var s = 1; s < chart.numColumns(); s++) {

    max = Number.MIN_VALUE;
    min = Number.MAX_VALUE;
    sum = 0;
    total = 0;

    //sensor's values
    for(var v = 0; v < chart.numRows(); v++) {

      var timestamp = chart.getValue(v, 0);

      if (timestamp >= minVisibleDate && timestamp <= maxVisibleDate) {

        value = chart.getValue(v, s);

        if (value > 0) {
          last = value;
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
      last = null;
    }

    updatePowerLegendValue("max", s, max);
    updatePowerLegendValue("min", s, min);
    updatePowerLegendValue("avg", s, avg);
    updatePowerLegendValue("last", s, last);
  }
}

function updatePowerLegendValue(name, i, value) {

  var div = document.getElementById(name + --i);
  div.innerHTML = value > 0 ? value.toFixed(2) : '';
}

function addPowerAnnotation(event, point) {

  var date = new Date(point.xval);
  var yval = point.yval.toFixed(2);
  var text = point.name + ': ' + formatDate(date) + ' - ' + yval;

  if (!removePowerAnnotation(powerChart, text)) {

    var annotations = powerChart.annotations();

    var width = 50 * ('' + yval).length / 6;

    annotations.push({
      series: point.name,
      x: formatCVSTimeStamp(date),
      shortText: yval,
      text: text,
      width: width,
      clickHandler: function(annotation, point, chart, event) {
        removePowerAnnotation(chart, annotation.text);
      }
    });

    powerChart.setAnnotations(annotations);
  }
}

function removePowerAnnotation(chart, text) {

  var remaining = new Array();
  var annotations = chart.annotations();

  for (var i = 0; i < annotations.length; i++) {
    if (annotations[i].text != text) {
      remaining.push(annotations[i]);
    }
  }

  if (remaining.length != annotations.length) {
    chart.setAnnotations(remaining);
    return true;

  } else {
    return false;
  }
}

function createBarChart(id, values, names, colors, dataLabels, stacked) {

  var xticks = [names.length];

  for (var i = 0; i < names.length; i++) {
    xticks[i] = [i, names[i]];
  }

  var data = [values.length];

  for (var v = 0; v < values.length; v++) {
    var series = [names.length];

    for (i = 0; i < names.length; i++) {
      series[i] = [i, values[v][i]];
    }
    data[v] = {data: series};
  }

  var options = {
    colors: colors,
    series: {
      bars: {
        show: true,
        barWidth: 0.5,
        lineWidth: 0,
        fill: 1,
        align: 'center'
      }
    },
    xaxis: {
      min: -0.5,
      max: (names.length - 0.5),
      ticks: xticks
    },
    grid: {
      clickable: true
    }
  };

  options.yaxis = {
    tickFormatter: function (value, axis) {
      return stacked ? value.toFixed(0) + ' %' : value.toFixed(2);
    }
  };

  if (stacked) {
    options.series.stack = 0;
  }

  var chart = new Object();
  chart.id = id;
  chart.data = data;
  chart.options = options;

  chart.plot = function() {
    var plot = $.plot($('#' + id), data, options);
    showBarDataLabels(plot, stacked, dataLabels);
  };

  chart.plot();

  return chart;
}

function showBarDataLabels(plot, stacked, dataLabels) {

  var series = plot.getData();
  var offset = plot.pointOffset({x: 0, y: 0});
  var floor = offset.top;

  var extraOffset = [20, 20, 20, 20, 20, 20, 20];

  for(var d = 0; d < series.length; d++) {

    $.each(series[d].data,

      function(i, point){

        if (point[1] > 0) {
          var x = point[0];
          var y = point[1];
          offset = plot.pointOffset({x: x, y: y});

          if (dataLabels.length <= d || dataLabels[d].length <= x) {
            return;
          }

          //point[1].toFixed(2)
          $('<div style="font-size: 10px; font-weight: bold">' + dataLabels[d][x].toFixed(2) + '</div>').css(
            {
              position: 'absolute',
              left: offset.left - 15,
              top: offset.top - extraOffset[x],
              display: 'none'
            }
          ).appendTo(plot.getPlaceholder()).fadeIn('slow');

          if (stacked) {
            extraOffset[x] += floor - offset.top;
          }
        }
    });
  }
}

function setSeriesColor(chartId, i, color) {

  color = '#' + color;

  if (chartId == 'relative') {
    relativeChart.options['colors'][i] = color;
    relativeChart.plot();

  } else if (chartId == 'energy') {
    energyChart.options['colors'][i] = color;
    energyChart.plot();

  } else {
    updateDygraphColor(powerChart, i, color);
    updateDygraphColor(sliderChart, i, color);
  }

  $.get('/logger/color/' + chartId + '/' + i + '/' + escape(color));
}

function updateDygraphColor(chart, i, color) {

  var values = chart.getColors();
  values[i] = color;
  chart.updateOptions({colors: values});
}