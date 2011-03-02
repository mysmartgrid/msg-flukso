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
var monitorChart;

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

function removeChartSeries(uid, i, username) {

  var table = document.getElementById('logger-legend-table');
  table.rows[i + 1].style.display = 'none';

  mainChart.setVisibility(i, false);
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

  mainChart.setAnnotations(new Array());
  
  mainChart.updateOptions({rollPeriod: level});
  sliderChart.updateOptions({rollPeriod: level});

  $.get('/logger/smoothinglevel/' + level);

  return true;
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
  return value == 0 ? "" : value.toFixed(2);
}

function updateLegend(chart) {

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

    updateLegendValue("max", s, max);
    updateLegendValue("min", s, min);
    updateLegendValue("avg", s, avg);
    updateLegendValue("last", s, last);
  }
}

function updateLegendValue(name, i, value) {

  var div = document.getElementById(name + --i);
  div.innerHTML = value > 0 ? value.toFixed(2) : '';
}

function updateChartColors(chart, i, color) {

  var values = chart.getColors();
  values[i] = "#" + color;
  chart.updateOptions({colors: values});
}

function addPointAnnotation(event, point) {

  var date = new Date(point.xval);
  var yval = point.yval.toFixed(2);
  var text = point.name + ': ' + formatDate(date) + ' - ' + yval;

  if (!removePointAnnotation(mainChart, text)) {

    var annotations = mainChart.annotations();

    var width = 50 * ('' + yval).length / 6;

    annotations.push({
      series: point.name,
      x: formatCVSTimeStamp(date),
      shortText: yval,
      text: text,
      width: width,
      clickHandler: function(annotation, point, chart, event) {
        removePointAnnotation(chart, annotation.text);
      }
    });

    mainChart.setAnnotations(annotations);
  }
}

function removePointAnnotation(chart, text) {

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

function formatFraction(n, d) {
  return (n > 0 && d > 0) ? n / d : 0;
}

function createMonitorChart(id, current, average, intervals, names, colors) {

  var xticks = [names.length];
  var data1  = [names.length];
  var data2  = [names.length];
  var data3  = [names.length];
  var perc;

  for (var i = 0; i < names.length; i++) {

    perc = current[i] / average[i];

    data1[i]  = [i, (perc < 1 ? perc     : 1)];
    data2[i]  = [i, (perc < 1 ? 1 - perc : 0)];
    data3[i]  = [i, (perc > 1 ? perc - 1 : 0)];
    xticks[i] = [i, names[i]];
  }

  var data = [ {data: data1}, {data: data2}, {data: data3} ];
  var options = {
      colors: colors,
      series: {
        stack: 0,
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
      yaxis: {
        tickFormatter: function (value, axis) {
          return (value * 100).toFixed(0) + ' %';
        }
      },
      grid: {
        clickable: true
      }
    };

  monitorChart = new Object();
  monitorChart.id = id;
  monitorChart.data = data;
  monitorChart.options = options;

  monitorChart.plot = function() {
    $.plot($('#' + id), data, options);

    $('#' + id).bind('plotclick',
      function (event, pos, item) {
        if (item) {
          window.location = '/logger/electricity/' + intervals[item.dataIndex];
        }
      }
    );
  };

  monitorChart.plot();
}

function setSeriesColor(chartId, i, color) {

  if (chartId == 'monitor') {
    monitorChart.options['colors'][i] = '#' + color;
    monitorChart.plot();

  } else {
    updateChartColors(mainChart, i, color);
    updateChartColors(sliderChart, i, color);
  }

  $.get('/logger/color/' + chartId + '/' + i + '/' + escape('#' + color));
}