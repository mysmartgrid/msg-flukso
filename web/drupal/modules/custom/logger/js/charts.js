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

function updatePowerChartForm(chart) {

  var form = document.getElementById('logger-powerchart-form');

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

function submitPowerChartForm(resetY) {

  var form = document.getElementById('logger-powerchart-form');
  
  if (resetY) {
    form.elements['yvalue1'].value = '';
    form.elements['yvalue2'].value = '';
  }
  form.submit();
}

function submitEnergyChartForm() {

  var form = document.getElementById('logger-energychart-form');
  form.submit();
}

function submitRelativeChartForm(clickedField) {

  var form = document.getElementById('logger-relativechart-form');
  for(var i = 0; i < form.elements.length; i++) {
    if (form.elements[i].name.indexOf('selected_meters', 0) == 0 && form.elements[i].checked) {
      form.submit();
      return;
    }
  }
  clickedField.checked = true;
}

function removePowerSeries(uid, i, username) {

  var table = document.getElementById('logger-legend-table');
  table.rows[i + 1].style.display = 'none';

  powerChart.setVisibility(i, false);
  sliderChart.setVisibility(i, false);

  var form = document.getElementById('logger-powerchart-form');
  var field = form.elements['new_user'];
  var found = false;

  for (var p = 0; !found && p < field.options.length; p++) {
    found = (field.options[p].text == username);
  }

  if (!found) {
    $.get('/logger/remove/user/' + uid);

    //IE makes it necessary to use an auxiliary array
    var sorted = new Array(field.length + 1);
    for(var s = 0; s < field.length; s++){
      sorted[s] = field.options[s];
    }
    sorted[s] = new Option(username, uid);

    Array.prototype.sort.call(
      sorted,
      function (option1, option2) {
        var text1 = option1.text.toLowerCase();
        var text2 = option2.text.toLowerCase();
        return text1 < text2 ? -1 : text1 > text2 ? 1 : 0;
      }
    );

    field.length = 0;
    for(s = 0; s < sorted.length; s++){
      field.options[s] = sorted[s];
    }
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

  updatePowerChartForm(powerChart);
}

function updateSliderChart(xvalue1, xvalue2, yvalues) {

  sliderChart.updateOptions({dateWindow: sliderChart.xAxisRange()});
  updatePowerChartForm(powerChart);
}

function slidePowerChart(event, center) {

  var xvalue1 = powerChart.xAxisRange(0)[0];
  var xvalue2 = powerChart.xAxisRange(0)[1];
  var drift = (xvalue2 - xvalue1) / 2;

  xvalue1 = center - drift;
  xvalue2 = center + drift;

  powerChart.updateOptions({dateWindow: [xvalue1, xvalue2]});
  sliderChart.updateOptions({dateWindow: sliderChart.xAxisRange()});

  updatePowerChartForm(powerChart);
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

function createBarChart(id, series, names, colors, dataLabels, stacked) {

  var numTicks = names.length;
  var barWidth = stacked ? 0.5 : (1 / (series.length + 1));
  var xpos;

  var xticks = [numTicks];
  for (var i = 0; i < numTicks; i++) {
    xpos = i + (stacked ? 0 : Math.floor((series.length - 1) / 2) * barWidth);
    xticks[i] = [xpos, names[i]];
  }

  var data = [series.length];

  for (var s = 0; s < series.length; s++) {
    var values = [numTicks];

    for (var v = 0; v < numTicks; v++) {
      xpos = v + (stacked ? 0 : barWidth * s);
      values[v] = [xpos, series[s][v]];
    }
    data[s] = {data: values};
  }

  var options = {
    colors: colors,
    series: {
      bars: {
        show: true,
        barWidth: barWidth,
        lineWidth: 0,
        fill: 1,
        align: 'center'
      }
    },
    xaxis: {
      min: (-1 * barWidth),
      max: (numTicks - barWidth),
      ticks: xticks
    },
    grid: {
      clickable: true
    }
  };

  options.yaxis = {
    autoscaleMargin: 0.05,
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
    showBarDataLabels(plot, stacked, dataLabels, barWidth);
  };

  chart.plot();

  return chart;
}

function showBarDataLabels(plot, stacked, dataLabels, barWidth) {

  var series = plot.getData();
  var offset = plot.pointOffset({x: 0, y: 0});
  var floor = offset.top;
  var labelHeight = 15;

  var extraOffset = new Array();
  for(var i = 0; i < dataLabels[0].length; i++) {
    extraOffset[i] = labelHeight;
  }

  for (var d = 0; d < series.length; d++) {

    $.each(series[d].data,

      function(i, point) {

        var y = point[1];

        //Only positive values are shown
        if (y <= 0) {
          return;
        }

        var x = point[0];
        var v = Math.round(x - (stacked ? 0 : barWidth * d));

        offset = plot.pointOffset({x: x, y: y});
        var barHeight = floor - offset.top;

        //If the data labels are too close
        var top = offset.top - extraOffset[v];
        if (barHeight < labelHeight) {
          top += barHeight - labelHeight;
        }

        if (stacked) {
          extraOffset[v] += barHeight;
        }

        var value = dataLabels[d][v];
        var precision = value < 1 ? 3 : value < 100 ? 2 : 1;
        value = value.toFixed(precision);

        if (value == 0) {
          value += '...';
        }

        var fontSize = barWidth > 0.4 ? 10 : barWidth > 0.3 ? 9 : 8;

        var div = '<div style="font-size: ' + fontSize + 'px; font-weight: bold; width: 50px; height: ' +
            labelHeight + 'px; text-align: center;">' + value + '</div>';

        var options = {
            position: 'absolute',
            left: offset.left - 25,
            top: top,
            display: 'none'
          };

        $(div).css(options).appendTo(plot.getPlaceholder()).fadeIn('slow');
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