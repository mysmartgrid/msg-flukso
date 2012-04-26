/**
 * Javascript functions used by module logger to build charts.
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

var lineChart;
var sliderChart;
var barChart;

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

/**
 * Look through stylesheets in reverse order that they appear in the document.
 */
function getStyleBySelector(selector) {

  var sheets = document.styleSheets;
  var rules, imported, rule, style, s, r, i;
  var isIE = sheets[0].cssRules == undefined;

  for (s = sheets.length - 1; s >= 0; s--) {
    rules = isIE ? sheets[s].rules : sheets[s].cssRules;

    style = findStyle(rules, selector);
    if (style != null) {
      return style;
    }
 
    rules = isIE ? sheets[s].imports : rules;

    for (r = 0; r < rules.length; r++){
      var imported = isIE ? rules[r].rules : rules[r].styleSheet.cssRules;

      style = findStyle(imported, selector);
      if (style != null) {
        return style;
      }
    }
  }
  return null;
}

function findStyle(rules, selector) {

  var r, rule;
  for (r = 0; r < rules.length; r++) {
    rule = rules[r];

    if (rule.selectorText && rule.selectorText.toLowerCase() == selector){
      return rule.style;
    }
  }
  return null;
}

function percentToPx(value, max) {
  if (value.indexOf('%') > -1)  {
    var perc = parseInt(value.replace("%",""));
    return perc * max / 100;
  } else {
    return value.replace("px","");
  }
}

function isMobile() {
  //If client is a mobile device
  return document.body.clientWidth < 600;
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

function submitEnergyChartForm(clickedField) {

  var form = document.getElementById('logger-energychart-form');
  for(var i = 0; i < form.elements.length; i++) {
    if (form.elements[i].name.indexOf('selected_sensor_types', 0) == 0 && form.elements[i].checked) {
      form.submit();
      return;
    }
  }
  clickedField.checked = true;
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

function removePowerSeries(uid, i, username, tableId) {

  var table = document.getElementById(tableId);

  //Check if it is the last visible one
  var visibleOnes = 0;
  for (var v = 1; v < table.rows.length; v++) {
    visibleOnes += (table.rows[v].style.display == 'none') ? 0 : 1;
  }
  if (visibleOnes == 1) {
    return;
  }

  table.rows[i + 1].style.display = 'none';

  lineChart.setVisibility(i, false);
  if (sliderChart) {
    sliderChart.setVisibility(i, false);
  }

  var form = document.getElementById('logger-powerchart-form');
  var field = form.elements['new_user'];
  var found = false;

  for (var p = 0; !found && p < field.options.length; p++) {
    found = (field.options[p].text == username);
  }

  if (!found) {
    jQuery.get('/logger/remove/user/' + uid);

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

  lineChart.setAnnotations(new Array());

  lineChart.updateOptions({rollPeriod: level});
  if (sliderChart) {
    sliderChart.updateOptions({rollPeriod: level});
  }

  jQuery.get('/logger/setvariable/smoothing_level/' + level);

  return true;
}

function updatePowerChart(xvalue1, xvalue2, yvalues) {

  lineChart.updateOptions({dateWindow: [xvalue1, xvalue2]});
  if (sliderChart) {
    sliderChart.updateOptions({dateWindow: null});
  }

  updatePowerChartForm(lineChart);
}

function updateSliderChart(xvalue1, xvalue2, yvalues) {

  sliderChart.updateOptions({dateWindow: sliderChart.xAxisRange()});
  updatePowerChartForm(lineChart);
}

function expandPowerChart() {
  lineChart.updateOptions({dateWindow: null});
  lineChart.updateOptions({valueRange: null});
  updateSliderChart();
}

function slidePowerChart(event, center) {

  var xvalue1 = lineChart.xAxisRange(0)[0];
  var xvalue2 = lineChart.xAxisRange(0)[1];
  var drift = (xvalue2 - xvalue1) / 2;

  xvalue1 = center - drift;
  xvalue2 = center + drift;

  lineChart.updateOptions({dateWindow: [xvalue1, xvalue2]});
  sliderChart.updateOptions({dateWindow: sliderChart.xAxisRange()});

  updatePowerChartForm(lineChart);
}

function highlightPowerChart(canvas, area, sliderChart) {

  var xvalue1 = lineChart.xAxisRange(0)[0];
  var xvalue2 = lineChart.xAxisRange(0)[1];
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
  if (div) {
    div.innerHTML = value > 0 ? value.toFixed(2) : '';
  }
}

function addPowerAnnotation(event, point) {

  var id = 'annotation' + point.name + '' + point.xval + '' + point.yval;
  var div = findAnnotationDiv(id);

  if (!div) {
    var date = new Date(point.xval);
    var yval = point.yval.toFixed(2);
    var text = point.name + ': ' + formatDate(date) + ' - ' + yval;
    var width = 50 * ('' + yval).length / 6;

    var appliances = getAppliances(point.name, date);
    var icons = '';
    for (var i = 0; i < appliances.length; i++) {
      icons += '<img class="point-annotation" src="/sites/all/modules/logger/img/appliances/icon-' + appliances[i] + '.jpg"/>';
      width += 35;
    }

    var annotations = new Array();
    annotations.push({
      series: point.name,
      x: formatCVSTimeStamp(date),
      shortText: yval,
      text: text,
      width: width,
      height: 30,
      cssClass: 'point-annotation ' + id,
      clickHandler: function(annotation, point, chart, event) {

        removePowerAnnotation(chart, annotation.text);
      }
    });

    lineChart.setAnnotations(annotations);

    div = findAnnotationDiv(id);
    div.innerHTML = '<span class="point-annotation">' + div.innerHTML + '</span>' + icons;
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
  chart.setAnnotations(remaining);
}

function findAnnotationDiv(className) {

  var divs = document.getElementsByTagName('div');
  for (var i = 0; i < divs.length; i++) {
    var div = divs[i];
    if (div.className.indexOf(className) != -1) {
      return div;
    }
  }
  return null;
}

function getAppliances(meter, timestamp) {

  var apps = new Array();

  jQuery.ajax({
    async: false,
    url: '/logger/getappliances/' + meter + '/' + timestamp, 
    success: function (result) {
      jQuery.each(result, function(i, id) {
        apps[i] = id;
      });
    }
  });
  return apps;
}

function createLineChart(id, fileURL, properties) {

  var div = document.getElementById(id);
  var clazz = getStyleBySelector('div.' + div.className);

  //Dygraph in IE fails if % or px are informed
  properties.width = percentToPx(clazz.width, div.clientWidth);
  properties.height = percentToPx(clazz.height);

  properties.axisLabelFontSize = clazz.fontSize.replace("px","");
  properties.yAxisLabelWidth = properties.axisLabelFontSize * 4;

  var chart = new Dygraph(div, '/' + fileURL, properties);
  storeChart(id, chart);

  return chart;
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
      return stacked ? value.toFixed(0) + '%' : value.toFixed(1);
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
    var plot = jQuery.plot(jQuery('#' + id), data, options);
    showBarDataLabels(plot, stacked, dataLabels, barWidth);
  };
  chart.plot();

  storeChart(id, chart);

  return chart;
}

function storeChart(id, chart) {

  if (id == 'lineChart') {
    lineChart = chart;
    sliderChart = null;
    barChart = null;

  } else if (id == 'sliderChart') {
    sliderChart = chart;
    barChart = null;

  } else if (id == 'barChart') {
    barChart = chart;
    lineChart = null;
    sliderChart = null;
  }
}

function getChart(id) {

  if (id == 'lineChart') {
    return lineChart;

  } else if (id == 'sliderChart') {
    return sliderChart;

  } else if (id == 'barChart') {
    return barChart;
  }
  return null;
}

function showBarDataLabels(plot, stacked, dataLabels, barWidth) {

  var labelDivClass = getStyleBySelector('p.chart-label');

  var series = plot.getData();
  var offset = plot.pointOffset({x: 0, y: 0});
  var floor = offset.top;
  var labelHeight = 15;

  var extraOffset = new Array();
  for(var i = 0; i < dataLabels[0].length; i++) {
    extraOffset[i] = labelHeight;
  }

  for (var d = 0; d < series.length; d++) {

    jQuery.each(series[d].data,

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

        var fontSize = parseInt(labelDivClass.fontSize.replace('px', ''));
        fontSize += (barWidth > 0.4 ? 2 : barWidth > 0.3 ? 1 : 0);

        var value = dataLabels[d][v];
        var precision = value < 1 ? 3 : value < 100 ? 2 : 1;
        precision -= fontSize < 8 ? 1 : 0;

        value = value.toFixed(precision);

        if (value == 0) {
          value += '...';
        }

        var div = '<div style="font-size: ' + fontSize + 'px; font-weight: bold; width: 50px; height: ' +
            labelHeight + 'px; text-align: center;">' + value + '</div>';

        var options = {
            position: 'absolute',
            left: offset.left - 25,
            top: top,
            display: 'none'
          };

        jQuery(div).css(options).appendTo(plot.getPlaceholder()).fadeIn('slow');
    });
  }
}

function setSeriesColor(chartId, i, color) {

  color = '#' + color;

  //TODO: avoid these tests
  if (chartId == 'energy' || chartId == 'relative') {
    barChart.options['colors'][i] = color;
    barChart.plot();

  } else {
    //FIXME: improve this code
    if (lineChart) {
      updateDygraphColor(lineChart, i, color);
    }
    if (sliderChart) {
      updateDygraphColor(sliderChart, i, color);
    }
  }

  jQuery.get('/logger/setvariable/series_color_' + chartId + '_' + i + '/' + escape(color));
}

function updateDygraphColor(chart, i, color) {

  var values = chart.getColors();
  values[i] = color;
  chart.updateOptions({colors: values});
}

function resizeCharts() {

  if (isMobile()) {

    resizeLineChart('lineChart');
    resizeLineChart('sliderChart');
    resizeBarChart();
  }
}

function resizeLineChart(id) {

  var chart = getChart(id);

  //TODO: simplify this method
  if (chart) {
    var isLarge = document.body.clientWidth > 400;

    var div = document.getElementById(id);
    var clazz = getStyleBySelector('div.' + div.className);
    var height = clazz.height.replace("px","");
    var width = isLarge ? 460 : 295; //Cellphone possibilities
    chart.resize(width, height);
  }
}

function resizeBarChart() {
  if (barChart) {
    barChart.plot();
  }
}

function resizeLegend(id, col1, col2) {

  if (isMobile()) {

    var table = document.getElementById(id);
    if (table) {
      var isLarge = document.body.clientWidth > 400;
      var display = isLarge ? 'table-cell' : 'none';
      col1 = col1 ? col1 : 3;
      col2 = col2 ? col2 : table.rows[0].cells.length - 1;

      for(var r = 0; r < table.rows.length; r++) {
        var cells = table.rows[r].cells;
        for(var c = col1; c < col2; c++) {
          cells[c].style.display = display;
        }
      }
    }
  }
}
