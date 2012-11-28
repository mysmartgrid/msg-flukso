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

function updateLineChartForm(chart) {

  var form = document.getElementById('logger-linechart-form');

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
  }
  refreshLineAnnotations();
  updateLineLegend(chart);
}

function submitLineChartForm(resetY) {

  var form = document.getElementById('logger-linechart-form');
  
  if (resetY) {
    form.elements['yvalue1'].value = '';
    form.elements['yvalue2'].value = '';
  }
  form.submit();
}

function enableFilledGraph() {
  var form = document.getElementById('logger-linechart-form');
  form.elements['filled_graph'].checked = 1;
}

function submitBarChartForm(clickedField) {

  //If form contains fields selected_sensor_types or selected_meters, at least one option must be selected in each

  var form = document.getElementById('logger-barchart-form');
  var checkBoxes = findCheckBoxes(form, 'selected_meters');
  var checked = false;
  var found = false;

  if (checkBoxes.length > 0) {
    found = true;
    checked = isAnyOptionChecked(checkBoxes);
  }

  if (!found || checked) {
    checkBoxes = findCheckBoxes(form, 'selected_sensor_types');
    found = false;

    if (checkBoxes.length > 0) {
      found = true;
      checked = isAnyOptionChecked(checkBoxes);
    }
  }

  if (!found || checked) {
    form.submit();
    return;
  }
  clickedField.checked = true;
}

function isAnyOptionChecked(checkBoxes) {
  var checked = false;
  for(var i = 0; i < checkBoxes.length; i++) {
    if (checkBoxes[i].checked) {
      checked = true;
      break;
    }
  }
  return checked;
}

function findCheckBoxes(form, fieldPrefix) {

  var checkBoxes = new Array();
  var c = 0;
  for(var i = 0; i < form.elements.length; i++) {

    if (form.elements[i].name.indexOf(fieldPrefix, 0) == 0) {
      checkBoxes[c++] = form.elements[i];
    }
  }
  return checkBoxes;
}

function removeLineSeries(uid, i, username, meter, tableId, context) {

  var visibilities = lineChart.visibility();

  if (isLastVisibleLine(i, visibilities)) {
    return;
  }

  //Remove the table line as soon as possible
  var table = document.getElementById(tableId);
  table.rows[i + 1].style.display = 'none';

  setLineVisibility(i, false);

  var form = document.getElementById('logger-linechart-form');

  jQuery.get('/logger/remove/sensor/' + meter + '/' + context);
}

function hideLineSeries(uid, i, username, meter, hideText, showText, context) {

  var visibilities = lineChart.visibility();

  if (isLastVisibleLine(i, visibilities)) {
    return;
  }

  var style, text, visible = !visibilities[i];

  if (visible) {
    text = hideText;
    style = 'visible';

  } else {
    text = showText;
    style = 'hidden';
  }

  document.getElementById('hide-legend-row' + i).innerHTML = text;

  setLineVisibility(i, visible);

  document.getElementById('max' + i).style.visibility = style;
  document.getElementById('min' + i).style.visibility = style;
  document.getElementById('avg' + i).style.visibility = style;
  document.getElementById('last' + i).style.visibility = style;

  jQuery.get('/logger/hide/sensor/' + meter + '/' + context);
}

function isLastVisibleLine(i, visibilities) {

  var visibleOnes = 0;
  for (var s = 0; s < visibilities.length; s++) {
    visibleOnes += visibilities[s] ? 1 : 0;
  }
  return visibilities[i] &&  visibleOnes == 1;
}

function setLineVisibility(i, visible) {

  lineChart.setVisibility(i, visible);
  if (sliderChart) {
    sliderChart.setVisibility(i, visible);
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

function updateLineChart(xvalue1, xvalue2, yvalues) {

  lineChart.updateOptions({dateWindow: [xvalue1, xvalue2]});
  if (sliderChart) {
    sliderChart.updateOptions({dateWindow: null});
  }

  updateLineChartForm(lineChart);
}

function updateSliderChart(xvalue1, xvalue2, yvalues) {

  sliderChart.updateOptions({dateWindow: sliderChart.xAxisRange()});
  updateLineChartForm(lineChart);
}

function expandLineChart() {
  lineChart.updateOptions({dateWindow: null});
  lineChart.updateOptions({valueRange: null});
  updateSliderChart();
}

function slideLineChart(event, center) {

  var xvalue1 = lineChart.xAxisRange(0)[0];
  var xvalue2 = lineChart.xAxisRange(0)[1];
  var drift = (xvalue2 - xvalue1) / 2;

  xvalue1 = center - drift;
  xvalue2 = center + drift;

  lineChart.updateOptions({dateWindow: [xvalue1, xvalue2]});
  sliderChart.updateOptions({dateWindow: sliderChart.xAxisRange()});

  updateLineChartForm(lineChart);
}

function highlightLineChart(canvas, area, sliderChart) {

  var xvalue1 = lineChart.xAxisRange(0)[0];
  var xvalue2 = lineChart.xAxisRange(0)[1];
  var yvalue1 = sliderChart.yAxisRange(0)[0];
  var yvalue2 = sliderChart.yAxisRange(0)[1];

  var point1 = sliderChart.toDomCoords(xvalue1, 0)[0];
  var point2 = sliderChart.toDomCoords(xvalue2, 0)[0];
  var width = point2 - point1;
  var height = yvalue2 - yvalue1 + 100;

  canvas.fillStyle = "#a5d2d9";
  canvas.fillRect(point1, area.y, width, height);
}

function updateLineLegend(chart) {

  var minVisibleDate = chart.xAxisRange(0)[0];
  var maxVisibleDate = chart.xAxisRange(0)[1];
  var lineVisibility = chart.visibility();

  //sensors
  for (var s = 1; s < chart.numColumns(); s++) {

    var max = Number.MIN_VALUE;
    var min = Number.MAX_VALUE;
    var sum = 0;
    var total = 0;
    var last = null;
    
    if (lineVisibility[s - 1]) {

      //sensor's values
      for (var v = 0; v < chart.numRows(); v++) {
        var timestamp = chart.getValue(v, 0);

        if (timestamp >= minVisibleDate && timestamp <= maxVisibleDate) {

          var value = chart.getValue(v, s);
          value = value ? value : 0;
          last = value;
          max = value > max ? value : max;
          min = value < min ? value : min;

          if (value != 0) {
            sum += value;
            total++;
          }
        }
      }
    }
    max = max == Number.MIN_VALUE ? null : max;
    min = min == Number.MAX_VALUE ? null : min;
    avg = total > 0 ? (sum / total) : null;

    updateLineLegendValue("max", s, max);
    updateLineLegendValue("min", s, min);
    updateLineLegendValue("avg", s, avg);
    updateLineLegendValue("last", s, last);
    updateLineLegendValue("sum", s, sum);
  }
}

function updateLineLegendValue(name, i, value) {

  var div = document.getElementById(name + --i);
  if (div) {
    if (value && !isNaN(value - 0)) {
      var abs = Math.abs(value);
      value = abs >= 0.01 ? value.toFixed(2) : abs >= 0.001 ? value.toFixed(3) : value;
      div.innerHTML = abs == 0 ? '' : abs < 0.001 ? '0.000...' : value;
    } else {
      div.innerHTML = '';
    }
  }
}

var annotationDivs = {};

function addLineAnnotation(event, point) {

  var seriesId = point.name.substring(1) - 1;
  var annotationId = 'annotation' + seriesId + '' + point.xval;

  var hour = (point.xval - (point.xval % 3600000)) / 1000;
  var date = new Date(point.xval);
  var timestamp = (date.getTime() - date.getTime() % 1000) / 1000;
  var yval = point.yval.toFixed(2);
  var text = point.name + ': ' + formatDate(date) + ' - ' + yval;
  var width = 50 * ('' + yval).length / 6;
  var height = 15;
  var icons = '';

  var appliances = getAppliances(seriesId, timestamp);
  if (appliances) {
    icons += appliances;
    width += 45 * (appliances.length / 100);
    height = 30;    
  }

  var weather = getWeather(seriesId, hour);
  if (weather) {
    icons += weather;
    width += 75;
    height = 30;
  }

  var annotation = {
    series: point.name,
    x: formatCVSTimeStamp(date),
    shortText: yval,
    text: text,
    width: width,
    height: height,
    cssClass: 'point-annotation ' + annotationId,
    clickHandler: function(annotation, point, chart, event) {

      removeLineAnnotation(chart, annotationId);
    }
  };

  var annotations = lineChart.annotations();
  annotations.push(annotation);

  lineChart.setAnnotations(annotations);

  var div = findAnnotationDiv(annotationId);
  if (weather || appliances) {
    div.innerHTML = '<span class="point-annotation">' + yval + '</span>' + icons;
  }
  annotationDivs[annotationId] = div.innerHTML;

  refreshLineAnnotations();
}

function refreshLineAnnotations() {

  for (id in annotationDivs) {
    var div = findAnnotationDiv(id);
    if (div) {
      div.innerHTML = annotationDivs[id];
    }
  }
}

function removeLineAnnotation(chart, annotationId) {

  var remaining = new Array();
  var annotations = chart.annotations();

  for (var i = 0; i < annotations.length; i++) {
    if (annotations[i].cssClass.indexOf(annotationId) < 0) {
      remaining.push(annotations[i]);
    }
  }
  chart.setAnnotations(remaining);
  refreshLineAnnotations();
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

function getAppliances(seriesIndex, timestamp) {

  var apps = new Array();
  var cache_id = lineChart.fileURL.substring(lineChart.fileURL.lastIndexOf('/') + 1);

  jQuery.ajax({
    async: false,
    url: '/logger/getappliances/' + cache_id, 
    success: function (result) {
      apps = result;
    }
  });

  var icons = '';

  for (a in apps[seriesIndex]) {
    var flag = parseInt(apps[seriesIndex][a][timestamp]);
    if (flag > 0) {
      icons += '<img class="point-annotation" src="/sites/all/modules/logger/img/appliances/icon-' + a + '.jpg"/>';
    }
  }
  return icons;
}

function getWeather(seriesIndex, time) {

  var weather = lineChart.weather;

  for (var i = 0; i < weather.length; i++) {
    if (weather[i][0] == seriesIndex) {
      for (var j = 0; j < weather[i][1].length; j++) {
        if (weather[i][1][j][0] == time) {
          var iconId = weather[i][1][j][1];
          var temperature = weather[i][1][j][2];
          return '<span class="point-annotation">&nbsp;&nbsp;(' + temperature + '°)</span>' +
            '<img class="point-annotation" src="/sites/all/modules/logger/img/weather/icon-' + iconId + '.jpg"/>';
        }
      }
    }
  }
  return null;
}

function createLineChart(id, fileURL, properties, weather) {

  var div = document.getElementById(id);
  var clazz = getStyleBySelector('div.' + div.className);

  //Dygraph in IE fails if % or px are informed
  properties.width = percentToPx(clazz.width, div.clientWidth);
  properties.height = percentToPx(clazz.height);

  properties.axisLabelFontSize = clazz.fontSize.replace("px","");
  properties.yAxisLabelWidth = properties.axisLabelFontSize * 4;

  var chart = new Dygraph(div, '/' + fileURL, properties);
  chart.weather = weather;
  chart.fileURL = fileURL;
  storeChart(id, chart);

  return chart;
}

function createBarChart(id, series, names, colors, dataLabels, dataLabels2, stacks, percent) {

  var stacked = stacks.length > 0;
  var numTicks = names.length;
  var barsPerPoint = stacked ? (Math.max.apply(null, stacks) + 1) : series.length;
  var barWidth = 1 / (barsPerPoint + 1);
  var xpos;

  var xticks = [numTicks];
  for (var i = 0; i < numTicks; i++) {
    xpos = i + (stacked ? 0 : Math.floor((series.length - 1) / 2) * barWidth);
    xticks[i] = [xpos, names[i]];
  }

  var data = [series.length];

  for (var s = 0; s < series.length; s++) {

    var values = [numTicks];
    var k = stacks[s];

    for (var v = 0; v < numTicks; v++) {
      xpos = v + barWidth * (stacked ? k : s);
      values[v] = [xpos, series[s][v]];
    }
    data[s] = {
      data: values,
      stack: (stacked ? stacks[s] : null)
    };
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
      return percent ? value.toFixed(0) + '%' : value.toFixed(1);
    }
  };

  var chart = new Object();
  chart.id = id;
  chart.data = data;
  chart.options = options;

  chart.plot = function() {
    var plot = jQuery.plot(jQuery('#' + id), data, options);
    showBarDataLabels(plot, stacks, dataLabels, barWidth, 15);
    showBarDataLabels(plot, stacks, dataLabels2, barWidth, -5);
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

function showBarDataLabels(plot, stacks, dataLabels, barWidth, yOffset) {

  if (dataLabels.length == 0) {
    return;
  }

  var series = plot.getData();
  var stacked = stacks.length > 0;
  var offset = plot.pointOffset({x: 0, y: 0});
  var floor = offset.top;
  var extraOffset = createBiArray(20, 20, yOffset);

  for (var d = 0; d < series.length; d++) {

    if (dataLabels[d] != undefined) { 
        jQuery.each(series[d].data,

        function(i, point) {

          //Coordinates
          var x = point[0];

          if (x >= 0) {
            var y = point[1];

            //Only positive values are shown
            if (y <= 0) {
              return;
            }

            //Stack index
            var s = stacked ? stacks[d] : d;

            //x-axis data point index
            var p = Math.round(x - barWidth * s);

            offset = plot.pointOffset({x: x, y: y});
            var barHeight = floor - offset.top;

            //Subtact previous bars' offsets
            offset.top -= extraOffset[s][p];

            //If the data labels are too close
            if (barHeight < 15) {
              offset.top += barHeight - 15;
            }

            if (stacked) {
              extraOffset[s][p] += barHeight;
            }

            showDataLabel(plot, offset, barWidth, dataLabels[d][p]);
          }
        }
      );
    }
  }
}

function showDataLabel(plot, offset, barWidth, value) {

  if (value != null) {

    if (typeof value == 'number') {
      //Format value
      var precision = value < 1000 ? 2 : 0;
      value = value.toFixed(precision);
      if (value == 0) {
        value += '...';
      }
    }

    var labelDivClass = getStyleBySelector('p.chart-label');
    var fontSize = parseInt(labelDivClass.fontSize.replace('px', ''));
    if (barWidth > 0.3) {
      fontSize += 2;
    }

    var div = '<div style="font-size: ' + fontSize + 'px; width: 50px; height: 15px; text-align: center;">' + value + '</div>';

    var options = {
      position: 'absolute',
      left: offset.left - 25,
      top: offset.top,
      display: 'none'
    };

    jQuery(div).css(options).appendTo(plot.getPlaceholder()).fadeIn('slow');
  }
}

function createBiArray(size1, size2, value) {

  var arr = new Array(size1);
  for (var i = 0; i < size1; i++) {
    arr[i] = new Array(size2);
    for (var j = 0; j < size2; j++) {
      arr[i][j] = value;
    }
  }
  return arr;
}

function setSeriesColor(chartId, i, color) {

  color = '#' + color;

  if (barChart) {
    barChart.options['colors'][i] = color;
    barChart.plot();
  }
  if (lineChart) {
    updateDygraphColors();
  }

  jQuery.get('/logger/setvariable/series_color_' + chartId + '_' + i + '/' + escape(color));

  return true;
}

function syncColorPickers(i, color) {

  var buttons = document.getElementsByClassName("color");

  for (var b = 0; b < buttons.length; b++) {
    if (buttons[b].className.indexOf('series_color' + i) > 0) {
      buttons[b].style.background = '#' + color;
      buttons[b].style.color = 'transparent';
      buttons[b].value = color;
    }
  }
  return true;
}

function updateDygraphColors() {

  var colorValues = lineChart.getColors();
  var series = lineChart.visibility();
  for (var c = 0; c < series.length; c++) {
    var field = document.getElementById('series_color' + c);
    if (field) {
      colorValues[c] = '#' + field.value;
    }
  }

  lineChart.updateOptions({colors: colorValues});
  if (sliderChart) {
    sliderChart.updateOptions({colors: colorValues});
  }
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
