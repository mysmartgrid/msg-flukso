/**
 * Javascript functions used by module logger to build statistics plots.
 *
 * Copyright (c) 2014 Fraunhofer Institut ITWM (www.itwm.fraunhofer.de)
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


function createConsumptionIndicator(consumption,xticklabels,xaxislabel,yaxislabel,consMark) {

  var data0 = [ [ consumption, 0.4] ]; //consumers yearly energy consumption
  var xmax = (consumption < 5000 ? 6500 : consumption + 2000) // setting final range of x-axis

  var consRanges = [ [900, 1500, 2100], [1600, 2600, 3700], [2200, 2900, 4200], [2600, 3400, 4900], [3100, 4000, 5900] ];

  var data1 = [ [consRanges[0][0], 0.7], [consRanges[1][0], 1.7], [consRanges[2][0], 2.7], [consRanges[3][0], 3.7], [consRanges[4][0], 4.7] ]; // 1st range delimiter
  var data2 = [ [consRanges[0][1]-consRanges[0][0], 0.7], [consRanges[1][1]-consRanges[1][0], 1.7], [consRanges[2][1]-consRanges[2][0], 2.7], [consRanges[3][1]-consRanges[3][0], 3.7], [consRanges[4][1]-consRanges[4][0], 4.7] ];
  var data3 = [ [consRanges[0][2]-consRanges[0][1], 0.7], [consRanges[1][2]-consRanges[1][1], 1.7], [consRanges[2][2]-consRanges[2][1], 2.7], [consRanges[3][2]-consRanges[3][1], 3.7], [consRanges[4][2]-consRanges[4][1], 4.7] ];
  var data4 = [ [xmax-consRanges[0][2], 0.7], [xmax-consRanges[1][2], 1.7], [xmax-consRanges[2][2], 2.7], [xmax-consRanges[3][2], 3.7], [xmax-consRanges[4][2], 4.7] ];

  var dataset = [
      { data: data0, color: "#555", stack: false, bars: {show: false}, points: {show: true, symbol: "diamond", radius: 5} }, //FhG color: "#009374"
      { data: data1, color: "#33DD00" },    { data: data2, color: "#EEFF00" },
      { data: data3, color: "#FF9900" },    { data: data4, color: "#FF0000" }
  ];
  var ticks = [ [1, xticklabels[0]], [2, xticklabels[1]], [3, xticklabels[2]], [4, xticklabels[3]],
                [5, xticklabels[4]] ];

  var markings = [{color:"#000", lineWidth:2, xaxis: { from: consumption, to: consumption } }];

  var options = {
    series: {
        stack: true,
        bars: { show: true }
    },
    bars: {
        lineWidth: 1,
        barWidth: 0.6,
        fillColor: { colors: [{ opacity: 0.4}, { opacity: 1}] },
        horizontal: true
    },
    xaxis: {
        axisLabel: xaxislabel,
        tickColor: "gray",
        color: "#3B3131",
        axisLabelFontSizePixels: 12,
        font: {size: 12},
        axisLabelPadding: 10
         },
    yaxis: {
        axisLabel: yaxislabel,
        axisLabelFontSizePixels: 12,
        axisLabelPadding: 10,
        tickLength: 0,
        min: 0.2,
        max: 5.5,
        color: "#3B3131",
        font: {size: 12},
        ticks: ticks,
        transform: function (v) { return -v; },
        inverseTransform: function (v) { return -v; }
        },
    grid: {
        hoverable: true,
	markings: markings,
        borderWidth: 0
    }
  };
  var plot = jQuery.plot(jQuery("#flot-placeholder2"), dataset, options);

  // add labels
  var o = plot.pointOffset({ x: data0[0][0], y: data0[0][1]});

  // we just append it to the placeholder which Flot already uses for positioning
  jQuery("#flot-placeholder2").append('<div style="position:absolute;left:' + (o.left + 10) + 'px;top:' + (o.top - 9) +
                                      'px;color:#555;font-size:100%"><b>' + consMark + '</b></div>')

}
