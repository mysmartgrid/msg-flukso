/**
 * Javascript functions used by module logger to build maps.
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

var map, markers, icon, marker;

function createMap(latitude, longitude) {

  map = new OpenLayers.Map('map');

  var basicLayer = new OpenLayers.Layer.WMS( "OpenLayers WMS", "http://vmap0.tiles.osgeo.org/wms/vmap0?", {layers: 'basic'} );
  map.addLayers([basicLayer]);

  markers = new OpenLayers.Layer.Markers( "Markers" );
  map.addLayer(markers);

  var iconSize = new OpenLayers.Size(21, 25);
  var iconOffset = function(size) {
    return new OpenLayers.Pixel(-(iconSize.w/2), -iconSize.h);
  };
  icon = new OpenLayers.Icon('/sites/all/modules/logger/js/openlayers/img/marker-green.png', iconSize, null, iconOffset);

  marker = new OpenLayers.Marker(new OpenLayers.LonLat(longitude, latitude), icon);
  markers.addMarker(marker);

  var click = createClickControl();
  map.addControl(click);
  map.addControl(new OpenLayers.Control.Navigation({'zoomWheelEnabled': true}));

  map.zoomToExtent(new OpenLayers.Bounds(9.0, 47.0, 14.0, 57.0));

  click.activate();
}

function createClickControl() {
  OpenLayers.Control.Click = OpenLayers.Class(OpenLayers.Control, {
    defaultHandlerOptions: {
      'single': true,
      'double': false,
      'pixelTolerance': 0,
      'stopSingle': false,
      'stopDouble': false
    },

    initialize: function(options) {
      this.handlerOptions = OpenLayers.Util.extend(
        {}, this.defaultHandlerOptions
      );
      OpenLayers.Control.prototype.initialize.apply(this, arguments);
      this.handler = new OpenLayers.Handler.Click(
        this, {
          'click': this.trigger
        }, this.handlerOptions
      );
    },

    trigger: function(e) {
      var coordinates = map.getLonLatFromViewPortPx(e.xy);
      var longitude = document.getElementById('edit-coordinates_longitude');
      var latitude = document.getElementById('edit-coordinates_latitude');

      longitude.value = coordinates.lon.toFixed(6);
      latitude.value = coordinates.lat.toFixed(6);

      markers.removeMarker(marker);
      marker = new OpenLayers.Marker(new OpenLayers.LonLat(longitude.value, latitude.value), icon);
      markers.addMarker(marker);
    }
  });
  return new OpenLayers.Control.Click();
}
