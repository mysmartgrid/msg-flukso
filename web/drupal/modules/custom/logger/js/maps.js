/**
 * Javascript functions used by module logger to build maps.
 * It relies on OpenLayers (http://dev.openlayers.org).
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

var map;

function createMap(latitude, longitude) {

  var coordinates = new OpenLayers.LonLat(longitude, latitude);
  var click = createClickControl();

  var markers = new OpenLayers.Layer.Markers("Markers");
  markers.addMarker(createMarker(coordinates));

  map = new OpenLayers.Map('map', {

    layers: [
      new OpenLayers.Layer.OSM(),
      markers
    ],

    controls: [
      new OpenLayers.Control.Navigation({'zoomWheelEnabled': true}),
      new OpenLayers.Control.PanZoomBar(),
      click
    ],

    zoom: 15,
    center: coordinates.transform(
      new OpenLayers.Projection("EPSG:4326"),
      new OpenLayers.Projection("EPSG:900913")
    )
  });

  click.activate();
}

function createMarker(coordinates) {

  var iconSize = new OpenLayers.Size(21, 25);
  var iconOffset = function(size) {
    return new OpenLayers.Pixel(-(iconSize.w/2), -iconSize.h);
  };
  var icon = new OpenLayers.Icon('/sites/all/modules/logger/js/openlayers/img/marker-green.png', iconSize, null, iconOffset);

  return new OpenLayers.Marker(coordinates, icon);
}

function createClickControl() {

  OpenLayers.Control.Click = OpenLayers.Class(OpenLayers.Control, {

    initialize: function(options) {
      this.handlerOptions = OpenLayers.Util.extend({}, this.defaultHandlerOptions);
      OpenLayers.Control.prototype.initialize.apply(this, arguments);
      this.handler = new OpenLayers.Handler.Click(
        this, {'click': this.trigger}
      );
    },

    trigger: function(e) {

      var coordinates = map.getLonLatFromViewPortPx(e.xy);

      var markers = map.getLayersByName('Markers')[0];
      markers.clearMarkers();
      markers.addMarker(createMarker(coordinates));

      coordinates.transform(map.getProjectionObject(), new OpenLayers.Projection("EPSG:4326"));
      updateFormFields(coordinates);
    }
  });

  return new OpenLayers.Control.Click();
}

function updateFormFields(coordinates) {

  var longitudeField = document.getElementById('edit-coordinates_longitude');
  var latitudeField = document.getElementById('edit-coordinates_latitude');

  longitudeField.value = coordinates.lon.toFixed(8);
  latitudeField.value = coordinates.lat.toFixed(8);
}
