/**
 * Javascript functions used by module logger.
 *
 * Copyright (c) 2010-2015 Fraunhofer Institut ITWM (www.itwm.fraunhofer.de)
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

function showSensorForecastFields() {

  var sensorTypeField = document.getElementById('edit-type-id');
  var forecastDiv = document.getElementById('forecast-options');

  //If page contains forecast options div
  if (forecastDiv) {

    //If production, show forecast options div
    if (sensorTypeField.value == 2) {
      forecastDiv.style.display = '';

    } else {
      forecastDiv.style.display = 'none';
      var forecastField = document.getElementById('edit-forecast-1');
      forecastField.checked = false;
    }
  }
}

function permissionSensorForecastFields(message) {
  var r = alert(message);
  var forecastField = document.getElementById('edit-forecast-1');

  //If page contains forecast options div
  if (forecastField) {
      forecastField.checked = false;
  }
  return 0;
}

function isMobile() {
  //If client is a mobile device
  return document.body.clientWidth < 600;
}

function resizeTable(id, col1, col2) {

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

function toggleDeviceAssignFields() {

  var form = document.getElementById('logger-deviceassign-form');

  var div = document.getElementById('new-user-fields-div');
  div.style.display = (form.new_account[0].checked ? 'none' : '');

  div = document.getElementById('user-fields-div');
  div.style.display = (form.new_account[1].checked ? 'none' : '');
}

function toggleFormDiv(formId,inputId,divId) {
  var inputElement=jQuery("form#"+formId+" #"+inputId);
  var divElement=jQuery("form#"+formId+" div#"+divId);
  if ( inputElement.is(":checkbox") ) {
    if ( inputElement.is(":checked") ) {
      divElement.slideDown()
    } else {
      divElement.slideUp()
    }
  } else {
    if ( inputElement.val() == "dhcp"
           ||inputElement.val()=="open" ) {
      divElement.slideUp()
    } else {
      divElement.slideDown()
    }
  }
}

function cleanHiddenFields(formId) {
  jQuery('form#' + formId + ' .field-group :hidden').val("");
}

function updateConfigApplied(formId) {
  jQuery('form#' + formId + ' #edit-config-applied').fadeOut();
}
