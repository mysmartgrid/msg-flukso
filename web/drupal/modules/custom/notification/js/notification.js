/**
 * Javascript functions used by module notification.
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

function selectEvent(eventId, defaultLimitUp, defaultUnitId, field) {

  var limitUpField = document.getElementById('edit-limit-up' + eventId);
  var unitIdField  = document.getElementById('edit-unit-id'  + eventId);

  if (limitUpField) {
    limitUpField.readOnly = !field.checked;
    limitUpField.value = field.checked ? defaultLimitUp : '';
  }
  
  if (unitIdField) {
    unitIdField.value = defaultUnitId;
  }
}

function changeLimitUp(eventId, field) {
  var eventIdField = document.getElementById('edit-event-id' + eventId);
  field.readOnly = !eventIdField.checked;
}