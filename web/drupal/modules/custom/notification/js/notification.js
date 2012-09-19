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

function selectEvent(checked, eventId, defaultLimitUp, defaultUnitId) {

  var limitUpField = document.getElementById('edit-limit-up' + eventId);
  var unitIdField  = document.getElementById('edit-unit-id'  + eventId);

  if (limitUpField) {
    limitUpField.readOnly = !checked;
    limitUpField.value = checked ? defaultLimitUp : '';
  }
  
  if (unitIdField) {
    unitIdField.value = defaultUnitId;
  }

  if (!checked) {
    var anyIssueField = getEventIdField(9);
    anyIssueField.checked = false;
  }
}

function changeLimitUp(eventId, field) {
  var eventIdField = getEventIdField(eventId);
  field.readOnly = !eventIdField.checked;
}

function checkDeviceIssueEvents(field) {

  checkDeviceIssueEvent(field.checked, 1, 2, 2);
  checkDeviceIssueEvent(field.checked, 3, 2, 2);
  checkDeviceIssueEvent(field.checked, 201);
  checkDeviceIssueEvent(field.checked, 200);
}

function checkDeviceIssueEvent(checked, eventId, defaultLimitUp, defaultUnitId) {

  var eventIdField = getEventIdField(eventId);
  eventIdField.checked = checked;

  selectEvent(checked, eventId, defaultLimitUp, defaultUnitId);
}

function getEventIdField(eventId) {
  return document.getElementById('edit-event-id' + eventId);
}

function showDeviceIssueEvents(show) {

  var issuesDiv = document.getElementById('issues');
  var showDetailsLink = document.getElementById('showDetails');
  var hideDetailsLink = document.getElementById('hideDetails');

  if (show) {
    issuesDiv.style.display = "";
    showDetailsLink.style.display = "none";
    hideDetailsLink.style.display = "";
  } else {
    issuesDiv.style.display = "none";
    showDetailsLink.style.display = "";
    hideDetailsLink.style.display = "none";
  }
}
