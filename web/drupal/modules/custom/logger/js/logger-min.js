function showSensorForecastFields(){var a=document.getElementById("edit-type-id"),b=document.getElementById("forecast-options");b&&(2==a.value?b.style.display="":(b.style.display="none",document.getElementById("edit-forecast-1").checked=!0))}function isMobile(){return 600>document.body.clientWidth} function resizeTable(a,b,c){if(isMobile()&&(a=document.getElementById(a))){var d=400<document.body.clientWidth?"table-cell":"none";b=b?b:3;c=c?c:a.rows[0].cells.length-1;for(var e=0;e<a.rows.length;e++)for(var g=a.rows[e].cells,f=b;f<c;f++)g[f].style.display=d}} function toggleDeviceAssignFields(){var a=document.getElementById("logger-deviceassign-form"),b=document.getElementById("new-user-fields-div");b.style.display=a.new_account[0].checked?"none":"";b=document.getElementById("user-fields-div");b.style.display=a.new_account[1].checked?"none":""}function showDeviceLANDiv(a){a=document.getElementById(a);document.getElementById("lan-fields-div").style.display=a.lan_enabled.checked?"":"none"} function showDeviceWIFIDiv(a){a=document.getElementById(a);document.getElementById("wifi-fields-div").style.display=a.wifi_enabled.checked?"":"none"}function showSensorDiv(a,b){document.getElementById(a);var c=document.getElementById("sensor"+b+"-fields-div"),d=document.getElementById("edit-enabled"+b);c.style.display=d.checked?"":"none"};
function resizeTable(a,b,c){if(isMobile()&&(a=document.getElementById(a))){var f=400<document.body.clientWidth?"table-cell":"none";b=b?b:3;c=c?c:a.rows[0].cells.length-1;for(var d=0;d<a.rows.length;d++)for(var g=a.rows[d].cells,e=b;e<c;e++)g[e].style.display=f}}
function toggleDeviceAssignFields(){var a=document.getElementById("logger-deviceassign-form"),b=document.getElementById("new-user-fields-div");b.style.display=a.new_account[0].checked?"none":"";b=document.getElementById("user-fields-div");b.style.display=a.new_account[1].checked?"none":""}function toggleFormDiv(a,b,c){b=jQuery("form#"+a+" #"+b);a=jQuery("form#"+a+" div#"+c);b.is(":checkbox")?b.is(":checked")?a.slideDown():a.slideUp():"dhcp"==b.val()||"open"==b.val()?a.slideUp():a.slideDown()}
function cleanHiddenFields(a){jQuery("form#"+a+" .field-group :hidden").val("")}function updateConfigApplied(a){jQuery("form#"+a+" #edit-config-applied").fadeOut()};
