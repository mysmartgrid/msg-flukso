<<<<<<< Updated upstream
function showSensorForecastFields(){var a=document.getElementById("edit-type-id");var c=document.getElementById("forecast-options");if(c){if(a.value==2){c.style.display=""}else{c.style.display="none";var b=document.getElementById("edit-forecast-1");b.checked=false}}}function permissionSensorForecastFields(b){var a=alert(b);var c=document.getElementById("edit-forecast-1");if(c){c.checked=false}return 0}function isMobile(){return document.body.clientWidth<600}function resizeTable(b,e,d){if(isMobile()){var i=document.getElementById(b);if(i){var h=document.body.clientWidth>400;var f=h?"table-cell":"none";e=e?e:3;d=d?d:i.rows[0].cells.length-1;for(var a=0;a<i.rows.length;a++){var j=i.rows[a].cells;for(var g=e;g<d;g++){j[g].style.display=f}}}}}function toggleDeviceAssignFields(){var a=document.getElementById("logger-deviceassign-form");var b=document.getElementById("new-user-fields-div");b.style.display=(a.new_account[0].checked?"none":"");b=document.getElementById("user-fields-div");b.style.display=(a.new_account[1].checked?"none":"")}function toggleFormDiv(e,c,b){var d=jQuery("form#"+e+" #"+c);var a=jQuery("form#"+e+" div#"+b);if(d.is(":checkbox")){if(d.is(":checked")){a.slideDown()}else{a.slideUp()}}else{if(d.val()=="dhcp"||d.val()=="open"){a.slideUp()}else{a.slideDown()}}}function cleanHiddenFields(a){jQuery("form#"+a+" .field-group :hidden").val("")}function updateConfigApplied(a){jQuery("form#"+a+" #edit-config-applied").fadeOut()};
=======
function showSensorForecastFields(){var e=document.getElementById("edit-type-id"),n=document.getElementById("forecast-options");if(n)if(2==e.value)n.style.display="";else{n.style.display="none";var t=document.getElementById("edit-forecast-1");t.checked=!1}}function permissionSensorForecastFields(e){var n=(alert(e),document.getElementById("edit-forecast-1"));return n&&(n.checked=!1),0}function isMobile(){return document.body.clientWidth<600}function resizeTable(e,n,t){if(isMobile()){var d=document.getElementById(e);if(d){var i=document.body.clientWidth>400,o=i?"table-cell":"none";n=n?n:3,t=t?t:d.rows[0].cells.length-1;for(var l=0;l<d.rows.length;l++)for(var c=d.rows[l].cells,s=n;t>s;s++)c[s].style.display=o}}}function toggleDeviceAssignFields(){var e=document.getElementById("logger-deviceassign-form"),n=document.getElementById("new-user-fields-div");n.style.display=e.new_account[0].checked?"none":"",n=document.getElementById("user-fields-div"),n.style.display=e.new_account[1].checked?"none":""}function toggleFormDiv(e,n,t){var d=jQuery("form#"+e+" #"+n),i=jQuery("form#"+e+" div#"+t);d.is(":checkbox")?d.is(":checked")?i.slideDown():i.slideUp():"dhcp"==d.val()||"open"==d.val()?i.slideUp():i.slideDown()}function cleanHiddenFields(e){jQuery("form#"+e+" .field-group :hidden").val("")}function updateConfigApplied(e){jQuery("form#"+e+" #edit-config-applied").fadeOut()}
>>>>>>> Stashed changes
