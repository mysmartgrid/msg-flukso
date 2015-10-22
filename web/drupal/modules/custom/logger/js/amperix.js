/*
 * JavaScript for Amperix - configuration - wizard
 */
/*
 * Initialisierung der Seite
 */

(function() {
  console.log('Init?');
  form1 = $( "#logger-deviceconfig2-form" );

  $(function() {
      currentTab="network";
      $.fn.bootstrapSwitch.defaults.state = false;
      $.fn.bootstrapSwitch.defaults.size = 'large';
      $.fn.bootstrapSwitch.defaults.onColor = 'success';
      $.fn.bootstrapSwitch.defaults.offColor = 'danger';

      //form.validate( );


    $('input[name="my-checkbox"]').on('switchChange.bootstrapSwitch', function(event, state) {
      console.log(this); // DOM element
      console.log(event); // jQuery event
      console.log(state); // true | false
    });

    });

 
}).call(this);


jQuery.validator.addMethod("integer", function(value, element) {
			       return this.optional(element) || /^([0-9])+$/.test(value);
			   }, "Must eine Zahl sein");

jQuery.validator.addMethod("string", function(value, element) {
			       return this.optional(element) || /^([a-zA-Z_0-9])+$/.test(value);
			   }, "Ungueltiges Zeichen: erlaubt: a-zA-Z0-9_");
jQuery.validator.addMethod("ipaddress", function(value, element) {
			       return this.optional(element) || /^([0-9])+.([0-9])+.([0-9])+.([0-9])+$/.test(value);
			   }, "Ungueltiges Zeichen: erlaubt: 0-9.");
jQuery.validator.addMethod("require_from_group", function(value, element, params) {
			       console.log("p0:" + params[0]);
			       console.log("p1:" + params[1]);
			       return this.optional(element) || 1;
			   });


/* switch to next tab */
function switchtab(tabname) {
	var name= "#cfgTabs a[href=\"#" + tabname + "\"]";
	console.log("Switch to: "+name);
	$(name).tab('show');
	currentTab = tabname;
}

/* 
 *
 */
function toggleNetwork(field1, field2, field3, field4, state) {
  var thisid = field1.attr('id');

  if(state) {
    field1.prop('hidden', false).change();
    field1.prop('required', true).change();

    field2.prop('hidden', true).change();
    field2.prop('required', false).change();

    field3.prop('checked', false).change();
	} else {
    field1.prop('hidden', true).change();
    field1.prop('required', true).change();
    field2.prop('required', true).change();
		
    field3.prop('checked', true).change();
  }
  console.log("my id is: "+thisid);
  if( thisid == 'nw-wlan') {
    console.log("WLAN");
    
    $('#nw-wlan-ssid').rules("add", { required: true , string: true, messages: { required: "Bitte SSID eingeben.",} } );
    $('#nw-wlan-ssid').closest('.form-group').removeClass('has-success').addClass('has-error');
    $('#nw-wlan-ssid1').removeClass('glyphicon-ok').addClass('glyphicon-remove');  
    $('#nw-wlan-key').rules("add", { required: true , string: true, messages: { required: "Bitte WiFi Schl&uuml;ssel eingeben.",} } );
    $('#nw-wlan-key').closest('.form-group').removeClass('has-success').addClass('has-error');
    $('#nw-wlan-key1').removeClass('glyphicon-ok').addClass('glyphicon-remove');  

    $('#nw-wlan-ssid').on('click', function() {  $('#logger-deviceconfig2-form').valid(); });
    $('#nw-wlan-ssid').on('change', function() {  $('#logger-deviceconfig2-form').valid(); });
    $('#nw-wlan-key').on('click', function() {  $('#logger-deviceconfig2-form').valid(); });
    $('#nw-wlan-key').on('change', function() {  $('#logger-deviceconfig2-form').valid(); });

  } else {
    $('#nw-wlan-ssid').rules("remove", "required" );
    $('#nw-wlan-key').rules("remove", "required" );
  }
  if (thisid == 'nw-lan') {
    console.log("LAN");
  //} else {
  }
}

function toggleHidden(field1, field2, state) {
  if(state) {
    field1.prop('hidden', false).change();
    nwip = $('#'+field2+'-ip');
    nwip.rules("add", { required: true, ipaddress: true, messages: { required: "Bitte SSID eingeben.",} } );
    $('#'+field2+'-ip').closest('.form-group').removeClass('has-success').addClass('has-error');
    $('#'+field2+'-ip1').removeClass('glyphicon-ok').addClass('glyphicon-remove');  

    $('#'+field2+'-gw').rules("add", { required: true, ipaddress: true, messages: { required: "Bitte SSID eingeben.",} } );
    $('#'+field2+'-gw').closest('.form-group').removeClass('has-success').addClass('has-error');
    $('#'+field2+'-gw1').removeClass('glyphicon-ok').addClass('glyphicon-remove');  

    $('#'+field2+'-mask').rules("add", { required: true, ipaddress: true, messages: { required: "Bitte SSID eingeben.",} } );
    $('#'+field2+'-mask').closest('.form-group').removeClass('has-success').addClass('has-error');
    $('#'+field2+'-mask1').removeClass('glyphicon-ok').addClass('glyphicon-remove');  

    $('#'+field2+'-ns').rules("add", { required: true, ipaddress: true, messages: { required: "Bitte SSID eingeben.",} } );
    $('#'+field2+'-ns').closest('.form-group').removeClass('has-success').addClass('has-error');
    $('#'+field2+'-ns1').removeClass('glyphicon-ok').addClass('glyphicon-remove');  
  } else {
    field1.prop('hidden', true).change()
  }
}

/*
 * Create the view for dynamic/static network configuration
 */
function createNetworkConfigHTML(w) {
  var r = "";
  r = r + '            <div id="' + w + 'Form" class="row">';
  r = r + '              <div class="col-sm-5">Netzadresse:</div>';
  r = r + '              <div class="col-sm-7">';
  r = r + '                  <select class="form-input" id="' + w + '-dhcp" name="' + w + '_protocol">';
  r = r + '                    <option value="dhcp" >Dynamisch (DHCP)</option>';
  r = r + '                    <option value="static">Statisch</option>'; 
  r = r + '                  </select>';
  r = r + '              </div>';
  r = r + '            </div>';

  r = r + '            <div id="nwbox-' + w + '" hidden >';
  r = r + '              <div class="row">';
  r = r + '                <div class="col-sm-5">IP-Adresse:</div>';
  r = r + '                <div class="col-sm-7">';
  r = r + '        <div class="form-group has-feedback">';
  r = r + '                  <input type="text" class="form-control" placeholder="192.168.178.20" aria-describedby="basic-addon2"';
  r = r + '                         name="' + w + '_ip" id="'+w+'-ip" />';
  r = r + '        <span class="glyphicon form-control-feedback" id="'+w+'-ip1"></span>';
  r = r + '        </div>';
  r = r + '                </div>';
  r = r + '              </div>';
  r = r + '              <div class="row">';
  r = r + '                <div class="col-sm-5">Gateway:</div>';
  r = r + '                <div class="col-sm-7">';
  r = r + '        <div class="form-group has-feedback">';
  r = r + '                  <input type="text" class="form-control" placeholder="192.168.178.1" aria-describedby="basic-addon2"';
  r = r + '                         name="' + w + '_gw" id="'+w+'-gw" />';
  r = r + '        <span class="glyphicon form-control-feedback" id="'+w+'-gw1"></span>';
  r = r + '        </div>';
  r = r + '                </div>';
  r = r + '              </div>';
  r = r + '              <div class="row">';
  r = r + '                <div class="col-sm-5">Netmask:</div>';
  r = r + '                <div class="col-sm-7">';
  r = r + '        <div class="form-group has-feedback">';
  r = r + '                  <input type="text" class="form-control" placeholder="255.255.255.0" aria-describedby="basic-addon2"';
  r = r + '                         name="' + w + '_mask" id="'+w+'-mask"/>';
  r = r + '        <span class="glyphicon form-control-feedback" id="'+w+'-mask1"></span>';
  r = r + '        </div>';
  r = r + '                </div>';
  r = r + '              </div>';
  r = r + '              <div class="row">';
  r = r + '                <div class="col-sm-5">Nameserver:</div>';
  r = r + '                <div class="col-sm-7">';
  r = r + '        <div class="form-group has-feedback">';
  r = r + '                  <input type="text" class="form-control" placeholder="192.168.178.1" aria-describedby="basic-addon2"';
  r = r + '                         name="' + w + '_ns" id="'+w+'-ns"/>';
  r = r + '        <span class="glyphicon form-control-feedback" id="'+w+'-ns1"></span>';
  r = r + '        </div>';
  r = r + '                </div>';
  r = r + '              </div>';
  r = r + '            </div>';
  return r;
}

function createNetworkConfig(w) {
  var checkbox = "#" + w + "-dhcp";
  var inputbox = "#nwbox-"+w;
  $(checkbox).on('change', function() {
    var selected = $(checkbox+' option:selected').val();
      if(selected == 'static') {
        toggleHidden($(inputbox), w, 1);
      } else {
        toggleHidden($(inputbox), w, 0);
      }
    });
    var selected = $(checkbox+' option:selected').val();
    if(selected == 'static') {
      toggleHidden($(inputbox), w, 1);
    } else {
      toggleHidden($(inputbox), w, 0);
    }
}


/*
 * toggle the sensor configuration
 */
function toggleSensor(field1, field2, state) {
  var id_attr1 = "#" + field1.attr("id") + "1";
  var id_attr2 = "#" + field2.attr("id") + "1";
  if(state) {
      field1.prop('disabled', false).change();
      field1.prop('required', false).change();
      field2.prop('disabled', false).change();
      field2.prop('required', false).change();
      field1.rules("add", { required: true, string: true, messages: { required: "Bitte den Sensornamen eingeben.",} } );
    field1.closest('.form-group').removeClass('has-success').addClass('has-error');
    $(id_attr1).removeClass('glyphicon-ok').addClass('glyphicon-remove');  
    field2.closest('.form-group').removeClass('has-success').addClass('has-error');
    $(id_attr2).removeClass('glyphicon-ok').addClass('glyphicon-remove');  

    field2.rules("add", { required: true, digits: true,
			 messages: { required: "Bitte Anzahl der Impulse je kWh eingeben.",
			     digits: "Bitte eine Zahl eingeben." }});

  } else {
    field1.prop('disabled', true).change()
    field1.prop('required', true).change()
    field2.prop('disabled', true).change()
    field2.prop('required', true).change()
    field1.rules("remove", "required" );
    field1.closest('.form-group').removeClass('has-error'); //.addClass('has-error');
    $(id_attr1).removeClass('glyphicon-remove'); //.addClass('glyphicon-remove');  
    field2.closest('.form-group').removeClass('has-error'); //.addClass('has-error');
    $(id_attr2).removeClass('glyphicon-remove'); //.addClass('glyphicon-remove');  
    field2.rules("remove", "required");
  }

}

/*
 * configuration Hall-Sensors
 */
function createHallSensorHTML(no,y,z) {
//  var r = "test";
  var r = '    <div id="sensor'+no+'Form" class="row">';
  r = r + '      <div class="col-sm-1">';
  r = r + '        <div class="form-group">';
  r = r + '          <div class="input-group">';
  r = r + '            <input type="hidden" id="sensor'+no+'_uuid" name="sensorenUUID[' + no + ']" value="">';
  r = r + '            <input type="checkbox" id="sensor'+no+'_checkbox" name="sensorenCheckbox[]" value="' + no + '" ';
  r = r + ' data-validation-required-message="S" ';
	r = r + ' data-size="mini" class="sensor-group">';
  r = r + '          </div>';
  r = r + '        </div>';
  r = r + '      </div>';
  r = r + '      <div class="col-sm-2"><center>Sensor ' + no + '</center></div>';
  r = r + '      <div class="col-sm-6">';
  r = r + '        <div class="form-group has-feedback">';
  r = r + '<input type="text" class="form-control" placeholder="Sensorname" aria-describedby="basic-addon2"';
  r = r + ' name="sensorname[' + no + ']" id="sensor' + no + '-name" disabled';
  r = r + ' data-validation-required-message="Bitte einen g&uuml;ltigen Name angeben." />';
  r = r + '        <span class="glyphicon form-control-feedback" id="sensor' + no + '-name1"></span>';
  r = r + '        </div>';
  r = r + '      </div>';
  r = r + '      <div class="col-sm-2">';
  r = r + '        <div class="form-group">';
//<!-- Ampere: 50, 100, 250, 500 -->
  r = r + '          <select class="form-control" id="sensor' + no + '-extras" name="sensorXtra[' + no + ']">';
  r = r + '            <option>50</option>';
  r = r + '            <option>100</option>';
  r = r + '            <option>250</option>';
  r = r + '            <option>500</option>';
  r = r + '          </select>';
  r = r + '        </div>';
  r = r + '      </div>';
  r = r + '      <div class="col-sm-1">A</div>';
  r = r + '    </div>';
  return r;
}

function createHallSensor(no,y,z) {
  $.fn.bootstrapSwitch.defaults.onColor = 'success';
  $.fn.bootstrapSwitch.defaults.offColor = 'danger';
  var name="[name='sensor"+no+"_checkbox']";
  $('#sensor'+no+'_checkbox').bootstrapSwitch('state', false);
  var input="input"+name;

  $('#sensor'+no+'-name').prop('disabled', true).change()
  $('#sensor'+no+'-extras').prop('disabled', true).change()

  $('#sensor'+no+'_checkbox').on('switchChange.bootstrapSwitch', function(event, state) {
    toggleSensor($('#sensor'+no+'-name'), $('#sensor'+no+'-extras'), state);
  });
  $('#sensor'+no+'-name').on('click', function() {  $('#logger-deviceconfig2-form').valid(); });
  $('#sensor'+no+'-name').on('change', function() {  $('#logger-deviceconfig2-form').valid(); });

	$('#sensor' + no + 'Form').click(function(event){$('#sensor'+no+'_checkbox').bootstrapSwitch('state', true);toggleSensor($('#sensor'+no+'-name'), $('#sensor'+no+'-extras'),true);});
}

/*
 * configuration S0-Sensors
 */
function createS0SensorHTML(no,y,z) {
//  var r = "test";
  var r = '    <div id="sensor'+no+'Form" class="row">';
  r = r + '      <div class="col-sm-1">';
  r = r + '        <div class="form-group">';
  r = r + '          <div class="input-group">';
  r = r + '            <input type="hidden" id="sensor'+no+'_uuid" name="sensorenUUID[' + no + ']" value="">';
  r = r + '            <input type="checkbox" id="sensor'+no+'_checkbox" name="sensorenCheckbox[]" value="' + no + '" data-size="mini" class="sensor-group">';
  r = r + '          </div>';
  r = r + '        </div>';
  r = r + '      </div>';

  r = r + '      <div class="col-sm-2"><center>Sensor ' + no + '</center></div>';
  r = r + '      <div class="col-sm-4">';
  r = r + '        <div class="form-group has-feedback">';
  r = r + '<input type="text" class="form-control" placeholder="Sensorname" aria-describedby="basic-addon2"';
  r = r + ' name="sensorname[' + no + ']" id="sensor' + no + '-name" disabled';
  r = r + ' data-bv-notempty-message="Bitte einen gueltigen Namen angeben." />';
  r = r + '        <span class="glyphicon form-control-feedback" id="sensor' + no + '-name1"></span>';
  r = r + '        </div>';
  r = r + '      </div>';

  r = r + '      <label class="col-sm-3 control-label">Impulse je kWh </label>';
  r = r + '      <div class="col-sm-2">';
  r = r + '        <div class="form-group has-success has-feedback">';
  r = r + '<input type="text" class="form-control" placeholder="1000" aria-describedby="basic-addon2"';
  r = r + ' name="sensorXtra[' + no + ']" id="sensor' + no + '-extras" disabled';
  r = r + ' data-rule-message="Bitte geben Sie eine Zahl ein."';
  r = r + ' data-bv-notempty-message="Bitte geben Sie eine Zahl ein."';
  r = r + ' data-bv-regexp-message="Wert miuss eine Zahl sein.">';
  r = r + '        <span class="glyphicon form-control-feedback" id="sensor' + no + '-extras1"></span>';
  r = r + '        </div>';
  r = r + '      </div> ';
  r = r + '    </div> ';
  return r;
}

function createS0Sensor(no,y,z) {
  $.fn.bootstrapSwitch.defaults.onColor = 'success';
  $.fn.bootstrapSwitch.defaults.offColor = 'danger';
  var name="[name='sensor"+no+"_checkbox']";
  $('#sensor'+no+'_checkbox').bootstrapSwitch('state', false);
  var input="input"+name;
  $('#sensor'+no+'-name').prop('disabled', true).change()
  $('#sensor'+no+'-extras').prop('disabled', true).change()

  $('#sensor'+no+'_checkbox').on('switchChange.bootstrapSwitch', function(event, state) {
    toggleSensor($('#sensor'+no+'-name'), $('#sensor'+no+'-extras'), state);
  });

  $('#sensor'+no+'-name').on('click', function() {  $('#logger-deviceconfig2-form').valid(); });
  $('#sensor'+no+'-name').on('change', function() {  $('#logger-deviceconfig2-form').valid(); });
  $('#sensor'+no+'-extras').on('click', function() {  $('#logger-deviceconfig2-form').valid(); });
  $('#sensor'+no+'-extras').on('change', function() {  $('#logger-deviceconfig2-form').valid(); });
}


function nextPage(cPage, nPage) {
	console.log("Current: "+cPage);
	console.log("Next   : "+nPage);
  var form1 = $( "#logger-deviceconfig2-form" );
  form1.validate({
    debug: true
	});
  //var id=currentTab;

  if( form1.valid() ) {
    //var tab = $(this).attr('data-tab-destination');
    if( nPage == 'synchronisation' ) {
      console.log(' Submit1...: '+cPage);
      //form1.submit();
      console.log(' Submit2...: '+cPage);
      document.devicewizard.submit();
      console.log(' Submit3...: '+cPage);
    } else {
      console.log(' Next tab');
			if( nPage == 'configuration' ) {
				if($('#nw_lan_checkbox').is(":checked")) { 
					$('#cfg-lan').html("LAN");
					$('#cfg-wifi').prop("hidden",true).change();
				} else if($('#nw_wlan_checkbox').is(":checked")) { 
					$('#cfg-lan').html("WiFi");
					$('#cfg-wifi').prop("hidden",false).change();
					$('#cfg-wlan-ssid').html($('#nw-wlan-ssid').val());
					$('#cfg-wlan-crypto').html($('#nw-wlan-crypto').val());
					$('#cfg-wlan-key').html($('#nw-wlan-key').val());
				} else { $('#cfg-lan').html("unknown"); }

				if($('#lan-dhcp').val() == "dhcp") {
					$('#cfg-protocol').html("DHCP");
					$('#cfg-static').prop("hidden", true).change();
				} else {
					$('#cfg-static').prop("hidden", false).change();
					$('#cfg-protocol').html("Static");
					$('#cfg-lan-ip').html($('#lan-ip').val());
					$('#cfg-lan-gw').html($('#lan-gw').val());
					$('#cfg-lan-mask').html($('#lan-mask').val());
					$('#cfg-lan-ns').html($('#lan-ns').val());
				}
			}
			if($('#sensor1_checkbox').is(':checked')) {
				$('#cfg-sensor1').removeClass('glyphicon-remove').addClass('glyphicon-ok');  
				//$('#cfg-sensor1').html("aktiviert");
				$('#cfg-sensor1-name').html($('#sensor1-name').val());
				$('#cfg-sensor1-extras').html($('#sensor1-extras').val() + ' A');
			} else {
				$('#cfg-sensor1').removeClass('glyphicon-ok').addClass('glyphicon-remove');  
				$('#cfg-sensor1-name').html('');
				//$('#cfg-sensor1').html("deaktiviert");
				$('#cfg-sensor1-extras').html('');
			}
			if($('#sensor2_checkbox').is(':checked')) {
				$('#cfg-sensor2').removeClass('glyphicon-remove').addClass('glyphicon-ok');  
				$('#cfg-sensor2-name').html($('#sensor2-name').val());
				$('#cfg-sensor2-extras').html($('#sensor2-extras').val() + ' A');
			} else {
				$('#cfg-sensor2').removeClass('glyphicon-ok').addClass('glyphicon-remove');  
				$('#cfg-sensor2-name').html('');
				$('#cfg-sensor2-extras').html('');
			}
			if($('#sensor3_checkbox').is(':checked')) {
				$('#cfg-sensor3').removeClass('glyphicon-remove').addClass('glyphicon-ok');  
				$('#cfg-sensor3-name').html($('#sensor3-name').val());
				$('#cfg-sensor3-extras').html($('#sensor3-extras').val() + ' A');
			} else {
				$('#cfg-sensor3').removeClass('glyphicon-ok').addClass('glyphicon-remove');  
				$('#cfg-sensor3-name').html('');
				$('#cfg-sensor3-extras').html('');
			}
			if($('#sensor4_checkbox').is(':checked')) {
				$('#cfg-sensor4').removeClass('glyphicon-remove').addClass('glyphicon-ok');  
				$('#cfg-sensor4-name').html($('#sensor4-name').val());
				$('#cfg-sensor4-extras').html($('#sensor4-extras').val() + ' Impuls/kWh');
			} else {
				$('#cfg-sensor4').removeClass('glyphicon-ok').addClass('glyphicon-remove');  
				$('#cfg-sensor4-name').html('');
				$('#cfg-sensor4-extras').html('');
			}
			if($('#sensor5_checkbox').is(':checked')) {
				$('#cfg-sensor5').removeClass('glyphicon-remove').addClass('glyphicon-ok');  
				$('#cfg-sensor5-name').html($('#sensor5-name').val());
				$('#cfg-sensor5-extras').html($('#sensor5-extras').val() + ' Impuls/kWh');
			} else {
				$('#cfg-sensor5').removeClass('glyphicon-ok').addClass('glyphicon-remove');
				$('#cfg-sensor5-name').html('');
				$('#cfg-sensor5-extras').html('');
			}
			switchtab(nPage);
    }
  } else {
    console.log(' Nicht valid');
    if( cPage == 'network' ) {
      alert( "Bitte beenden Sie zunaechst die Netzwerkkonfiguration." );
    } else {
      if( cPage == 'sensoren' ) {
				alert( "Es muss mindestens 1 Sensor aktiviert werden." );
      } else {
				alert( "Die Eingabe ist noch fehlerhaft bzw unvollstaedig." );
			}
		}
		return false;
  }
}

function fillForm() {
  var form1 = $( "#logger-deviceconfig2-form" );
  //var form1 = document.getElementById("logger-deviceconfig2-form2")
  console.log('Fillform ' + form1.attr('id'));
  form1.attr('name', 'devicewizard');
  form1.attr('role', 'form');
  form1.attr('class', 'form-horizontal');
  form1.attr('data-bv-message',"This value is not valid");
  form1.attr('data-bv-feedbackicons-valid',"glyphicon glyphicon-ok");
  form1.attr('data-bv-feedbackicons-invalid',"glyphicon glyphicon-remove");
  form1.attr('data-bv-feedbackicons-validating',"glyphicon glyphicon-refresh");
  form1.attr('data-bv-message',"Dieser Wert ist ungueltig.");

  var txt = '<ul class="nav nav-tabs nav-justified" id="cfgTabs" role="tablist">';
  txt = txt + '<li role="presentation" class="active"><a href="#network" id="network-tab" role="tab" data-toggle="tab" aria-controls="network">Netzwerk</a></li>';
  txt = txt + '<li role="presentation"><a  href="#sensoren" id="sensoren-tab" role="tab" data-toggle="tab" aria-controls="sensoren">Sensoren</a></li>';
  txt = txt + '<li role="presentation"><a  href="#configuration" id="configuration-tab" role="tab" data-toggle="tab" aria-controls="configuration">Konfiguration</a></li>';
  txt = txt + '<li role="presentation"><a  href="#synchronisation" id="synchronisation-tab" role="tab" data-toggle="tab" aria-controls="synchronisation">Synchronisieren</a></li>';
  txt = txt + '</ul>';

  txt = txt + '<div class="tab-content">';
  txt = txt + '<div role="tabpanel" class="tab-pane fade in active" id="network" aria-labelledBy="network-tab">';
  txt = txt + '<div class="panel panel-default">';

  // panel network
  txt = txt + '<div class="panel-heading"><center>Netzwerkkonfiguration</center></div>';
  txt = txt + '      <div class="panel-body" id="nw" >';
  txt = txt + '        <div class="row">';
  txt = txt + '          <div class="col-sm-2 form-group">';
  txt = txt + '                <div class="input-group">';
  txt = txt + '                  <input type="checkbox" id="nw_lan_checkbox" name="nw_enabled[]" value="1" ';
  txt = txt + '                         data-size="mini"';
  txt = txt + '                         class="network-group">';
  txt = txt + '                </div>';
  txt = txt + '          </div>';
  txt = txt + '          <div class="col-sm-1 form-group">LAN';
  txt = txt + '          </div>';
  txt = txt + '          <div class="col-sm-2 form-group"></div>';
  txt = txt + '          <div class="col-sm-2">';
  txt = txt + '                <div class="input-group">';
  txt = txt + '                  <input type="checkbox" id="nw_wlan_checkbox" name="nw_enabled[]" value="2" ';
  txt = txt + '                         data-size="mini"';
  txt = txt + '                         class="network-group">';
  txt = txt + '                </div>';
  txt = txt + '          </div>';
  txt = txt + '          <div class="col-sm-1 form-group">WiFi';
  txt = txt + '          </div>';
  txt = txt + '          <div class="col-sm-2 form-group"></div>';
  txt = txt + '        </div>';

  txt = txt + '        <div id="nw-wlan" hidden>'; // WiFi-config
  txt = txt + '                <div id="wlanForm" class="row">';
  txt = txt + '                  <div class="col-sm-5">SSID:</div>';
  txt = txt + '                  <div class="col-sm-7">';
  txt = txt + '                    <div class="form-group has-feedback">';
  txt = txt + '                    <input type="text" class="form-control" placeholder="SSID" aria-describedby="basic-addon2"';
  txt = txt + '                           name="nwWlanSSID" id="nw-wlan-ssid" />';
  txt = txt + '                    <span class="glyphicon form-control-feedback" id="nw-wlan-ssid1"></span>';
  txt = txt + '                  </div>';
  txt = txt + '                  </div>';
  txt = txt + '                </div>';
  txt = txt + '                <div id="wlanForm" class="row">';
  txt = txt + '                  <div class="col-sm-5">verschluessel:</div>';
  txt = txt + '                  <div class="col-sm-7">';
  txt = txt + '                    <div class="form-group has-feedback">';
  txt = txt + '                      <select class="form-control" id="nw-wlan-crypto" name="nw-wlan-crypto">';
  txt = txt + '                        <option value="open">keine</option>';
  txt = txt + '                        <option value="wep">WEP</option>';
  txt = txt + '                        <option value="wpa">WPA (TKIP)</option>';
  txt = txt + '                        <option value="wpa2">WPA2 (CCMP)</option>';
  txt = txt + '                      </select>';
  txt = txt + '                  </div>';
  txt = txt + '                  </div>';
  txt = txt + '                </div>';
  txt = txt + '                <div class="row">';
  txt = txt + '                  <div class="col-sm-5">Schluessel/Passwort:</div>';
  txt = txt + '                  <div class="col-sm-7">';
  txt = txt + '                    <div class="form-group has-feedback">';
  txt = txt + '                    <input type="text" class="form-control" placeholder="Passwort oder Schluessel" aria-describedby="basic-addon2"';
  txt = txt + '                           name="nw-wlan-key" id="nw-wlan-key"';
  txt = txt + '                           data-bv-notempty-message="The password/PSK is required and cannot be empty" />';
  txt = txt + '                    <span class="glyphicon form-control-feedback" id="nw-wlan-key1"></span>';
  txt = txt + '                  </div>';
  txt = txt + '                  </div>';
  txt = txt + '                </div>';
  txt = txt + '        </div>';
  txt = txt + createNetworkConfigHTML('lan');

  txt = txt + '        <!-- end row network -->';
  txt = txt + '        <div class="row">';
  txt = txt + '          <div class="col-lg-12" align="right">';
  txt = txt + '          <button type="button" class="btn btn-default" data-tab-destination="sensoren" aria-label="Left Align" id="btn-next-sensoren" onclick="nextPage(\'network\', \'sensoren\');">';
  txt = txt + '          <span class="glyphicon glyphicon-chevron-right" aria-hidden="true"></span>';
  txt = txt + '          </button>';
  txt = txt + '          </div>';
  txt = txt + '        </div>';
  txt = txt + '      </div>';

  txt = txt + '</div>';
  txt = txt + '</div>';

  // tab 2
  txt = txt + '<div role="tabpanel" class="tab-pane" id="sensoren" aria-labelledBy="sensoren-tab">';
  txt = txt + '<div class="panel panel-default">';
  txt = txt + '<div class="panel-heading"><center>Hallsensoren</center></div>';
  txt = txt + '      <div class="panel-body">';
  txt = txt + createHallSensorHTML(1,2,3);
  txt = txt + createHallSensorHTML(2,2,3);
  txt = txt + createHallSensorHTML(3,2,3);
  txt = txt + '      </div>';
  txt = txt + '    </div>';

  txt = txt + '    <div class="panel panel-default">';
  txt = txt + '      <div class="panel-heading"><center>S0 Zaehler</center></div>';
  txt = txt + '      <div class="panel-body">';
  txt = txt + createS0SensorHTML(4,2,3);
  txt = txt + createS0SensorHTML(5,2,3);
  txt = txt + '      </div>';
  txt = txt + '    </div>';
  txt = txt + '    <div class="row">';
  txt = txt + '       <div class="col-lg-12" align="right">';
  txt = txt + '          <button type="button" class="btn btn-default" data-tab-destination="configuration" aria-label="Left Align" id="next-button" onclick="nextPage(\'sensoren\',\'configuration\');">';
  txt = txt + '          <span class="glyphicon glyphicon-chevron-right" aria-hidden="true"></span>';
  txt = txt + '          </button>';
  txt = txt + '       </div>';
  txt = txt + '   </div>';
  txt = txt + '</div>';

  // tab 3 - syntese
  txt = txt + '<div role="tabpanel" class="tab-pane" id="configuration">';
  txt = txt + '  <div class="panel panel-default">';
  txt = txt + '    <div class="panel-heading"><center>Konfiguration</center></div>';
  txt = txt + '    <div class="panel-body">';
  txt = txt + '      <div class="row">';
  txt = txt + '        <div class="col-sm-3">Anschluss:</div>';
  txt = txt + '        <div class="col-sm-6" id="cfg-lan"></div>';
  txt = txt + '      </div>';
  txt = txt + '      <div id="cfg-wifi"><div class="row">';
  txt = txt + '        <div class="col-sm-3">ESSID:</div>';
  txt = txt + '        <div class="col-sm-6" id="cfg-wlan-ssid"></div>';
  txt = txt + '      </div>';
  txt = txt + '      <div class="row">';
  txt = txt + '        <div class="col-sm-3">Verschl&uuml;sselung:</div>';
  txt = txt + '        <div class="col-sm-6" id="cfg-wlan-crypto"></div>';
  txt = txt + '      </div>';
  txt = txt + '      <div class="row">';
  txt = txt + '        <div class="col-sm-3">Passwort/psk:</div>';
  txt = txt + '        <div class="col-sm-6" id="cfg-wlan-key"></div>';
  txt = txt + '      </div></div>';
  txt = txt + '      <div class="row">';
  txt = txt + '        <div class="col-sm-3">Protocol:</div>';
  txt = txt + '        <div class="col-sm-6" id="cfg-protocol"></div>';
  txt = txt + '      </div>';

  txt = txt + '      <div id="cfg-static"><div class="row">';
  txt = txt + '        <div class="col-sm-3">IP Adresse:</div>';
  txt = txt + '        <div class="col-sm-6" id="cfg-lan-ip"></div>';
  txt = txt + '      </div>';
  txt = txt + '      <div class="row">';
  txt = txt + '        <div class="col-sm-3">Netzmaske:</div>';
  txt = txt + '        <div class="col-sm-6" id="cfg-lan-mask"></div>';
  txt = txt + '      </div>';
  txt = txt + '      <div class="row">';
  txt = txt + '        <div class="col-sm-3">Gateway:</div>';
  txt = txt + '        <div class="col-sm-6" id="cfg-lan-gw"></div>';
  txt = txt + '      </div>';
  txt = txt + '      <div class="row">';
  txt = txt + '        <div class="col-sm-3">Nameserver:</div>';
  txt = txt + '        <div class="col-sm-6" id="cfg-lan-ns"></div>';
  txt = txt + '      </div></div>';

  txt = txt + '      <div class="row">';
  txt = txt + '        <div class="col-sm-3">Sensor 1:</div>';
  txt = txt + '        <div class="col-sm-1 glyphicon" id="cfg-sensor1"></div>';
  txt = txt + '        <div class="col-sm-3" id="cfg-sensor1-name"></div>';
  txt = txt + '        <div class="col-sm-3" id="cfg-sensor1-extras"></div>';
  txt = txt + '      </div>';
  txt = txt + '      <div class="row">';
  txt = txt + '        <div class="col-sm-3">Sensor 2:</div>';
  txt = txt + '        <div class="col-sm-1 glyphicon" id="cfg-sensor2"></div>';
  txt = txt + '        <div class="col-sm-3" id="cfg-sensor2-name"></div>';
  txt = txt + '        <div class="col-sm-3" id="cfg-sensor2-extras">A</div>';
  txt = txt + '      </div>';
  txt = txt + '      <div class="row">';
  txt = txt + '        <div class="col-sm-3">Sensor 3:</div>';
  txt = txt + '        <div class="col-sm-1 glyphicon" id="cfg-sensor3"></div>';
  txt = txt + '        <div class="col-sm-3" id="cfg-sensor3-name"></div>';
  txt = txt + '        <div class="col-sm-3" id="cfg-sensor3-extras">A</div>';
  txt = txt + '      </div>';
  txt = txt + '      <div class="row">';
  txt = txt + '        <div class="col-sm-3">Sensor 4:</div>';
  txt = txt + '        <div class="col-sm-1 glyphicon" id="cfg-sensor4"></div>';
  txt = txt + '        <div class="col-sm-3" id="cfg-sensor4-name"></div>';
  txt = txt + '        <div class="col-sm-3" id="cfg-sensor4-extras">Impuls/kWh</div>';
  txt = txt + '      </div>';
  txt = txt + '      <div class="row">';
  txt = txt + '        <div class="col-sm-3">Sensor 5:</div>';
  txt = txt + '        <div class="col-sm-1 glyphicon" id="cfg-sensor5"></div>';
  txt = txt + '        <div class="col-sm-3" id="cfg-sensor5-name"></div>';
  txt = txt + '        <div class="col-sm-3" id="cfg-sensor5-extras">Impuls/kWh</div>';
  txt = txt + '      </div>';


  txt = txt + '    </div>';
  txt = txt + '  </div>';
  txt = txt + '  <div class="row">';
  txt = txt + '    <div class="col-lg-12" align="right">';
  txt = txt + '      <button type="button" class="btn btn-default" data-tab-destination="save" aria-label="Left Align" id="next-button" onclick="nextPage(\'configuration\',\'synchronisation\');">';
  txt = txt + '        <span class="glyphicon glyphicon-chevron-right" aria-hidden="true"></span>';
  txt = txt + '      </button>';
  txt = txt + '    </div>';
  txt = txt + '  </div>';
  txt = txt + '</div>';

  // tab 4 - synchronisation
  txt = txt + '<div role="tabpanel" class="tab-pane" id="synchronisation">';
  txt = txt + '  <div class="panel panel-default">';
  //txt = txt + '    <div class="panel-heading"><center>Synchronisation</center></div>';
  txt = txt + '    <div class="panel-body">';
	txt = txt + '       synchronisation progress bar';      
  txt = txt + '    </div>';
  txt = txt + '  </div>';
  txt = txt + '</div>';

  // end of tab-content
  txt = txt + '</div>';

  //var txt = txt1.concat(txt2, txt3, txt4, txt5);

  html = $.parseHTML( txt );
  //console.log('Fillform ' + txt1);

  //console.log('Fillform "' + txt +'"');
  form1.append(html);

  createNetworkConfig('lan');
  //createNetworkConfig('wlan');
  createHallSensor(1,2,3);
  createHallSensor(2,2,3);
  createHallSensor(3,2,3);
  createS0Sensor(4,2,3);
  createS0Sensor(5,2,3);

  $('#logger-deviceconfig2-form').validate({   
    debug: true,

	  ignore: ":hidden",
    rules: {
          sensor1_checkbox: {
		require_from_group: [1, ".sensor-group"]
          },
          sensor2_checkbox: {
		require_from_group: [1, ".sensor-group"]
          },
          sensor3_checkbox: {
		require_from_group: [1, ".sensor-group"]
          },
          sensor4_checkbox: {
		require_from_group: [1, ".sensor-group"]
          },
          sensor5_checkbox: {
		require_from_group: [1, ".sensor-group"]
          }
    },
    highlight: function(element) {
        var id_attr = "#" + $( element ).attr("id") + "1";
        $(element).closest('.form-group').removeClass('has-success').addClass('has-error');
        $(id_attr).removeClass('glyphicon-ok').addClass('glyphicon-remove');        
    },
    unhighlight: function(element) {
        var id_attr = "#" + $( element ).attr("id") + "1";
        $(element).closest('.form-group').removeClass('has-error').addClass('has-success');
        $(id_attr).removeClass('glyphicon-remove').addClass('glyphicon-ok');        
    },
    errorElement: 'span',
        errorClass: 'help-block',
        errorPlacement: function(error, element) {
            if(element.length) {
                error.insertAfter(element);
            } else {
            error.insertAfter(element);
            }
        } 
  });

	$('.network-group').rules("add", { required: true, string: true, messages: { required: ""}, } );
	$('.sensor-group').rules("add", { required: true, string: false, messages: { required: false}, } );
	//$("#logger-deviceconfig2-form").validate().settings.onsubmit = false;

    $("#nw_lan_checkbox").bootstrapSwitch('state', true);

    $("#nw_wlan_checkbox").bootstrapSwitch('state', false);

    $('#nw_lan_checkbox').on('switchChange.bootstrapSwitch', function(event, state) {
															 toggleNetwork($('#nw-lan'), $('#nw-wlan'), $('#nw_wlan_checkbox'), $('#nw_lan_checkbox'), state);
														 });
    $('#nw_wlan_checkbox').on('switchChange.bootstrapSwitch', function(event, state) {
																toggleNetwork($('#nw-wlan'), $('#nw-lan'), $('#nw_lan_checkbox'), $('#nw_wlan_checkbox'), state);
															});
  console.log('Fillform - done');

}


$(document).ready
(
 function() {
	 var form = $( "#logger-deviceconfig2-form" );
	 fillForm();
	 //form.validate();
	 updateValues();
   setSensorID();

	 $('a[data-toggle="tab"]').on('shown.bs.tab', function (e, data) {
																console.log("datatoggle...");
																currentTab=$(this).attr('aria-controls');
																console.log("datatoggle..."+ currentTab);
																})

	 $('#cfgTabs a').click
		 (function (e) {
			 e.preventDefault();
			 console.log('T switchtab validate - current: ' + currentTab);
			 console.log("T Attr: " + $(this).attr('aria-controls'));
			 destTab=$(this).attr('aria-controls');
			 return nextPage(currentTab, destTab);
		 })

		 console.log('==>Ready called.');
 });
