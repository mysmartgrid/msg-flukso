<?php
  $id = isset($id)? $id : '';
  $widget = "  <widget_instance id=\"$id\">
    <name>Energie Graph</name>
    <widget>
      <name>Energie Graph</name>
      <description>Zeigt den Verbrauch der letzten Stunde als Graph an.</description>
      <version>1.0</version>
      <mode time=\"30\" mode=\"timeout\" />
      <access sendable=\"false\" deleteable=\"false\" access=\"private\" virtualable=\"false\" />
      <user username=\"\" />
      <thumbnail href=\"https://www.mysmartgrid.de/sites/all/themes/mysmartgrid/logo.png\" contenttype=\"image/png\" />
      <movie href=\"file:////mnt/storage/usr/widgets/last_reading/EnergyGraph.swf\" contenttype=\"application/x-shockwave-flash\" />
    </widget>
    <access access=\"private\" />
    <mode time=\"30\" mode=\"timeout\" />
    <widget_parameters>
    </widget_parameters>
  </widget_instance>
";
?>
