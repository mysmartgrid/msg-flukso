<?php
  $id = isset($id)? $id : '';
  $widget = "  <widget_instance id=\"$id\">
    <name>Gluehlampe</name>
    <widget>
      <name>Glühlampe</name>
      <description>Zeigt eine Glühlampe, die heller leuchtet, wenn mehr Strom verbraucht wird.</description>
      <version>1.0</version>
      <mode time=\"30\" mode=\"timeout\" />
      <access sendable=\"false\" deleteable=\"false\" access=\"private\" virtualable=\"false\" />
      <user username=\"\" />
      <thumbnail href=\"https://www.mysmartgrid.de/sites/all/themes/mysmartgrid/logo.png\" contenttype=\"image/png\" />
      <movie href=\"file:////mnt/storage/usr/widgets/last_reading/Gluehlampe.swf\" contenttype=\"application/x-shockwave-flash\" />
    </widget>
    <access access=\"private\" />
    <mode time=\"30\" mode=\"timeout\" />
    <widget_parameters>
    </widget_parameters>
  </widget_instance>
";
?>
