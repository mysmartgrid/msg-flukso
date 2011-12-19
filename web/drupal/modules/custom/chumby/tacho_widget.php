<?php
  $widget = "  <widget_instance id=\"$id\">
    <name>Energietacho</name>
    <widget id=\"$id\">
      <name>Energietacho</name>
      <description>Zeigt den aktuellen Verbrauch als Tachonadel an.</description>
      <version>1.0</version>
      <mode mode=\"default\" time=\"30\"/>
      <access sendable=\"false\" deleteable=\"false\" access=\"private\" virtualable=\"false\"/>
      <user username=\"\"/>
      <thumbnail href=\"file:////mnt/storage/usr/widgets/logo.png\" contenttype=\"image/png\"/>
      <movie href=\"file:////mnt/storage/usr/widgets/last_reading/energietacho.swf\" contenttype=\"application/x-shockwave-flash\"/>
    </widget>
    <access access=\"private\"/>
    <mode mode=\"default\" time=\"30\"/>
  </widget_instance>
";
?>
