<?php
  $widget = "
    <widget_instance id=\"$widget->id\">
      <name>$widget->name</name>
      <widget id=\"$widget->id\">
        <name>$widget->name</name>
        <description>$widget->description</description>
        <version>1.0</version>
        <mode mode=\"default\" time=\"30\"/>
        <access sendable=\"false\" deleteable=\"false\" access=\"private\" virtualable=\"false\"/>
        <user username=\"\"/>
        <thumbnail href=\"file:////mnt/storage/usr/widgets/$widget->directory/$widget->movie.png\" contenttype=\"image/png\"/>
        <movie href=\"file:////mnt/storage/usr/widgets/$widget->directory/$widget->movie.swf\" contenttype=\"application/x-shockwave-flash\"/>
      </widget>
      <access access=\"private\"/>
      <mode mode=\"default\" time=\"30\"/>
    </widget_instance>";
?>
