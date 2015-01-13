= last_reading widgets

Widgets to display the last_reading the msg-chumby-daemon received from your Flukso. The boundary values for "low", "middle" and "high" are imported from the msg-chumby-daemon on widget startup. To change these values see the howto.

= energietacho 

The widget displays the power consumption in a speedometer like view. The needle is rotated based on the last_reading of your flukso and the provided boundary values. If the _high value is exceeded the needle "hangs" at "High" until lower values are received. However, the current consumption in watts is always displayed as digits.  

= Gluehlampe

The widget displays a lightbulb which glows (off, yellow, red) according to your power consumption and boundary values. 
This widget uses three MovieClips in one frame which, and exchanges/blends them based on the result of the DataArray.getMeasure method. 

Can be easily adapted by exchanging the three MovieClips with own designs.
 
= Gluehlampe_frames

Here, unlike in the "Gluehlampe" widget, three frames (with one MovieClip each) are used for the different states of power consumption. The shown frame is switched according to the result of DataArray.getMeasure again. 

Can be easily adapted by exchanging the three MovieClips with own designs.

= EnergyGraph_default (_log/_section)

The energy consumption of the last minute is show as a "bar"graph. The graph is updated automatically every second. The color is set according to the given boundaries (low/middle/high). 
There are three types of scaling: 
- default: The graph is linearly scaled based on the max value within the last minute.
- log: The graph is logscaled based on the max value within the last minute.
- section: The graph is divided into three equal horizontal sections (low, middle, high). The bars are scaled based on the section they belong to.  

In the .fla file the scaling can be chosen by setting "scaling" to 0=defaul, 1=log, 2=section.

= files

- Makefile
  Used to copy widgets to your chumby and start them. (IP needs to be adjusted!)
  
- energietacho.swf | .fla
  Widget(swf) and Flash(fla) file for the speedometer like widget.

- Gluehlampe.swf | .fla
  Widget(swf) and Flash(fla) file for the lightbulb widget using only one frame.

- Gluehlampe_frames.swf | .fla
  Widget(swf) and Flash(fla) file for the lightbulb widget using multiple frames.

- EnergyGraph_*.swf | EnergyGraph.fla
  Widgets(swf) and Flash(fla) file(s) for the EnergyGraph. 

- XMLParse.as
  AS2 file which handles the XML parsing. 

- DataArray.as
  AS2 file which handles the received data and provides different access methods.

- rectangle.as
  AS2 file for drawing a rectangle/bar with given attributes.

- caurina.*
  AS library for easy tweening, etc.

= howto
To simply use the widgets on your chumby with the default values, you can copy them as described in the mySmartGrid developer wiki. (Makefile is included)

To change the boundary values for the consumption you have to open your msgchumbyrc file and change the three values. These changes will only take effect after a restart of the msg-chumby-daemon with the new msgchumbyrc!

In the "Gluehlampe" and "Gluehlampe_frames" widgets, the three MovieClips can be exchanged with arbitrary other MovieClips. 
