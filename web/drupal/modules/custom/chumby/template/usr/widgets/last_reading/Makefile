SWF_FILE=energietacho.swf
#SWF_FILE=Gluehlampe.swf
#SWF_FILE=Gluehlampe_frames.swf
#SWF_FILE=EnergyGraph_default.swf
#SWF_FILE=EnergyGraph_log.swf
#SWF_FILE=EnergyGraph_section.swf
CHUMBY_PATH=/tmp
CHUMBY_USER=root

#Put your local Chumby IP here
CHUMBY_IP=192.168.21.209

default: help

copy: $(SWF_FILE)
	scp $< $(CHUMBY_USER)@$(CHUMBY_IP):$(CHUMBY_PATH)

run: $(SWF_FILE) copy stop
	ssh $(CHUMBY_USER)@$(CHUMBY_IP) "chumbyflashplayer.x -i $(CHUMBY_PATH)/$< 2>&1 &"

stop: 
	ssh $(CHUMBY_USER)@$(CHUMBY_IP) "/usr/chumby/scripts/stop_control_panel 2>&1"
	
restore: stop
	ssh $(CHUMBY_USER)@$(CHUMBY_IP) "/usr/chumby/scripts/start_control_panel 2>&1"

.SILENT:

help:
	echo "Copies and runs an SWF file on a local Chumby, please edit the Makefile."
	echo "Available commands:"
	echo " * copy: copies $(SWF_FILE) to the chumby at $(CHUMBY_IP)"
	echo " * run: executes $(SWF_FILE) on the chumby "
	echo " * stop: stops the currently active control panel"
	echo " * restore: loads the original control panel (BROKEN?)"
