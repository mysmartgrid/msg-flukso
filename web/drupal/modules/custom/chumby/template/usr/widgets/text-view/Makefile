SWF_FILE=currentpower.swf
CHUMBY_PATH=/tmp
CHUMBY_USER=root
CHUMBY_IP=192.168.1.100

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
