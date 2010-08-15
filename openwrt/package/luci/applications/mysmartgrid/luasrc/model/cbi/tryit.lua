--[[
LuCI - Lua Configuration Interface

Copyright 2008 Steven Barth <steven@midlink.org>
Copyright 2008 Jo-Philipp Wich <xm@leipzig.freifunk.net>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

	http://www.apache.org/licenses/LICENSE-2.0

$Id: network.lua 4171 2009-01-27 20:50:28Z Cyrus $
]]--

f = SimpleForm("tryit", "Test Connection", "Trying to connect to the server of the MySmartgrid project")

uci = luci.model.uci.cursor()

-- Get wireless interface
wifidevs={}
uci:foreach("wireless", "wifi-device",
	function(section)
		table.insert(wifidevs, section[".name"])
	end)

if uci:get("network", "wan", "proto") == "none" then
	uci:set("wireless", wifidevs[1], "disabled", 1) -- Disable wireless interface
else
	uci:delete("wireless", wifidevs[1], "disabled") -- Activate wireless interface
end
uci:save("wireless")
uci:commit("wireless")
uci:save("network")
uci:commit("network")
uci:apply({"wireless", "network"}) -- Apply changes

function heartbeat()
  require 'posix'
  require 'xmlrpc.http'
  require 'luci.sys'

  auth = require('flukso.auth')
  dbg  = require('flukso.dbg')

  -- config parameters
  local param = {server        = 'dev2-logger.mysmartgrid.de',
                 xmlrpcaddress = 'http://dev2-logger.mysmartgrid.de/xmlrpc',
                 xmlrpcversion = '1',
                 xmlrpcmethod  = 'logger.heartbeat'}

  local monitor = {reset = tonumber(arg[1])}

  -- open the connection to the syslog deamon, specifying our identity
  posix.openlog('heartbeat')

  -- calculate hmac and collect relevant monitoring points
  local auth = auth.new()
  auth:load()
  monitor.version = tonumber(auth.version)

  monitor.uptime  = math.floor(luci.sys.uptime())
  monitor.uart_oe = string.match(luci.sys.exec('cat /proc/tty/driver/serial'), 'oe:(%d+)') or 0
  system, model, monitor.memtotal, monitor.memcached, monitor.membuffers, monitor.memfree = luci.sys.sysinfo()

  auth:hmac(monitor)

  dbg.vardump(auth)
  dbg.vardump(monitor)

  -- send a heartbeat method call
  local pcall_ok, return_or_err, pong = pcall(xmlrpc.http.call,
    param.xmlrpcaddress..'/'..param.xmlrpcversion,
    param.xmlrpcmethod,
    auth,
    monitor)

	return pcall_ok, return_or_err, pong
end

res, err, pong = heartbeat()

result = f:field(DummyValue, "", "")
function result.cfgvalue(self, section)
	if res then
		return "Verbindungsaufbau erfolgreich"
	elseif err:find("host not found") then
		return "Konnte keine Verbindung zum mySmartGrid Server herstellen. "
	else
		return "Fehler aufgetreten: " .. err
	end
end

return f
