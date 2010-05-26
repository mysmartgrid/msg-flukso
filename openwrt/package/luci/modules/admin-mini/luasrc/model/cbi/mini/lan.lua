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

f = SimpleForm("lan", "Local Area Network", "Configure the Flukso to use wired lan to connect to the internet")

dhcp = f:field(Flag, "DHCP", "DHCP", "Automaticaly set network settings")
dhcp.default = 1

ipaddr = f:field(Value, "ipaddr", "IP-Address")
ipaddr:depends("DHCP", "")
ipaddr.defaults = "..."
function ipaddr:validate(value)
	if dhcp.enabled then
    return value:match("[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+") -- Returns nil if it doesn't match otherwise returns match
	else
		return false
	end
end

netmask = f:field(Value, "netmask", "Netmask")
netmask:depends("DHCP", "")
netmask.defaults = "255.255.255.0"
function netmask:validate(value)
    return value:match("[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+") -- Returns nil if it doesn't match otherwise returns match
end

gateway = f:field(Value, "gw", "Standard-Gateway")
gateway:depends("DHCP", "")
function gateway:validate(value)
    return value:match("[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+") -- Returns nil if it doesn't match otherwise returns match
end

dns1 = f:field(Value, "dns1", "Primary DNS-Server")
dns1:depends("DHCP", "")
function dns1:validate(value)
    return value:match("[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+") -- Returns nil if it doesn't match otherwise returns match
end

dns2 = f:field(Value, "dns2", "Secondary DNS-Server")
dns2:depends("DHCP", "")
dns2.optional = true
function dns2:validate(value)
    return value:match("[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+") -- Returns nil if it doesn't match otherwise returns match
end

function dhcp.write(self, section, value)
	luci.http.redirect("tryit")
end

return f
