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

local uci = require "luci.model.uci".cursor()

m = Map("network", translate("network"), translate("m_n_network"))

s = m:section(NamedSection, "lan", "interface", translate("m_n_local"))
s.addremove = false

dhcp = s:option(Flag, "proto", "DHCP", "Automatically set network settings")
dhcp.enabled = "dhcp"
dhcp.disabled = "static"
dhcp.rmempty = false

ip = s:option(Value, "ipaddr", translate("ipaddress"))
ip:depends("proto", "")

function ip:validate(value)
	if uci:get("network", "wan", "ipaddr") == value then
		return nil
	end
    return value:match("[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+") -- Returns nil if it doesn't match otherwise returns match
end

nm = s:option(Value, "netmask", translate("netmask"))
nm:depends("proto", "")
nm:value("255.255.255.0")
nm:value("255.255.0.0")
nm:value("255.0.0.0")

gw = s:option(Value, "gateway", translate("gateway") .. translate("cbi_optional"))
gw:depends("proto", "")
gw.rmempty = true

dns = s:option(Value, "dns", translate("dnsserver") .. translate("cbi_optional"))
dns:depends("proto", "")
dns.rmempty = true

-- Tell tryit.lua that the wireless interface isn't used
uci:set("network", "wan", "proto", "none")
uci:save("network")
uci:commit("network")

return m
