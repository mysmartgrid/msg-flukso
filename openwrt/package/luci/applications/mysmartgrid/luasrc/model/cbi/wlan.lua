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

m = Map("wireless", translate("wifi"), translate("a_w_devices1"))
m:chain("network")

s = m:section(TypedSection, "wifi-iface", "")
s.anonymous = true
s.addremove = false

s:option(Value, "ssid", translate("a_w_netid"))

encr = s:option(ListValue, "encryption", translate("encryption"))
encr.override_values = true
encr:value("none", "No Encryption")
encr:value("wep", "WEP")
encr:value("psk", "WPA-PSK")
encr:value("psk2", "WPA2-PSK")

key = s:option(Value, "key", translate("key"))
key:depends("encryption", "wep")
key:depends("encryption", "psk")
key:depends("encryption", "psk2")
key.rmempty = true
key.description = translate('wifi_keyreq')

function key:validate(value, section)
        function string.tohex(x)
                local hex = ''
                for c in x:gmatch('(.)') do hex = hex .. string.format("%02x", c:byte()) end
                return hex
        end

        function string.hexcheck(x)
                return #(x:match('%x*')) == #x and x
        end

        if encr:formvalue(section) == 'wep' then
                if #value == 5 or #value == 13 then
                        return value:tohex()
                elseif #value == 10 or #value == 26 then
                        return value:hexcheck()
                else
                        return nil
                end
        elseif encr:formvalue(section) == 'psk' or encr:formvalue(section) == 'psk2' then
                return #value > 7 and #value < 64 and value
        else
                return value
        end
end

n = Map("network", translate("network"), translate("m_n_network"))

w = n:section(NamedSection, "wan", "interface", translate("m_n_local"))
w.addremove = false

dhcp = w:option(Flag, "proto", "DHCP", "Automatically set network settings")
dhcp.enabled = "dhcp"
dhcp.disabled = "static"
dhcp.rmempty = false

ip = w:option(Value, "ipaddr", translate("ipaddress"))
ip:depends("proto", "")

function ip:validate(value)
	if uci:get("network", "lan", "ipaddr") == value then
		return nil
	end
    return value:match("[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+") -- Returns nil if it doesn't match otherwise returns match
end

nm = w:option(Value, "netmask", translate("netmask"))
nm:depends("proto", "")
nm:value("255.255.255.0", "255.255.255.0")
nm:value("255.255.0.0")
nm:value("255.0.0.0")
nm.default = "255.255.255.0"

gw = w:option(Value, "gateway", translate("gateway") .. translate("cbi_optional"))
gw:depends("proto", "")
gw.rmempty = true

dns = w:option(Value, "dns", translate("dnsserver") .. translate("cbi_optional"))
dns:depends("proto", "")
dns.rmempty = true

return m, n
