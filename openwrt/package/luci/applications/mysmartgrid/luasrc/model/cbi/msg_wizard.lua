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

f = SimpleForm("msg_wizard", translate("wizard"), translate("m_n_wizard"))

interface = f:field(ListValue, "interface", "Interface")
interface.widget = "select"
interface:value("wlan", "WLAN")
interface:value("lan", "LAN")

function interface.write(self, section, value)
	luci.http.redirect(value)
end

return f
