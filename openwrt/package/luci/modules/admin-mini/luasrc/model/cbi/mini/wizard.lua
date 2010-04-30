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

m = Map("flukso", translate("wizard"), translate("m_n_wizard"))

s = m:section(NamedSection, "network", "internal", translate("m_n_interface"))
s.addremove = false
i = s:option(ListValue, "interface", translate("interface"))
i.override_values = true
i.widget = "radio"
i:value("eth0", translate("wired network"))
i:value("wlan0", translate("wireless network"))

return m
