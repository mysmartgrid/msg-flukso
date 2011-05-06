# Copyright (c) 2008 jokamajo.org
# $Id$

include $(TOPDIR)/rules.mk

PKG_NAME:=msg-flukso
PKG_VERSION:=103
PKG_RELEASE:=1

PKG_BUILD_DIR:=$(BUILD_DIR)/$(PKG_NAME)-$(PKG_VERSION)
PKG_INSTALL_DIR:=$(PKG_BUILD_DIR)/ipkg-install

include $(INCLUDE_DIR)/package.mk

define Package/msg-flukso
  SECTION:=utils
  CATEGORY:=Utilities
  DEPENDS:=+luaxmlrpc +luaxyssl +ntpclient +avahi-daemon
  TITLE:=mySmartGrid - die intelligente Gerätesteuerung für Ihr Zuhause.
  URL:=http://mysmartgrid.de/
endef

define Package/msg-flukso/Description
  This package contains the necessary openwrt files for the Flukso, customized \\\
  for the mySmartGrid project.
endef

define Build/Prepare
	mkdir -p $(PKG_BUILD_DIR)
	$(CP) src/* $(PKG_BUILD_DIR)/
endef

define Build/Compile
endef

define Package/msg-flukso/install
	$(INSTALL_DIR) $(1)/usr/share/lua/flukso/
	$(CP) $(PKG_BUILD_DIR)/data.lua $(1)/usr/share/lua/flukso/
	$(CP) $(PKG_BUILD_DIR)/dbg.lua $(1)/usr/share/lua/flukso/
	$(CP) $(PKG_BUILD_DIR)/auth.lua $(1)/usr/share/lua/flukso/
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/flukso.lua $(1)/usr/share/lua/flukso/
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/heartbeat.lua $(1)/usr/share/lua/flukso/
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/load.lua $(1)/usr/share/lua/flukso/
	$(INSTALL_DIR) $(1)/etc/init.d/
	$(CP) $(PKG_BUILD_DIR)/flukso.init $(1)/etc/init.d/flukso
	$(INSTALL_DIR) $(1)/etc/avahi/services/
	$(CP) $(PKG_BUILD_DIR)/tools/flukso.service $(1)/etc/avahi/services/
	$(INSTALL_DIR) $(1)/bin/
	$(INSTALL_BIN) $(PKG_BUILD_DIR)/tools/net_defaults $(1)/bin/
endef

define Package/msg-flukso/postinst
  #!/bin/sh

  #create symlink for local lan interface
  test -L /www || ln -s /tmp/sensor /www

	uci set system.system.version = '103'
	uci commit system
endef

define Package/msg-flukso/prerm
  #!/bin/sh

  #remove symlink for local lan interface
	rm /www
endef

$(eval $(call BuildPackage,msg-flukso))
