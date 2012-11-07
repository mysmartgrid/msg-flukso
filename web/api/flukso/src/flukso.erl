%%
%% Fukso module specification.
%%
%% Copyright (c) 2008-2010 flukso.net
%%               2011 Fraunhofer Institut ITWM (www.itwm.fraunhofer.de)
%%
%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version 2
%% of the License, or (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%%
%% You should have received a copy of the GNU General Public License
%% along with this program; if not, write to the Free Software
%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
%%

-module(flukso).
-author('Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>').

-export([start/0, start_link/0, stop/0]).


ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.


mysql_prepare() ->
    mysql:prepare(watchdog, <<"INSERT INTO watchdog (uid, type, message, variables, severity, location, hostname, timestamp) VALUES (?, ?, ?, ?, ?, ?, ?, ?)">>),
    mysql:prepare(permissions, <<"SELECT permissions FROM logger_tokens WHERE meter = ? AND token = ?">>),
    mysql:prepare(sensor_key, <<"SELECT sha FROM (logger_devices ld INNER JOIN logger_meters lm ON ld.device = lm.device) WHERE lm.meter = ?">>),
    mysql:prepare(sensor_props, <<"SELECT uid, device FROM logger_meters WHERE meter = ?">>),
    mysql:prepare(sensor_update, <<"UPDATE logger_meters SET access = ?, value = ? WHERE meter = ?">>),
    %TODO: mysql:prepare(sensor_config, <<"UPDATE logger_meters SET class = ?, type = ?, function = ?, voltage = ?, current = ?, phase = ?, constant = ?, enabled = ? WHERE meter = ?">>),
    mysql:prepare(sensor_config, <<"UPDATE logger_meters SET function = ? WHERE meter = ?">>),
    mysql:prepare(sensor_agg, <<"SELECT meter FROM logger_aggregated_meters WHERE virtual_meter = ?">>),
    mysql:prepare(timezone, <<"SELECT timezone FROM users WHERE uid = ?">>),
    mysql:prepare(device_key, <<"SELECT sha FROM logger_devices WHERE device = ?">>),
    mysql:prepare(device_props, <<"SELECT sha, upgrade, resets, firmware_version FROM logger_devices WHERE device = ?">>),
    mysql:prepare(device_update, <<"UPDATE logger_devices SET access = ?, version = ?, upgrade = ?, resets = ?, uptime = ?, memtotal = ?, memfree = ?, memcached = ?, membuffers = ?, sha = ?, firmware_version = ? WHERE device = ?">>),
    mysql:prepare(device_upgrade_update, <<"UPDATE logger_devices SET upgrade = ? WHERE device = ?">>),
    mysql:prepare(event_insert, <<"INSERT INTO event_log (device, event_id, time) VALUES (?, ?, ?)">>),
    mysql:prepare(device_insert, <<"INSERT INTO logger_devices (device, serial, uid, sha, created, access, version, firmware_version, upgrade, resets, uptime, memtotal, memfree, memcached, membuffers, uart_oe, sensor, country) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)">>),
    mysql:prepare(sensor_insert, <<"INSERT INTO logger_meters (meter, uid, device, created, access, type, function, phase, constant, value, factor, unit, price, latitude, longitude) SELECT ?, uid, device, ?, ?, ?, ?, ?, ?, ?, ?, ?, 0.18, 49.444710, 7.769031 FROM logger_devices WHERE device = ?">>),
    mysql:prepare(token_insert, <<"INSERT INTO logger_tokens (token, meter, permissions) VALUES (?, ?, ?)">>),
    mysql:prepare(support_slot, <<"SELECT username, host, port, tunnel_port FROM device_support_slot WHERE device = ?">>).


%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    flukso_deps:ensure(),
    ensure_started(crypto),
    ensure_started(erlrrd),
    ensure_started(mysql),
    mysql_prepare(),
    ensure_started(webmachine),
    flukso_sup:start_link().


%% @spec start() -> ok
%% @doc Start the flukso server.
start() ->
    flukso_deps:ensure(),
    ensure_started(crypto),
    ensure_started(erlrrd),
    ensure_started(mysql),
    mysql_prepare(),
    ensure_started(webmachine),
    application:start(flukso).


%% @spec stop() -> ok
%% @doc Stop the flukso server.
stop() -> 
    Res = application:stop(flukso),
    application:stop(webmachine),
    application:stop(mysql),
    application:stop(erlrrd),
    application:stop(crypto),
    Res.
