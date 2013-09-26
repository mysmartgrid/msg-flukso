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
    mysql:prepare(timezone, <<"SELECT timezone FROM users WHERE uid = ?">>),
    mysql:prepare(permissions, <<"SELECT permissions FROM logger_tokens WHERE meter = ? AND token = ?">>),

    mysql:prepare(unit_props, <<"SELECT id, factor, type_id FROM unit WHERE string_id = LOWER(?)">>),
    mysql:prepare(unit_factor, <<"SELECT factor FROM unit WHERE id = ?">>),

    mysql:prepare(token_insert, <<"INSERT INTO logger_tokens (token, meter, permissions) VALUES (?, ?, ?)">>),
    mysql:prepare(token_delete, <<"DELETE FROM logger_tokens WHERE meter = ?">>),
    mysql:prepare(sensor_key, <<"SELECT sha FROM (logger_devices ld INNER JOIN logger_meters lm ON ld.device = lm.device) WHERE lm.meter = ?">>),
    mysql:prepare(sensor_props, <<"SELECT uid, device, unit_id, factor FROM logger_meters WHERE meter = ?">>),
    mysql:prepare(sensor_factor, <<"SELECT factor FROM logger_meters WHERE meter = ?">>),
    mysql:prepare(sensor_device_type, <<"SELECT d.type_id FROM logger_devices d, logger_meters m WHERE d.device = m.device and m.meter = ?">>),
    mysql:prepare(device_sensors, <<"SELECT m.meter, m.external_id, m.function, m.description, un.string_id AS unit FROM logger_meters m, unit un WHERE m.device = ? AND m.unit_id = un.id">>),
    mysql:prepare(sensor_update, <<"UPDATE logger_meters SET access = ?, value = ? WHERE meter = ?">>),
    mysql:prepare(sensor_delete, <<"DELETE FROM logger_meters WHERE meter = ?">>),
    mysql:prepare(sensor_agg, <<"SELECT meter FROM logger_aggregated_meters WHERE virtual_meter = ?">>),
    mysql:prepare(sensor_agg_delete, <<"DELETE FROM logger_aggregated_meters WHERE meter = ?">>),
    mysql:prepare(sensor_storage_delete, <<"DELETE FROM logger_meter_storage WHERE meter = ?">>),
    mysql:prepare(sensor_insert, <<"INSERT INTO logger_meters (meter, uid, device, created, access, type, external_id, function, description, phase, constant, value, factor, unit_id, price, latitude, longitude) SELECT ?, uid, device, ?, 0, ?, ?, ?, ?, 0, 0, 0, ?, ?, 0.18, 49.444710, 7.769031 FROM logger_devices WHERE device = ?">>),
    mysql:prepare(sensor_config, <<"UPDATE logger_meters SET external_id = ?, function = ?, description = ?, unit_id = ? WHERE meter = ?">>),

    mysql:prepare(device_key, <<"SELECT sha FROM logger_devices WHERE device = ?">>),
    mysql:prepare(device_props, <<"SELECT sha, resets, firmware_id, description FROM logger_devices WHERE device = ?">>),
    mysql:prepare(device_serial, <<"SELECT serial FROM logger_devices WHERE device = ?">>),
    mysql:prepare(device_type, <<"SELECT type_id FROM logger_devices WHERE device = ?">>),
    mysql:prepare(device_update, <<"UPDATE logger_devices SET access = ?, version = ?, resets = ?, uptime = ?, memtotal = ?, memfree = ?, memcached = ?, membuffers = ?, sha = ?, firmware_id = ?, description = ? WHERE device = ?">>),
    mysql:prepare(device_insert, <<"INSERT INTO logger_devices (device, serial, uid, sha, created, firmware_id, resets, uptime, memtotal, memfree, memcached, membuffers, country, description, type_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)">>),
    mysql:prepare(device_delete, <<"DELETE FROM logger_devices WHERE device = ?">>),

    mysql:prepare(firmware_props, <<"SELECT id, release_time, build, tag, upgradable FROM logger_device_firmware WHERE version = ? AND device_type_id = ?">>),
    mysql:prepare(firmware_upgrade_delete, <<"DELETE FROM logger_firmware_upgrade_request WHERE device = ?">>),
    mysql:prepare(firmware_upgrade_props, <<"SELECT d.sha, d.type_id, f.version AS from_version, t.version AS to_version FROM logger_devices d, logger_device_firmware f, logger_device_firmware t, logger_firmware_upgrade_request u WHERE d.device = ? AND d.device = u.device AND d.firmware_id = f.id AND u.firmware_id = t.id AND f.upgradable = 1 AND t.upgradable = 1 AND u.approved > 0">>),

    mysql:prepare(notification_delete, <<"DELETE FROM notification WHERE device = ?">>),
    mysql:prepare(msgdump_delete, <<"DELETE FROM msgdump WHERE meter = ?">>),

    mysql:prepare(event_insert, <<"INSERT INTO event_log (device, event_id, time) VALUES (?, ?, ?)">>),
    mysql:prepare(event_delete, <<"DELETE FROM event_log WHERE device = ?">>),

    mysql:prepare(support_slot, <<"SELECT username, host, port, tunnel_port FROM device_support_slot WHERE device = ?">>),
    mysql:prepare(support_slot_release, <<"UPDATE support_slot SET device = NULL WHERE device = ?">>).


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
