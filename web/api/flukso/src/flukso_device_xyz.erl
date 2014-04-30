%%
%% The /device/xyz resource implementation.
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

-module(flukso_device_xyz).
-author('Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>, Ely de Oliveira <ely.oliveira@itwm.fraunhofer.de>').

-export([init/1,
         allowed_methods/2,
         malformed_request/2,
         is_authorized/2,
         content_types_provided/2,
         to_json/2,
         process_post/2,
         delete_resource/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("flukso.hrl").

%init([]) -> 
%    {ok, undefined}.


%Logging
init(Config) ->
   {{trace, "/var/log/erlang/flukso-api/trace"}, Config}.


allowed_methods(ReqData, State) ->
    {['POST', 'GET', 'DELETE'], ReqData, State}.


malformed_request(ReqData, State) ->
    case wrq:method(ReqData) of
        'POST'   -> malformed_POST(ReqData, State);
        'GET'    -> malformed_GET(ReqData, State);
        'DELETE' -> malformed_GET(ReqData, State)
    end.


malformed_POST(ReqData, _State) ->

    Return = case check_version(wrq:get_req_header("X-Version", ReqData)) of
      {Version, true} ->

        case check_device(wrq:path_info(device, ReqData)) of
          {Device, true} ->

            case check_digest(wrq:get_req_header("X-Digest", ReqData)) of
              {Digest, true} ->

                {struct, JsonData} = mochijson2:decode(wrq:req_body(ReqData)),
                case check_optional_key(JsonData) of
                  {Key, true} ->

                    case check_optional_device_type(JsonData) of
                      %Device type not informed
                      {?UNKNOWN_DEVICE_TYPE_ID, true} ->
                        {data, Result} = mysql:execute(pool, device_serial, [Device]),%FIXME: get rid of this test
                        {false, ReqData, #state{device = Device, digest = Digest, typeId =
                          case mysql:get_result_rows(Result) of
                            [[Serial]] -> if Serial < 99000000 -> ?FLUKSO1_DEVICE_TYPE_ID; true -> ?FLUKSO2_DEVICE_TYPE_ID end;
                            _ -> ?FLUKSO2_DEVICE_TYPE_ID
                          end}};

                      %Valid device type informed
                      {TypeId, true} ->
                        {false, ReqData, #state{device = Device, digest = Digest, typeId = TypeId}};

                      %Invalid device type
                      _ -> ?HTTP_INVALID_TYPE 
                    end;
                  _ -> ?HTTP_INVALID_KEY
                end;
              _ -> ?HTTP_UNAUTHORIZED
            end;
          _ -> ?HTTP_INVALID_ID
        end;
      _ -> ?HTTP_NOT_IMPLEMENTED
    end,

    case Return of
      {false, ReqData, State} -> Return;
      _ -> {{halt, Return}, ReqData, undefined}
    end.


malformed_GET(ReqData, _State) ->

    Return = case check_version(wrq:get_req_header("X-Version", ReqData)) of
      {Version, true} ->

        case check_device(wrq:path_info(device, ReqData)) of
          {Device, true} ->

            case check_digest(wrq:get_req_header("X-Digest", ReqData)) of
              {Digest, true} ->
                {false, ReqData, #state{device = Device, digest = Digest}};

              _ -> ?HTTP_UNAUTHORIZED
            end;
          _ -> ?HTTP_INVALID_ID
        end;
      _ -> ?HTTP_NOT_IMPLEMENTED
    end,

    case Return of
      {false, ReqData, State} -> Return;
      _ -> {{halt, Return}, ReqData, undefined}
    end.


is_authorized(ReqData, State) ->
    case wrq:method(ReqData) of
        'POST'   -> is_auth_POST(ReqData, State);
        'GET'    -> is_auth_GET(ReqData, State);
        'DELETE' -> is_auth_GET(ReqData, State)
    end.


is_auth_POST(ReqData, #state{device = Device, digest = ClientDigest} = State) ->

    {data, Result} = mysql:execute(pool, device_key, [Device]),

    {case mysql:get_result_rows(Result) of

      %If device is found, use the key stored in the database
      [[Key]] -> check_digest(Key, ReqData, ClientDigest);

      %Otherwise, use key informed in the request
      _ ->
        {struct, JsonData} = mochijson2:decode(wrq:req_body(ReqData)),
        case proplists:is_defined(<<"key">>, JsonData) of

          true -> check_digest(proplists:get_value(<<"key">>, JsonData), ReqData, ClientDigest);
          _ -> {halt, ?HTTP_NOT_FOUND}
        end
    end, ReqData, State}.


is_auth_GET(ReqData, #state{device = Device, digest = ClientDigest} = State) ->

    {data, Result} = mysql:execute(pool, device_key, [Device]),

    {case mysql:get_result_rows(Result) of

      %If device is found, use the key stored in the database
      [[Key]] -> check_digest(Key, ReqData, ClientDigest);

      %Otherwise, return false
      _ -> false

    end, ReqData, State}.


content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.


to_json(ReqData, #state{device = Device, jsonpCallback = JsonpCallback} = State) ->
    {data, Result} = mysql:execute(pool, device_props, [Device]),
    [[Key, Resets, FirmwareId, DeviceDescription]] = mysql:get_result_rows(Result),

    {data, _Result} = mysql:execute(pool, device_sensors, [Device]),
    _Sensors = mysql:get_result_rows(_Result),

    Sensors = [{struct, [
        {<<"meter">>, Meter},
        {<<"externalid">>, ExternalId},
        {<<"function">>, Function},
        {<<"description">>, SensorDescription},
        {<<"unit">>, Unit}]} || [Meter, ExternalId, Function, SensorDescription, Unit] <- _Sensors],

    Encoded = mochijson2:encode({struct, [
              {<<"description">>, DeviceDescription},
              {<<"sensors">>, Sensors}]}),

    {case JsonpCallback of
        undefined -> Encoded;
            _ -> [JsonpCallback, "(", Encoded, ");"]
    end,
    ReqData, State}.


process_post(ReqData, #state{device = Device, typeId = TypeId} = State) ->

    {data, Result} = mysql:execute(pool, device_props, [Device]),

    Timestamp = unix_time(),
    {struct, JsonData} = mochijson2:decode(wrq:req_body(ReqData)),

    Version = get_optional_value(<<"version">>, JsonData, 0),
    Reset = get_optional_value(<<"reset">>, JsonData, 0),
    Uptime = get_optional_value(<<"uptime">>, JsonData, 0),
    Memtotal = get_optional_value(<<"memtotal">>, JsonData, 0),
    Memcached = get_optional_value(<<"memcached">>, JsonData, 0),
    Membuffers = get_optional_value(<<"membuffers">>, JsonData, 0),
    Memfree = get_optional_value(<<"memfree">>, JsonData, 0),

    %Check if firmware has been informed
    {InformedVersion, InformedFirmwareId} = case proplists:is_defined(<<"firmware">>, JsonData) of

      %Firmware informed
      true ->
        {struct, Firmware} = proplists:get_value(<<"firmware">>, JsonData),
        FirmwareVersion = proplists:get_value(<<"version">>, Firmware),
        {data, _Result} = mysql:execute(pool, firmware_props, [FirmwareVersion, TypeId]),

        {FirmwareVersion, case mysql:get_result_rows(_Result) of
          [[KnownFirmwareId, _Time, _Build, _Tag, _Upg]] -> KnownFirmwareId;
          _ -> ?UNKNOWN_FIRMWARE_ID
        end};

      %Firmware not informed
      _ ->
        case TypeId of
          %Old releases of Flukso 2 never inform the firmware version. So, this field must be set here.
          ?FLUKSO2_DEVICE_TYPE_ID -> {?FLUKSO2_DEFAULT_FIRMWARE_VERSION, ?FLUKSO2_DEFAULT_FIRMWARE_ID};

          _ -> {undefined, ?UNKNOWN_FIRMWARE_ID}
        end
    end,

    {Response, ErrorCode} = case mysql:get_result_rows(Result) of

      %Device exists
      [[Key, Resets, CurrentFirmwareId, CurrentDescription]] ->

        %Retrieve Network configuration if it exists
        {data, NResult} = mysql:execute(pool, device_network_props, [Device]),
        OldNetwork = case mysql:get_result_rows(NResult) of
          [[1, _LanEnabled, _LanProtocol, _LanIp, _LanNetmask, _LanGateway, _WifiEnabled, _WifiEssid, _WifiEnc, _WifiPsk, _WifiProtocol, _WifiIp, _WifiNetmask, _WifiGateway]] ->
            {1, _LanEnabled, _LanProtocol, _LanIp, _LanNetmask, _LanGateway, _WifiEnabled, _WifiEssid, _WifiEnc, _WifiPsk, _WifiProtocol, _WifiIp, _WifiNetmask, _WifiGateway};
          _ ->
             undefined
        end,

        %Decide which key to use
        {NewKey, NewResets} = case  proplists:is_defined(<<"key">>, JsonData) of

          %New Device Message - 2nd invocation
          true -> {proplists:get_value(<<"key">>, JsonData), 0}; %Key can be changed, but the encryption is based on the formed key
            
          %Heartbeat Message
          _ -> {Key, Resets + Reset} %Same Key
        end,

        {FirmwareId, Upgrade} = case InformedVersion of

          %Device has not informed its firmware version
          undefined -> {CurrentFirmwareId, 0};

          %Device has informed its firmware version
          _ ->

            %Check if there is an upgrade request for the device
            {data, R} = mysql:execute(pool, firmware_upgrade_props, [Device]),
            case mysql:get_result_rows(R) of

              %Upgrade request found, and device has performed a firmware upgrade
              [[K, T, FromVersion, InformedVersion]] ->

                %Delete upgrade request
                mysql:execute(pool, firmware_upgrade_delete, [Device]),
                {InformedFirmwareId, 0};

              %Upgrade request found, but device has not yet performed a firmware upgrade
              [[K, T, FromVersion, ToVersion]] -> {CurrentFirmwareId, 999};

              %No upgrade request found
              _ -> {InformedFirmwareId, 0}
            end
        end,

        DescriptionCheck = case CurrentDescription of
          undefined -> {undefined, true};
          _ -> check_printable_chars(get_optional_value(<<"description">>, JsonData, CurrentDescription))
        end,

        case DescriptionCheck of
          {Description, true} ->
            mysql:execute(pool, device_update, [Timestamp, Version, NewResets, Uptime, Memtotal, Memfree, Memcached, Membuffers, NewKey, FirmwareId, Description, Device]),

            Network = process_network(OldNetwork, Device, JsonData),
            case Network of
              ?HTTP_BAD_ARGUMENT -> {"Invalid Network Configuration", ?HTTP_BAD_ARGUMENT};
              _ ->
                mysql:execute(pool, event_insert, [Device, ?HEARTBEAT_RECEIVED_EVENT_ID, Timestamp]),
                {"ok", ?HTTP_OK}
            end;
          _ ->
            Network = undefined,
            {"Invalid Characters", ?HTTP_INVALID_CHARS}
        end;

      %New Device Message - 1st invocation
      _ ->
        %Function unix_time() returns unique ids (as long as this code runs on a single machine).
        Serial = Timestamp,
        Upgrade = 0,
        Key = proplists:get_value(<<"key">>, JsonData),
        Description = get_optional_value(<<"description">>, JsonData, "Flukso Device"),

        case check_printable_chars(Description)  of
          {Description, true} ->
             mysql:execute(pool, device_insert, [Device, Serial, 0, Key, Timestamp, InformedFirmwareId, Reset, Uptime, Memtotal, Memfree, Memcached, Membuffers, "DE", Description, TypeId]),
             Network = process_network(undefined, Device, JsonData),
             case Network of
               ?HTTP_BAD_ARGUMENT -> {"Invalid Network Configuration", ?HTTP_BAD_ARGUMENT};
               _ -> {"ok", ?HTTP_OK}
             end;
          _ ->
            Network = undefined,
            {"Invalid Characters", ?HTTP_INVALID_CHARS}
        end
    end,

    case ErrorCode of
      ?HTTP_OK ->
        Support = compose_support_tag(Device),
        Config = compose_config_tag(Network, Device),

        Answer = lists:append(lists:append([{<<"upgrade">>, Upgrade}, {<<"timestamp">>, Timestamp}], Support), Config),
        digest_response(Key, Answer, ReqData, State);
      _ ->
        JsonResponse = mochijson2:encode({struct, [{<<"response">>, list_to_binary(Response)}]}),
        {{halt, ErrorCode}, wrq:set_resp_body(JsonResponse, ReqData), State}
    end. 


process_network(OldNetwork, Device, JsonData) ->

  case proplists:is_defined(<<"network">>, JsonData) of
    true ->
      {struct, Network} = proplists:get_value(<<"network">>, JsonData),

      %Current network configuration
      {OldPending, OldLanEnabled, OldLanProtocol, OldLanIp, OldLanNetmask, OldLanGateway, OldWifiEnabled, OldWifiEssid, OldWifiEnc, OldWifiPsk, OldWifiProtocol, OldWifiIp, OldWifiNetmask, OldWifiGateway} =
        case OldNetwork of
          undefined -> {0, 0, undefined, undefined, undefined, undefined, 0, undefined, undefined, undefined, undefined, undefined, undefined, undefined};
          _ -> OldNetwork
        end,

      %If LAN properties are defined
      {LanEnabled, LanProtocol, LanIp, LanNetmask, LanGateway} = case proplists:is_defined(<<"lan">>, Network) of
        true ->
          {struct, Lan} = proplists:get_value(<<"lan">>, Network),
          {proplists:get_value(<<"enabled">>, Lan),
          proplists:get_value(<<"protocol">>, Lan),
          proplists:get_value(<<"ip">>, Lan),
          proplists:get_value(<<"netmask">>, Lan),
          proplists:get_value(<<"gateway">>, Lan)};
        _ -> {OldLanEnabled, OldLanProtocol, OldLanIp, OldLanNetmask, OldLanGateway}
      end,

      %If WIFI properties are defined
      {WifiEnabled, WifiEssid, WifiEnc, WifiPsk, WifiProtocol, WifiIp, WifiNetmask, WifiGateway} = case proplists:is_defined(<<"wifi">>, Network) of
        true ->
          {struct, Wifi} = proplists:get_value(<<"wifi">>, Network),
          {proplists:get_value(<<"enabled">>, Wifi),
          proplists:get_value(<<"essid">>, Wifi),
          proplists:get_value(<<"enc">>, Wifi),
          proplists:get_value(<<"psk">>, Wifi),
          proplists:get_value(<<"protocol">>, Wifi),
          proplists:get_value(<<"ip">>, Wifi),
          proplists:get_value(<<"netmask">>, Wifi),
          proplists:get_value(<<"gateway">>, Wifi)};
        _ -> {OldWifiEnabled, OldWifiEssid, OldWifiEnc, OldWifiPsk, OldWifiProtocol, OldWifiIp, OldWifiNetmask, OldWifiGateway}
      end,


      %FIXME: validate parameters. if invalid, return ?HTTP_BAD_ARGUMENT

      Pending = case OldNetwork of

        %If network record does not exist
        undefined ->
          mysql:execute(pool, device_network_insert, [Device, 0, LanEnabled, LanProtocol, LanIp, LanNetmask, LanGateway, WifiEnabled, WifiEssid, WifiEnc, WifiPsk, WifiProtocol, WifiIp, WifiNetmask, WifiGateway]),
          0;

        %Network record does exist
        _ ->
          Different = case {LanEnabled, LanProtocol, LanIp, LanNetmask, LanGateway, WifiEnabled, WifiEssid, WifiEnc, WifiPsk, WifiProtocol, WifiIp, WifiNetmask, WifiGateway} of
            {OldLanEnabled, OldLanProtocol, OldLanIp, OldLanNetmask, OldLanGateway, OldWifiEnabled, OldWifiEssid, OldWifiEnc, OldWifiPsk, OldWifiProtocol, OldWifiIp, OldWifiNetmask, OldWifiGateway} -> 0; %Equal
            _ -> 1 %Still different
          end,
          mysql:execute(pool, device_network_update, [Different]),
          Different
      end,
      {Pending, LanEnabled, LanProtocol, LanIp, LanNetmask, LanGateway, WifiEnabled, WifiEssid, WifiEnc, WifiPsk, WifiProtocol, WifiIp, WifiNetmask, WifiGateway};
    _ -> OldNetwork
  end.


compose_support_tag(Device) ->

    %Check if device has requested remote support, and if a port is available
    {data, _Result} = mysql:execute(pool, support_slot, [Device]),

    case mysql:get_result_rows(_Result) of

      [[User, Host, Port, TunnelPort]] ->

        KeysPath = string:concat(string:concat("./var/keys/", erlang:binary_to_list(Host)), "/"),

        %Device private key
        DeviceKeyPath = string:concat(string:concat(KeysPath, Device), "_device_id"),
        {ok, DeviceKey} = file:read_file(DeviceKeyPath),

        %Technician public key
        TechKeyPath = string:concat(string:concat(KeysPath, Device), "_tech_id.pub"),
        {ok, TechKey} = file:read_file(TechKeyPath),

        %Support host public key
        HostKeyPath = string:concat(KeysPath, "host_id.pub"),
        {ok, HostKey} = file:read_file(HostKeyPath),

        Support = {struct, [
          {<<"user">>, User},
          {<<"host">>, Host},
          {<<"port">>, Port},
          {<<"tunnelPort">>, TunnelPort},
          {<<"devicekey">>, base64:encode(DeviceKey)},
          {<<"techkey">>, TechKey},
          {<<"hostkey">>, HostKey}]},

        [{<<"support">>, Support}];

      _ -> []
    end.


compose_config_tag(Network, Device) ->

  case Network of
    %If pending
    {1, LanEnabled, LanProtocol, LanIp, LanNetmask, LanGateway, WifiEnabled, WifiEssid, WifiEnc, WifiPsk, WifiProtocol, WifiIp, WifiNetmask, WifiGateway} ->

      LanConfig = {struct, [
          {<<"enabled">>, LanEnabled},
          {<<"protocol">>, LanProtocol},
          {<<"ip">>, LanIp},
          {<<"netmask">>, LanNetmask},
          {<<"gateway">>, LanGateway}
        ]},

      WifiConfig = {struct, [
          {<<"enabled">>, WifiEnabled},
          {<<"essid">>, WifiEssid},
          {<<"enc">>, WifiEnc},
          {<<"psk">>, WifiPsk},
          {<<"protocol">>, WifiProtocol},
          {<<"ip">>, WifiIp},
          {<<"netmask">>, WifiNetmask},
          {<<"gateway">>, WifiGateway}
        ]},

     NetworkTag = {<<"network">>, {struct, [
       {<<"lan">>, LanConfig},
       {<<"wifi">>, WifiConfig}
     ]}},

     {data, Result} = mysql:execute(pool, device_active_sensors, [Device]),
     SensorsTag = {<<"sensors">>, [Meter || [Meter] <- mysql:get_result_rows(Result)]},

     [{<<"config">>, {struct, [NetworkTag, SensorsTag]}}];

    _ -> []
  end.


delete_resource(ReqData, #state{device = Device, digest = ClientDigest} = State) ->

    {data, _Result} = mysql:execute(pool, device_sensors, [Device]),

    Sensors = mysql:get_result_rows(_Result),
    [delete_device_sensor(Meter) || [Meter, ExternalId, Function, Description, Unit] <- Sensors],

    mysql:execute(pool, event_delete, [Device]),
    mysql:execute(pool, notification_delete, [Device]),
    mysql:execute(pool, support_slot_release, [Device]),
    mysql:execute(pool, device_network_delete, [Device]),
    mysql:execute(pool, device_delete, [Device]),

    JsonResponse = mochijson2:encode({struct, [{<<"response">>, list_to_binary([])}]}),
    {true, wrq:set_resp_body(JsonResponse, ReqData), State}.


delete_device_sensor(Sensor) ->

  mysql:execute(pool, msgdump_delete, [Sensor]),
  mysql:execute(pool, sensor_agg_delete, [Sensor]),
  mysql:execute(pool, token_delete, [Sensor]),
  mysql:execute(pool, energy_sensor_delete, [Sensor]),
  mysql:execute(pool, sensor_delete, [Sensor]),

  file:delete(string:concat(?BASE_PATH, string:concat(binary_to_list(Sensor), ".rrd"))).
