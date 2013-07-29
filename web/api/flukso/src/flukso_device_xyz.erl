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
-author('Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>').

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
        'DELETE' -> malformed_DELETE(ReqData, State)
    end.


malformed_POST(ReqData, _State) ->

    {_Version, ValidVersion} = check_version(wrq:get_req_header("X-Version", ReqData)),
    {Device, ValidDevice} = check_device(wrq:path_info(device, ReqData)),
    {Digest, ValidDigest} = check_digest(wrq:get_req_header("X-Digest", ReqData)),

    {struct, JsonData} = mochijson2:decode(wrq:req_body(ReqData)),
    IsKeyDefined = proplists:is_defined(<<"key">>, JsonData),
    if
      %When defined, Key is validated 
      IsKeyDefined == true ->
        {Key, ValidKey} = check_key(proplists:get_value(<<"key">>, JsonData));
      true ->
        ValidKey = true
    end,

    IsTypeDefined = proplists:is_defined(<<"type">>, JsonData),
    if
      %When defined, type is validated
      IsTypeDefined == true ->
        {Type, ValidType} = check_device_type(proplists:get_value(<<"type">>, JsonData));
      true ->
        ValidType = true
    end,

    State = #state{device = Device, digest = Digest},

    {case {ValidVersion, ValidDevice, ValidDigest, ValidKey, ValidType} of
        {true, true, true, true, true} -> false;
        _ -> true
     end,
    ReqData, State}.


malformed_GET(ReqData, _State) ->

    {_Version, ValidVersion} = check_version(wrq:get_req_header("X-Version", ReqData)),
    {Device, ValidDevice} = check_device(wrq:path_info(device, ReqData)),
    {Digest, ValidDigest} = check_digest(wrq:get_req_header("X-Digest", ReqData)),
    State = #state{device = Device, digest = Digest},

    {case {ValidVersion, ValidDevice, ValidDigest} of
        {true, true, true} -> false;
        _ -> true
     end,
    ReqData, State}.


malformed_DELETE(ReqData, _State) ->

    {_Version, ValidVersion} = check_version(wrq:get_req_header("X-Version", ReqData)),
    {Device, ValidDevice} = check_device(wrq:path_info(device, ReqData)),
    {Digest, ValidDigest} = check_digest(wrq:get_req_header("X-Digest", ReqData)),

    State = #state{device = Device, digest = Digest},

    {case {ValidVersion, ValidDevice, ValidDigest} of
        {true, true, true} -> false;
        _ -> true
    end, ReqData, State}.


is_authorized(ReqData, State) ->
    case wrq:method(ReqData) of
        'POST'   -> is_auth_POST(ReqData, State);
        'GET'    -> is_auth_GET(ReqData, State);
        'DELETE' -> is_auth_GET(ReqData, State)
    end.


is_auth_POST(ReqData, #state{device = Device, digest = ClientDigest} = State) ->

    {data, Result} = mysql:execute(pool, device_key, [Device]),

    Key = case mysql:get_result_rows(Result) of

      %If device is found, use the key stored in the database
      [[_Key]] -> _Key;

      %Otherwise, use key informed in the request
      _ ->
        {struct, JsonData} = mochijson2:decode(wrq:req_body(ReqData)),
        proplists:get_value(<<"key">>, JsonData)
    end,   

    {check_digest(Key, ReqData, ClientDigest), ReqData, State}.


is_auth_GET(ReqData, #state{device = Device, digest = ClientDigest} = State) ->

    {data, Result} = mysql:execute(pool, device_key, [Device]),

    {case mysql:get_result_rows(Result) of

      %If device is found, use the key stored in the database
      [[Key]] ->
           check_digest(Key, ReqData, ClientDigest);

      %Otherwise, return false
      _ -> false

    end, ReqData, State}.


content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.


to_json(ReqData, #state{device = Device, jsonpCallback = JsonpCallback} = State) ->
    {data, Result} = mysql:execute(pool, device_props, [Device]),
    [[Key, Upgrade, Resets, FirmwareVersion, DeviceDescription]] = mysql:get_result_rows(Result),

    {_data, _Result} = mysql:execute(pool, device_sensors, [Device]),
    _Sensors = mysql:get_result_rows(_Result),

    Sensors = [{struct, [
        {<<"meter">>, Meter},
        {<<"function">>, Function},
        {<<"description">>, SensorDescription},
        {<<"unit">>, Unit}]} || [Meter, Function, SensorDescription, Unit] <- _Sensors],

    Encoded = mochijson2:encode({struct, [
              {<<"description">>, DeviceDescription},
              {<<"sensors">>, Sensors}]}),

    {case JsonpCallback of
        undefined -> Encoded;
            _ -> [JsonpCallback, "(", Encoded, ");"]
    end,
    ReqData, State}.


%
% Heartbeat message example:
%
% JSON: {"memtotal":13572,"version":210,"memcached":3280,"membuffers":1076,"memfree":812,"uptime":17394,"reset":1,
%        "firmware":{"version":"2.3.1-1","releasetime":"20120131_1845"}}
% Mochijson2: {struct,[{<<"memtotal">>,   13572},
%                      {<<"version">>,      210},
%                      {<<"memcached">>,   3280},
%                      {<<"membuffers">>,  1076},
%                      {<<"memfree">>,      812},
%                      {<<"uptime">>,     17394},
%                      {<<"reset">>,          1},
%                      {<<"firmware">>,  {struct, [{<<"version">>,     "2.3.1-1"},
%                                                  {<<"releasetime">>, "20120131_1845"},
%                                                  {<<"build">>,       "f0ba69e4fea1d0c411a068e5a19d0734511805bd"},
%                                                  {<<"tag">>,         "flukso-2.0.3-rc1-19-gf0ba69e"}]}]}}
%
% Config message example:
%
% JSON: {"key":12345678901234567890123456789012}
% Mochijson2: {struct,[{<<"key">>, 12345678901234567890123456789012}]}
%
process_post(ReqData, #state{device = Device} = State) ->

    {data, Result} = mysql:execute(pool, device_props, [Device]),

    Timestamp = unix_time(),
    {struct, JsonData} = mochijson2:decode(wrq:req_body(ReqData)),

    case mysql:get_result_rows(Result) of

      %Device exists
      [[Key, Upgrade, Resets, OldFirmwareVersion, OldDescription]] ->

        Version = get_optional_value(<<"version">>, JsonData, 0),
        Reset = get_optional_value(<<"reset">>, JsonData, 0),
        Uptime = get_optional_value(<<"uptime">>, JsonData, 0),
        Memtotal = get_optional_value(<<"memtotal">>, JsonData, 0),
        Memcached = get_optional_value(<<"memcached">>, JsonData, 0),
        Membuffers = get_optional_value(<<"membuffers">>, JsonData, 0),
        Memfree = get_optional_value(<<"memfree">>, JsonData, 0),

        IsKeyInformed = proplists:is_defined(<<"key">>, JsonData),

        if
           %New Device Message - 2nd invocation
           IsKeyInformed == true ->

            %Key can be changed, but the encryption is based on the formed key
            NewKey = proplists:get_value(<<"key">>, JsonData),
            NewResets = 0;
            
          %Heartbeat Message
          true ->
            NewKey = Key, %Key is not changed
            NewResets = Resets + Reset
        end,

        IsFirmwareInformed = proplists:is_defined(<<"firmware">>, JsonData),

        FirmwareVersion = if
          IsFirmwareInformed == true ->
            {struct, Firmware} = proplists:get_value(<<"firmware">>, JsonData),
            proplists:get_value(<<"version">>, Firmware);
            %TODO: process <<"build">> and <<"tag">>

          true ->
            OldFirmwareVersion
        end,

        Description = get_optional_value(<<"description">>, JsonData, OldDescription),

        mysql:execute(pool, device_update,
          [Timestamp, Version, Upgrade, NewResets, Uptime, Memtotal, Memfree, Memcached, Membuffers, NewKey, FirmwareVersion, Description, Device]),

        mysql:execute(pool, event_insert, [Device, ?HEARTBEAT_RECEIVED_EVENT_ID, Timestamp]);

      %New Device Message - 1st invocation
      _ ->
        %Function unix_time() returns unique ids (as long as this code runs on a single machine).
        Serial = Timestamp,
        Upgrade = 0,
        Key = proplists:get_value(<<"key">>, JsonData),
        Description = get_optional_value(<<"description">>, JsonData, "Flukso Device"),

        TypeId = case proplists:is_defined(<<"type">>, JsonData) of
          true ->
            case proplists:get_value(<<"type">>, JsonData) of
              <<"flukso2">> -> ?FLUKSO2_DEVICE_TYPE_ID;
              <<"vzlogger">> -> ?VZLOGGER_DEVICE_TYPE_ID;
              <<"libklio">> -> ?LIBKLIO_DEVICE_TYPE_ID;
              _ -> ?UNKNOWN_DEVICE_TYPE_ID
            end;
          _ -> ?FLUKSO2_DEVICE_TYPE_ID 
        end,

        mysql:execute(pool, device_insert,
          [Device, Serial, 0, Key, Timestamp, 0, 0, "2.0.0-0", 0, 0, 0, 0, 0, 0, 0, 0, 0, "DE", Description, TypeId])
    end,

    Support = compose_support_tag(Device),
    Answer = lists:append([{<<"upgrade">>, Upgrade}, {<<"timestamp">>, Timestamp}], Support),

    digest_response(Key, Answer, ReqData, State).


compose_support_tag(Device) ->

    %Check if device has requested remote support, and if a port is available
    {_data, _Result} = mysql:execute(pool, support_slot, [Device]),

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


delete_resource(ReqData, #state{device = Device, digest = ClientDigest} = State) ->

    {_data, _Result} = mysql:execute(pool, device_sensors, [Device]),

    Sensors = mysql:get_result_rows(_Result),
    [delete_device_sensor(Meter) || [Meter, Function, Description, Unit] <- Sensors],

    mysql:execute(pool, event_delete, [Device]),
    mysql:execute(pool, notification_delete, [Device]),
    mysql:execute(pool, support_slot_release, [Device]),
    mysql:execute(pool, device_delete, [Device]),

    JsonResponse = mochijson2:encode({struct, [{<<"response">>, list_to_binary([])}]}),
    {true, wrq:set_resp_body(JsonResponse, ReqData), State}.


delete_device_sensor(Sensor) ->

  mysql:execute(pool, msgdump_delete, [Sensor]),
  mysql:execute(pool, sensor_agg_delete, [Sensor]),
  mysql:execute(pool, token_delete, [Sensor]),
  mysql:execute(pool, sensor_delete, [Sensor]),

  file:delete(string:concat(?BASE_PATH, string:concat(binary_to_list(Sensor), ".rrd"))).
