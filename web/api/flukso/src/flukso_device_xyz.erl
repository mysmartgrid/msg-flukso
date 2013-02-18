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
    {['POST', 'DELETE'], ReqData, State}.


malformed_request(ReqData, State) ->
    case wrq:method(ReqData) of
        'POST'   -> malformed_POST(ReqData, State);
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

    State = #state{device = Device, digest = Digest},

    {case {ValidVersion, ValidDevice, ValidDigest, ValidKey} of
        {true, true, true, true} -> false;
        _ -> true
     end,
    ReqData, State}.


malformed_DELETE(ReqData, _State) ->
    io:fwrite("malformed_DELETE device\n"),

    {_Version, ValidVersion} = check_version(wrq:get_req_header("X-Version", ReqData)),
    {Device, ValidDevice} = check_device(wrq:path_info(device, ReqData)),
    {Digest, ValidDigest} = check_digest(wrq:get_req_header("X-Digest", ReqData)
),

    State = #state{device = Device, digest = Digest},

    {case {ValidVersion, ValidDevice, ValidDigest} of
        {true, true, true} -> false;
        _ -> true
     end,
    ReqData, State}.


is_authorized(ReqData, State) ->
    case wrq:method(ReqData) of
        'POST'   -> is_auth_POST(ReqData, State);
        'DELETE' -> is_auth_DELETE(ReqData, State)
    end.


is_auth_POST(ReqData, #state{device = Device, digest = ClientDigest} = State) ->

    {data, Result} = mysql:execute(pool, device_key, [Device]),

    case mysql:get_result_rows(Result) of

      %If device is found, use the key stored in the database
      [[_Key]] ->
        Key = _Key;

      %Otherwise, use key informed in the request
      _ ->
        {struct, JsonData} = mochijson2:decode(wrq:req_body(ReqData)),
        Key = proplists:get_value(<<"key">>, JsonData)
    end,   

    {check_digest(Key, ReqData, ClientDigest), ReqData, State}.


is_auth_DELETE(ReqData, #state{device = Device, digest = ClientDigest} = State) ->
    io:fwrite("is_auth_DELETE device\n"),

    {data, Result} = mysql:execute(pool, device_key, [Device]),

    case mysql:get_result_rows(Result) of

      %If device is found
      [[_Key]] ->
        Key = _Key,
        DigestCheck = check_digest(Key, ReqData, ClientDigest);

      %If device does not exist. %TODO: return proper message
      _ ->
        DigestCheck = true 
    end,

    {DigestCheck, ReqData, State}.


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

    IsDescriptionInformed = proplists:is_defined(<<"description">>, JsonData),

    case mysql:get_result_rows(Result) of

      %Device exists
      [[Key, Upgrade, Resets, OldFirmwareVersion, OldDescription]] ->

        IsKeyInformed = proplists:is_defined(<<"key">>, JsonData),

        if
           %New Device Message - 2nd invocation
           IsKeyInformed == true ->

            %Key can be changed, but the encryption is based on the formed key
            NewKey = proplists:get_value(<<"key">>, JsonData),
            Version = 0,
            Uptime = 0,
            Memtotal = 0,
            Memcached = 0,
            Membuffers = 0,
            Memfree = 0,
            NewResets = 0;
            
          %Heartbeat Message
          true ->
            NewKey = Key, %Key is not changed
            Version = proplists:get_value(<<"version">>, JsonData),
            Reset = proplists:get_value(<<"reset">>, JsonData),
            Uptime = proplists:get_value(<<"uptime">>, JsonData),
            Memtotal = proplists:get_value(<<"memtotal">>, JsonData),
            Memcached = proplists:get_value(<<"memcached">>, JsonData),
            Membuffers = proplists:get_value(<<"membuffers">>, JsonData),
            Memfree = proplists:get_value(<<"memfree">>, JsonData),
            NewResets = Resets + Reset
        end,

        IsFirmwareInformed = proplists:is_defined(<<"firmware">>, JsonData),

        if
          IsFirmwareInformed == true ->
            {struct, Firmware} = proplists:get_value(<<"firmware">>, JsonData),
            FirmwareVersion = proplists:get_value(<<"version">>, Firmware);
            %TODO: process <<"build">> and <<"tag">>

          true ->
            FirmwareVersion = OldFirmwareVersion
        end,

        if
          IsDescriptionInformed == true ->
            Description = proplists:get_value(<<"description">>, JsonData);

          true ->
            Description = OldDescription
        end,

        mysql:execute(pool, device_update,
          [Timestamp, Version, Upgrade, NewResets, Uptime, Memtotal, Memfree, Memcached, Membuffers, NewKey, FirmwareVersion, Description, Device]),

        mysql:execute(pool, event_insert, [Device, ?HEARTBEAT_RECEIVED_EVENT_ID, Timestamp]);

      %New Device Message - 1st invocation
      _ ->
        %Function unix_time() returns unique ids (as long as this code runs on a single machine).
        Serial = Timestamp,
        Upgrade = 0,
        Key = proplists:get_value(<<"key">>, JsonData),

        if
          IsDescriptionInformed == true ->
            Description = proplists:get_value(<<"description">>, JsonData);

          true ->
            Description = "Flukso Device"
        end,

        mysql:execute(pool, device_insert,
          [Device, Serial, 0, Key, Timestamp, 0, 0, "2.0.0-0", 0, 0, 0, 0, 0, 0, 0, 0, 0, "DE", Description])
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
    io:fwrite("delete_resource device\n"),

    {_data, _Result} = mysql:execute(pool, device_sensors, [Device]),

    Sensors = mysql:get_result_rows(_Result),
    Deleted = [delete_device_sensor(Sensor) || [Sensor] <- Sensors],

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
  {ok}.
