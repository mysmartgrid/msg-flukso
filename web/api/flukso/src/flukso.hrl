%%
%% Common record definitions and helper functions for the Flukso API.
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

-define(BASE_PATH,               "var/data/base/").
-define(FIRMWARE_UPGRADES_PATH,  "var/upgrades/").

-define(MINUTE,     60).
-define(QUARTER,   900).
-define(HOUR,     3600).
-define(DAY,     86400).
-define(WEEK,   604800).
-define(MONTH, 2419200).
-define(YEAR, 31536000).

-define(NO_COMMUNICATION_EVENT_ID,       1).
-define(COMMUNICATION_RESTORED_EVENT_ID, 3).
-define(PEAK_CONSUMPTION_EVENT_ID,       4).

-define(MESSAGE_RECEIVED_EVENT_ID,        100).
-define(HEARTBEAT_RECEIVED_EVENT_ID,      101).
-define(MEASUREMENT_RECEIVED_EVENT_ID,    102).
-define(BROWNOUT_EVENT_ID,                104).
-define(FIRMWARE_UPGRADED_EVENT_ID,       105).
-define(FAILED_FIRMWARE_UPGRADE_EVENT_ID, 106).

-define(CORRUPTED_MEASUREMENT_EVENT_ID,   201).
-define(INVALID_TIMESTAMP_EVENT_ID,       202).

-define(HTTP_OK,                200).
-define(HTTP_BAD_ARGUMENT,      400).
-define(HTTP_INVALID_TIMESTAMP, 470).

-record(state,
        {rrdSensor,
         rrdStart,
         rrdEnd,
         rrdResolution,
         rrdFactor,
         unitId,
         token,
         device,
         event,
         digest,
         jsonpCallback}).


%% checks
check_version(Version) ->
    case Version of
        "1.0" -> {Version, true};
        _ -> {false, false}
    end.

check_version(undefined, undefined) ->
    {false, false};
check_version(Version, undefined) ->
    check_version(Version);
check_version(undefined, Version) ->
    check_version(Version);
check_version(_, _) ->
    {false, false}.


check_event(Event) ->
    case Event of
        BROWNOUT_EVENT_ID -> {Event, true};
        FIRMWARE_UPGRADED_EVENT_ID -> {Event, true};
        FAILED_FIRMWARE_UPGRADE_EVENT_ID -> {Event, true};
        _ -> {false, false}
    end.

check_event(undefined, undefined) ->
    {false, false};
check_event(Event, undefined) ->
    check_event(Event);
check_event(undefined, Event) ->
    check_event(Event);
check_event(_, _) ->
    {false, false}.


check_sensor(Sensor) ->
    check_hex(Sensor, 32).

check_device(Device) ->
    check_hex(Device, 32).

check_key(Key) ->
    check_hex(Key, 32).

check_token(undefined, undefined) ->
    {false, false};
check_token(Token, undefined) ->
    check_hex(Token, 32);
check_token(undefined, Token) ->
    check_hex(Token, 32);
check_token(_, _) ->
    {false, false}.

check_digest(Digest) ->
    check_hex(Digest, 40).

check_hex(String, Length) ->
    io:write(String),
    case re:run(String, "[0-9a-f]+", []) of 
        {match, [{0, Length}]} -> {String, true};
        _ -> {false, false}
    end.

check_time(undefined, undefined, _End, _Resolution) ->
    {false, false, false, false};
check_time(Interval, undefined, undefined, undefined) ->
    case default_resolution(Interval) of
        false -> {false, false, false, false};
        DefResolution -> check_time(Interval, undefined, undefined, DefResolution)
    end;
check_time(Interval, undefined, undefined, Resolution) ->
    Now = unix_time(),
    case {time_to_seconds(Interval), time_to_seconds(Resolution)} of
        {false, _} -> {false, false, false, false};
        {_, false} -> {false, false, false, false};
        {IntervalSec, ResolutionSec} -> 
            AlignedEnd = time_align(Now, ResolutionSec),
            AlignedStart = AlignedEnd - IntervalSec,
            {integer_to_list(AlignedStart), integer_to_list(AlignedEnd), integer_to_list(ResolutionSec), true}
    end;
check_time(undefined, Start, undefined, Resolution) ->
    check_time(undefined, Start, integer_to_list(unix_time()), Resolution);
check_time(undefined, Start, End, undefined) ->
    check_time(undefined, Start, End, "minute");
check_time(undefined, Start, End, Resolution) ->
    case {re:run(Start, "[0-9]+", []), re:run(End, "[0-9]+", []), time_to_seconds(Resolution)} of
        {_, _, false} -> {false, false, false, false};
        {{match, [{0,_}]}, {match, [{0,_}]}, ResolutionSec} ->
            AlignedStart = time_align(list_to_integer(Start), ResolutionSec),
            AlignedEnd = time_align(list_to_integer(End), ResolutionSec),
            {integer_to_list(AlignedStart), integer_to_list(AlignedEnd), integer_to_list(ResolutionSec), true};
        _ -> {false, false, false, false}
    end;
check_time(_, _, _, _) ->
    {false, false, false, false}.


check_unit(Unit) ->
    UnitString = string:to_lower(Unit),
    {UnitString,
      case UnitString of
        "watt" -> true;
        "kwhperyear" -> true;
        "kwh" -> true;
        "wh" -> true;
        _ -> false  
      end}.


check_jsonp_callback(undefined) ->
    {undefined, true};
check_jsonp_callback(JsonpCallback) ->
    Length = string:len(JsonpCallback),

    case re:run(JsonpCallback, "[0-9a-zA-Z_]+", []) of
        {match, [{0, Length}]} -> {JsonpCallback, true};
        _ -> {false, false}
    end.


check_digest(Key, ReqData, ClientDigest) ->
    Data = wrq:req_body(ReqData),
    <<X:160/big-unsigned-integer>> = crypto:sha_mac(Key, Data),
    ServerDigest = lists:flatten(io_lib:format("~40.16.0b", [X])),

    case ServerDigest of
      ClientDigest -> true;
      _ -> "Incorrect digest"
    end.

digest_response(Key, Properties, ReqData, State) ->
    digest_response(Key, Properties, ReqData, State, true).

digest_response(Key, Properties, ReqData, State, Embody) ->

    JsonResponse = mochijson2:encode({struct, Properties}),

    <<X:160/big-unsigned-integer>> = crypto:sha_mac(Key, JsonResponse),
    Digest = lists:flatten(io_lib:format("~40.16.0b", [X])),

    DigestedReqData = wrq:set_resp_header("X-Digest", Digest, ReqData),

    case Embody of
      false ->
        {JsonResponse, DigestedReqData, State};
      true ->
        EmbodiedReqData = wrq:set_resp_body(JsonResponse, DigestedReqData),
        {true , EmbodiedReqData, State}
    end.


%% helper functions
unix_time() ->
    {Megaseconds, Seconds, _Microseconds} = erlang:now(),
    Megaseconds*1000000 + Seconds.

time_align(Time, Resolution) ->
    (Time div Resolution) * Resolution.

default_resolution(Interval) ->
    DefResolutions = [{"15min", "minute"},
                      {"hour", "minute"},
                      {"day", "15min"},
                      {"week", "day"},
                      {"month", "day"},
                      {"year", "week"},
                      {"night", "day"}],

    case lists:keyfind(Interval, 1, DefResolutions) of
        false -> false;
        {_Interval, Defresolution} -> Defresolution
    end.

time_to_seconds(Time) ->
    Times = [{"minute", ?MINUTE},
             {"15min", ?QUARTER},
             {"hour", ?HOUR},
             {"day", ?DAY},
             {"week", ?WEEK},
             {"month", ?MONTH},
             {"year", ?YEAR},
             {"night", ?MONTH}],

    case lists:keyfind(Time, 1, Times) of
        false -> false;
        {_Time, TimeSec} -> TimeSec
    end.

% severity levels
-define(EMERGENCY, 0).
-define(ALERT,     1).
-define(CRITICAL,  2).
-define(ERROR,     3).
-define(WARNING,   4).
-define(NOTICE,    5).
-define(INFO,      6).
-define(DEBUG,     7).

% logging severity threshold
-define(LOGLEVEL, 7).

% log to Drupal's watchdog table
logger(Uid, Type, Message, Severity, ReqData) when Severity =< ?LOGLEVEL ->
    mysql:execute(pool, watchdog,
        [Uid,
         Type,
         Message,
         <<"a:0:{}">>,
         Severity,
         list_to_binary(wrq:raw_path(ReqData)),
         list_to_binary(wrq:peer(ReqData)),
         unix_time()
        ]);
logger(_Uid, _Type, _Message, _Severity, _ReqData) ->
    true.

% erlrrd wrappers
rrd_fetch(Path, RrdSensor, RrdStart, RrdEnd, RrdResolution) ->
    erlrrd:fetch(erlrrd:c([[Path, [RrdSensor|".rrd"]], "AVERAGE", ["-s ", RrdStart], ["-e ", RrdEnd], ["-r ", RrdResolution]])).

rrd_update(Path, RrdSensor, RrdData) ->
    erlrrd:update([Path, [RrdSensor|".rrd"], " ", RrdData]).

rrd_create(Path, RrdSensor) ->
  %FIXME: use erlrrd:create
  file:copy("/var/www/flukso-api/flukso/var/data/base/derive.template.rrd", ["/var/www/flukso-api/flukso/var/data/base/"|[RrdSensor|".rrd"]]).

rrd_last(RRDFile) ->
  erlrrd:last(RRDFile).

% conversion functions
list_to_hex(L) -> lists:map(fun(X) -> int_to_hex(X) end, L).
 
int_to_hex(N) when N < 256 -> [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 -> $0+N;
hex(N) when N >= 10, N < 16 -> $a + (N-10).

