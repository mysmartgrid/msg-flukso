%% @author Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>
%% @copyright (C) 2009-2011 Bart Van Der Meerssche
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%%
%% @doc Flukso API: /sensor/xyz resource specification 

-module(flukso_sensor_xyz).
-author('Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>').

-export([init/1,
         allowed_methods/2,
         malformed_request/2,
         is_authorized/2,
         content_types_provided/2,
         to_json/2,
         process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("flukso.hrl").

init([]) -> 
    {ok, undefined}.

% debugging
%init(Config) ->
%   {{trace, "/tmp"}, Config}.

allowed_methods(ReqData, State) ->
    {['POST', 'GET'], ReqData, State}.

malformed_request(ReqData, State) ->
    case wrq:method(ReqData) of
        'POST' -> malformed_POST(ReqData, State);
        'GET'  -> malformed_GET(ReqData, State)
    end.

malformed_POST(ReqData, _State) ->
    io:fwrite("malformed_POST sensor\n"),

    {_Version, ValidVersion} = check_version(wrq:get_req_header("X-Version", ReqData)),
    {RrdSensor, ValidSensor} = check_sensor(wrq:path_info(sensor, ReqData)),
    {Digest, ValidDigest} = check_digest(wrq:get_req_header("X-Digest", ReqData)),

    State = #state{rrdSensor = RrdSensor,
                   digest = Digest},

    {case {ValidVersion, ValidSensor, ValidDigest} of
        {true, true, true} -> false;
        _ -> true
     end,
    ReqData, State}.

malformed_GET(ReqData, _State) ->
    {_Version, ValidVersion} = check_version(wrq:get_req_header("X-Version", ReqData), wrq:get_qs_value("version", ReqData)),
    {RrdSensor, ValidSensor} = check_sensor(wrq:path_info(sensor, ReqData)),
    {RrdStart, RrdEnd, RrdResolution, ValidTime} = check_time(wrq:get_qs_value("interval", ReqData), wrq:get_qs_value("start", ReqData), wrq:get_qs_value("end", ReqData), wrq:get_qs_value("resolution", ReqData)),
    {RrdFactor, ValidUnit} = check_unit(wrq:get_qs_value("unit", ReqData)),
    {Token, ValidToken} = check_token(wrq:get_req_header("X-Token", ReqData), wrq:get_qs_value("token", ReqData)),
    {JsonpCallback, ValidJsonpCallback} = check_jsonp_callback(wrq:get_qs_value("jsonp_callback", ReqData)),

    State = #state{rrdSensor = RrdSensor, 
                   rrdStart = RrdStart,
                   rrdEnd = RrdEnd,
                   rrdResolution = RrdResolution,
                   rrdFactor = RrdFactor,
                   token = Token,
                   jsonpCallback = JsonpCallback},

    {case {ValidVersion, ValidSensor, ValidTime, ValidUnit, ValidToken, ValidJsonpCallback}  of
	{true, true, true, true, true, true} -> false;
	_ -> true
     end, 
    ReqData, State}.

is_authorized(ReqData, State) ->
    case wrq:method(ReqData) of
        'POST' -> is_auth_POST(ReqData, State);
        'GET'  -> is_auth_GET(ReqData, State)
    end.


is_auth_POST(ReqData, #state{rrdSensor = Sensor, digest = ClientDigest} = State) ->
    io:fwrite("is_auth_POST sensor\n"),

    {data, Result} = mysql:execute(pool, sensor_key, [Sensor]),

    case mysql:get_result_rows(Result) of
        [[Key]] ->
            Data = wrq:req_body(ReqData),
            <<X:160/big-unsigned-integer>> = crypto:sha_mac(Key, Data),
            ServerDigest = lists:flatten(io_lib:format("~40.16.0b", [X])),

            {case ServerDigest of
                 ClientDigest -> true;
                 _WrongDigest -> "Incorrect digest"
             end,
             ReqData, State};

        _NoKey ->
          %No proper provisioning for this sensor
          %FIXME: validate using the device informed as argument
          {true, ReqData, State}
    end.


is_auth_GET(ReqData, #state{rrdSensor = RrdSensor, token = Token} = State) ->
    io:fwrite("is_auth_GET sensor\n"),

    {data, Result} = mysql:execute(pool, permissions, [RrdSensor, Token]),

    {case mysql:get_result_rows(Result) of
        [[62]] -> true;
        _Permission -> "Access refused" 
    end,
    ReqData, State}.

content_types_provided(ReqData, State) -> 
        {[{"application/json", to_json}], ReqData, State}.

to_json(ReqData, #state{rrdSensor = RrdSensor, rrdStart = RrdStart, rrdEnd = RrdEnd, rrdResolution = RrdResolution, rrdFactor = RrdFactor, jsonpCallback = JsonpCallback} = State) -> 
    case wrq:get_qs_value("interval", ReqData) of
        _Interval -> Path = ?BASE_PATH
    end,

    %% debugging: io:format("~s~n", [erlrrd:c([[Path, [RrdSensor|".rrd"]], "AVERAGE", ["-s ", RrdStart], ["-e ", RrdEnd], ["-r ", RrdResolution]])]),

    case rrd_fetch(Path, RrdSensor, RrdStart, RrdEnd, RrdResolution) of
        {ok, Response} ->
            Filtered = [re:split(X, "[:][ ]", [{return,list}]) || [X] <- Response, string:str(X, ":") == 11],
            Datapoints = [[list_to_integer(X), round(list_to_float(Y) * RrdFactor)] || [X, Y] <- Filtered, string:len(Y) /= 3],
            Nans = [[list_to_integer(X), list_to_binary(Y)] || [X, Y] <- Filtered, string:len(Y) == 3],
            Final = mochijson2:encode(lists:merge(Datapoints, Nans)),
            {case JsonpCallback of
                undefined -> Final;
                _ -> [JsonpCallback, "(", Final, ");"]
             end,
            ReqData, State};

        {error, _Reason} ->
            {{halt, 404}, ReqData, State}
    end.


process_post(ReqData, State) ->
    io:fwrite("process_post sensor\n"),

    {struct, JsonData} = mochijson2:decode(wrq:req_body(ReqData)),
    Payload = {proplists:get_value(<<"measurements">>, JsonData),
               proplists:get_value(<<"config">>, JsonData)},

    case Payload of
        {undefined, undefined} ->
            {false, ReqData, State};
	{Measurements, undefined} ->
            process_measurements(Measurements, ReqData, State);
        {undefined, Config} ->
            process_config(Config, ReqData, State);
        {_Measurements, _Config} ->
            {false, ReqData, State}
    end.

% JSON: {"config":{"type":"electricity","enable":0,"class":"analog","current":50,"voltage":230}}
% Mochijson2: {struct,[{<<"config">>, {struct,[{<<"type">>,<<"electricity">>}, {<<"enable">>,0}, ... ]} }]}
process_config({struct, Params}, ReqData, #state{rrdSensor = Sensor} = State) ->

    io:fwrite("process_config sensor\n"),

    {data, Result} = mysql:execute(pool, sensor_props, [Sensor]),

    case mysql:get_result_rows(Result) of

      %Sensor is found
      [[_Uid, Device]] -> 

        Args = [%proplists:get_value(<<"class">>,    Params),
                %proplists:get_value(<<"type">>,     Params),
                proplists:get_value(<<"function">>, Params),
                %proplists:get_value(<<"voltage">>,  Params),
                %proplists:get_value(<<"current">>,  Params),
                %proplists:get_value(<<"phase">>,    Params),
                %proplists:get_value(<<"constant">>, Params),
                %proplists:get_value(<<"enable">>,   Params),
                Sensor],

        %TODO: create fields in table logger_meters
        {updated, _Result} = mysql:execute(pool, sensor_config, Args),
        RrdResponse = "ok";

      %Sensor is not found
      _ ->
        Timestamp = unix_time(),
        Function = proplists:get_value(<<"function">>, Params),
        Device = proplists:get_value(<<"device">>, Params),

        rrd_create(?BASE_PATH, Sensor),

        case rrd_create(?BASE_PATH, Sensor) of
          {ok, _RrdResponse} ->
            RrdResponse = "ok",
            mysql:execute(pool, sensor_insert, [Sensor, Timestamp, 0, 0, 1, Function, 0, 0, 0, 0, "watt", Device]);

          {error, RrdResponse} ->
            logger(0, <<"rrdcreate.base">>, list_to_binary(RrdResponse), ?ERROR, ReqData)
        end
    end,

    JsonResponse = mochijson2:encode({struct, [{<<"response">>, list_to_binary(RrdResponse)}]}),
    {true, wrq:set_resp_body(JsonResponse, ReqData), State}.


% JSON: {"measurements":[[<TS1>,<VALUE1>],...,[<TSn>,<VALUEn>]]}
% Mochijson2: {struct,[{<<"measurements">>,[[<TS1>,<VALUE1>],...,[<TSn>,<VALUEn>]]}]}
process_measurements(Measurements, ReqData, #state{rrdSensor = RrdSensor} = State) ->
    RrdData = [[integer_to_list(Time), ":", integer_to_list(Counter), " "] || [Time, Counter] <- Measurements],
    [LastTimestamp, LastValue] = lists:last(Measurements),

    {data, Result} = mysql:execute(pool, sensor_props, [RrdSensor]),
    [[Uid, _Device]] = mysql:get_result_rows(Result),

    case rrd_update(?BASE_PATH, RrdSensor, RrdData) of    
        {ok, _RrdResponse} ->
            RrdResponse = "ok",
            mysql:execute(pool, sensor_update, [unix_time(), LastValue, RrdSensor]);

        {error, RrdResponse} ->
            logger(Uid, <<"rrdupdate.base">>, list_to_binary(RrdResponse), ?ERROR, ReqData)
    end,

    %TODO: define constants
    mysql:execute(pool, event_insert, [_Device, 102, unix_time()]),

    JsonResponse = mochijson2:encode({struct, [{<<"response">>, list_to_binary(RrdResponse)}]}),
    {true , wrq:set_resp_body(JsonResponse, ReqData), State}.
