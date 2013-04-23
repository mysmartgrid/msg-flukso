%%
%% The /sensor/xyz resource implementation.
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

-module(flukso_sensor_xyz).
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
    {RrdSensor, ValidSensor} = check_sensor(wrq:path_info(sensor, ReqData)),
    {Digest, ValidDigest} = check_digest(wrq:get_req_header("X-Digest", ReqData)),

    State = #state{rrdSensor = RrdSensor, digest = Digest},

    ErrorCode = case {ValidVersion, ValidSensor, ValidDigest} of
      {true, true, true} ->
        try
          {struct, JsonData} = mochijson2:decode(wrq:req_body(ReqData)),
          Payload = {
            proplists:get_value(<<"measurements">>, JsonData),
            proplists:get_value(<<"config">>, JsonData)},

            case Payload of
              {undefined, undefined} -> ?HTTP_BAD_ARGUMENT;
              {undefined, Config} -> ?HTTP_OK;
              {Measurements, undefined} ->
                %Valid measurement timestamp: from -7 days to +5 minutes
                ServerTimestamp = unix_time(),
                FromTime = ServerTimestamp - ?WEEK,
                ToTime = ServerTimestamp + 5 * ?MINUTE,
                InvalidTimestamps = lists:filter(fun([Time, Counter]) -> (Time < FromTime) or (Time > ToTime) end, Measurements),
                case length(InvalidTimestamps) of
                  0 -> ?HTTP_OK;
                  _ -> ?HTTP_INVALID_TIMESTAMP 
                end;
              _ -> ?HTTP_BAD_ARGUMENT
            end
        catch
          _:_ -> ?HTTP_BAD_ARGUMENT
        end;
      _ -> ?HTTP_BAD_ARGUMENT
    end,

    {case ErrorCode of
        ?HTTP_OK -> false;
        _ -> {halt, ErrorCode}
     end,
    ReqData, State}.


malformed_GET(ReqData, _State) ->

    {_Version, ValidVersion} = check_version(
      wrq:get_req_header("X-Version", ReqData), 
      wrq:get_qs_value("version", ReqData)),
    {RrdSensor, ValidSensor} = check_sensor(wrq:path_info(sensor, ReqData)),
    {RrdStart, RrdEnd, RrdResolution, ValidTime} = check_time(
      wrq:get_qs_value("interval", ReqData), 
      wrq:get_qs_value("start", ReqData), 
      wrq:get_qs_value("end", ReqData), 
      wrq:get_qs_value("resolution", ReqData)),
    {RrdFactor, ValidUnit} = check_unit(wrq:get_qs_value("unit", ReqData)),

    TokenHeader = wrq:get_req_header("X-Token", ReqData),
    {Token, ValidToken} = case TokenHeader of
      undefined -> {undefined, true};
      _ -> check_token(TokenHeader, wrq:get_qs_value("token", ReqData))
    end,

    DigestHeader = wrq:get_req_header("X-Digest", ReqData),    
    {Digest, ValidDigest} = case DigestHeader of
      undefined -> {undefined, true};
      _ -> check_digest(DigestHeader)
    end,

    {JsonpCallback, ValidJsonpCallback} = check_jsonp_callback(wrq:get_qs_value("jsonp_callback", ReqData)),

    State = #state{rrdSensor = RrdSensor, 
                   rrdStart = RrdStart,
                   rrdEnd = RrdEnd,
                   rrdResolution = RrdResolution,
                   rrdFactor = RrdFactor,
                   token = Token,
                   digest = Digest,
                   jsonpCallback = JsonpCallback},

    {case {ValidVersion, ValidSensor, ValidTime, ValidUnit, ValidToken, ValidDigest, ValidJsonpCallback} of
	{true, true, true, true, true, true, true} -> false;
	_ -> true
     end, 
    ReqData, State}.

malformed_DELETE(ReqData, _State) ->

    {_Version, ValidVersion} = check_version(wrq:get_req_header("X-Version", ReqData)),
    {RrdSensor, ValidSensor} = check_sensor(wrq:path_info(sensor, ReqData)),
    {Digest, ValidDigest} = check_digest(wrq:get_req_header("X-Digest", ReqData)),

    State = #state{rrdSensor = RrdSensor, digest = Digest},

    {case {ValidVersion, ValidSensor, ValidDigest} of
        {true, true, true} -> false;
        _ -> true
     end,
    ReqData, State}.

is_authorized(ReqData, State) ->
    case wrq:method(ReqData) of
        'POST'   -> is_auth_POST(ReqData, State);
        'GET'    -> is_auth_GET(ReqData, State);
        'DELETE' -> is_auth_DELETE(ReqData, State)
    end.


is_auth_POST(ReqData, #state{rrdSensor = Sensor, digest = ClientDigest} = State) ->

    {data, Result} = mysql:execute(pool, sensor_key, [Sensor]),

    case mysql:get_result_rows(Result) of

      %Sensor is found
      [[_Key]] ->
        Key = _Key;

      %Sensor is not registered yet
      _ ->
        %This must be a config message
        {struct, JsonData} = mochijson2:decode(wrq:req_body(ReqData)),
        {struct, Params} = proplists:get_value(<<"config">>, JsonData),
        Device = proplists:get_value(<<"device">>, Params),
        {_data, _Result} = mysql:execute(pool, device_key, [Device]),
        [[Key]] = mysql:get_result_rows(_Result)
    end,

    {check_digest(Key, ReqData, ClientDigest), ReqData, State}.


is_auth_GET(ReqData, #state{rrdSensor = RrdSensor, token = Token, digest = ClientDigest} = State) ->

    {case ClientDigest of
      undefined ->
      {data, Result} = mysql:execute(pool, permissions, [RrdSensor, Token]),
        case mysql:get_result_rows(Result) of
          [[62]] -> true;
          _Permission -> "Access refused"
        end;

      _ ->
        {data, Result} = mysql:execute(pool, sensor_key, [RrdSensor]),
        case mysql:get_result_rows(Result) of
          [[Key]] -> check_digest(Key, ReqData, ClientDigest);
          _ -> false
        end
    end,
    ReqData, State}.


is_auth_DELETE(ReqData, #state{rrdSensor = Sensor, digest = ClientDigest} = State) ->

    {data, Result} = mysql:execute(pool, sensor_key, [Sensor]),

    case mysql:get_result_rows(Result) of

      %Sensor is found
      [[_Key]] ->
        Key = _Key,
        Auth = check_digest(Key, ReqData, ClientDigest);

      %Sensor is not found %TODO: improve this function
      _ ->
        Auth = true 
    end,

    {Auth, ReqData, State}.


content_types_provided(ReqData, State) -> 
    {[{"application/json", to_json}], ReqData, State}.


to_json(ReqData, #state{rrdSensor = RrdSensor, rrdStart = RrdStart, rrdEnd = RrdEnd, rrdResolution = RrdResolution, rrdFactor = RrdFactor, jsonpCallback = JsonpCallback} = State) -> 
    case wrq:get_qs_value("interval", ReqData) of
        _Interval -> Path = ?BASE_PATH
    end,

    % Check if sensor is virtual
    {data, Result} = mysql:execute(pool, sensor_agg, [RrdSensor]),

    AggSeries = case mysql:get_result_rows(Result) of

        % Ordinary sensor
        [] ->
            {ok, Series} = query_sensor(Path, RrdSensor, RrdStart, RrdEnd, RrdResolution, RrdFactor),
            Series;

        % Virtual sensor
        RrdSensors ->
            AggSensors = RrdSensors,

            % Create a list of [Timestamp, Value] for every sensor
            AllSeries = [[query_sensor(Path, AggSensor, RrdStart, RrdEnd, RrdResolution, RrdFactor)] || [AggSensor] <- AggSensors],

            % Merge all pairs [Timestamp, Value]
            Merged = lists:merge([Series || [{ok, Series}] <- AllSeries]),

            % Convert NaN to zero
            ToNumber = fun (V) ->
                case is_number(V) of true -> V; false -> 0 end
            end,
            Converted = [[Timestamp, ToNumber(Value)] || [Timestamp, Value] <- Merged],

            % Form a sorted list of unique timestamps
            Timestamps = lists:usort([Timestamp || [Timestamp, Value] <- Converted]),

            % Sum values for every timestamp
            SumValues = fun (T) ->
                lists:sum([Value || [Timestamp, Value] <- Converted, Timestamp =:= T])
            end,
            [[Timestamp, SumValues(Timestamp)] || Timestamp <- Timestamps]
    end,

    Encoded = mochijson2:encode(AggSeries),

    {case JsonpCallback of
        undefined -> Encoded;
            _ -> [JsonpCallback, "(", Encoded, ");"]
    end,
    ReqData, State}.


query_sensor(Path, RrdSensor, RrdStart, RrdEnd, RrdResolution, RrdFactor) ->

    %debugging
    %io:format("~s~n", [erlrrd:c([[Path, [RrdSensor|".rrd"]], "AVERAGE", ["-s ", RrdStart], ["-e ", RrdEnd], ["-r ", RrdResolution]])]),

    case rrd_fetch(Path, RrdSensor, RrdStart, RrdEnd, RrdResolution) of
       {ok, Response} ->
           Filtered = [re:split(X, "[:][ ]", [{return,list}]) || [X] <- Response, string:str(X, ":") == 11],
           Datapoints = [[list_to_integer(X), round(list_to_float(Y) * RrdFactor)] || [X, Y] <- Filtered, string:len(Y) /= 4],
           Nans = [[list_to_integer(X), list_to_binary(Y)] || [X, Y] <- Filtered, string:len(Y) == 4],
           Final = lists:merge(Datapoints, Nans),
           {ok, Final};

       {error, _Reason} ->
           {error, _Reason}
   end.


process_post(ReqData, State) ->

    {struct, JsonData} = mochijson2:decode(wrq:req_body(ReqData)),
    Payload = {
      proplists:get_value(<<"measurements">>, JsonData),
      proplists:get_value(<<"config">>, JsonData)},

    case Payload of
        {undefined, undefined} ->
            {badarg, ReqData, State};

        {Measurements, undefined} ->
            process_measurements(Measurements, ReqData, State);

        {undefined, Config} ->
            process_config(Config, ReqData, State);

        {_Measurements, _Config} ->
            {badarg, ReqData, State}
    end.


%
% Config message example:
%
% JSON: {"config":{"device":"12345678901234567890123456789012","type":"electricity","enable":0,"class":"analog","current":50,"voltage":230}}
% Mochijson2: {struct,[{<<"config">>, {struct,[{<<"device">>,<<"12345678901234567890123456789012">>}, {<<"type">>,<<"electricity">>}, ... ]} }]}
%
process_config({struct, Params}, ReqData, #state{rrdSensor = Sensor} = State) ->

    {data, Result} = mysql:execute(pool, sensor_props, [Sensor]),

    case mysql:get_result_rows(Result) of

      %Sensor is found
      [[_Uid, Device]] ->

        Args = [%proplists:get_value(<<"class">>,      Params),
                %proplists:get_value(<<"type">>,       Params),
                proplists:get_value(<<"function">>,    Params),
                proplists:get_value(<<"description">>, Params),
                %proplists:get_value(<<"voltage">>,    Params),
                %proplists:get_value(<<"current">>,    Params),
                %proplists:get_value(<<"phase">>,      Params),
                %proplists:get_value(<<"constant">>,   Params),
                %proplists:get_value(<<"enable">>,     Params),
                Sensor],

        {updated, _Result} = mysql:execute(pool, sensor_config, Args),
        RrdResponse = "ok";

      %Sensor is not found
      _ ->
        Timestamp = unix_time(),
        Function = proplists:get_value(<<"function">>, Params),
        Description = proplists:get_value(<<"description">>, Params),
        Device = proplists:get_value(<<"device">>, Params),

        case rrd_create(?BASE_PATH, Sensor) of
          {ok, _RrdResponse} ->

            B = term_to_binary({node(), now()}),
            L = binary_to_list(erlang:md5(B)),
            Token = lists:flatten(list_to_hex(L)),

            RrdResponse = "ok",

            mysql:execute(pool, sensor_insert, [Sensor, Timestamp, 0, 1, Function, Description, 0, 0, 0, 0, "watt", Device]),
            mysql:execute(pool, token_insert, [Token, Sensor, 62]);

          {error, RrdResponse} ->
            logger(0, <<"rrdcreate.base">>, list_to_binary(RrdResponse), ?ERROR, ReqData)
        end
    end,

    JsonResponse = mochijson2:encode({struct, [{<<"response">>, list_to_binary(RrdResponse)}]}),
    {true, wrq:set_resp_body(JsonResponse, ReqData), State}.


%
% Measurement message example:
%
% JSON: {"measurements":[[<TS1>,<VALUE1>],...,[<TSn>,<VALUEn>]]}
% Mochijson2: {struct,[{<<"measurements">>,[[<TS1>,<VALUE1>],...,[<TSn>,<VALUEn>]]}]}
%
process_measurements(Measurements, ReqData, #state{rrdSensor = RrdSensor} = State) ->

    {data, Result} = mysql:execute(pool, sensor_props, [RrdSensor]),

    case mysql:get_result_rows(Result) of

      %Sensor is found
      [[Uid, Device]] ->

        ServerTimestamp = unix_time(),

        case rrd_last(string:concat(?BASE_PATH, [RrdSensor|".rrd"])) of
          % valid
          {ok, Response} ->
            RrdTimestamp = Response;

          % error
          {error, Reason} ->
            RrdTimestamp = 1
          end,

          case parse_measurements(ServerTimestamp, RrdTimestamp, Measurements) of

            %Measurements are valid
            {ok, RrdData} ->
              %debugging
              %UnsortedList = [[integer_to_list(T), ":", integer_to_list(C), " "] || [T, C] <- Measurements],
              %logger(Uid, <<"rrdupdate.base">>,
              %  string:concat(string:concat("Unsorted Measurements:\n", UnsortedList), string:concat("\nSorted Measurements:\n", RrdData)),
              %  ?INFO, ReqData),

              case rrd_update(?BASE_PATH, RrdSensor, RrdData) of

                %RRD files were successfully updated
                {ok, _RrdResponse} ->
                  RrdResponse = "ok",
                    [LastTimestamp, LastValue] = lists:last(Measurements),

                    %debugging
                    %io:fwrite(string:concat("RrdData=", erlrrd:c([RrdData]))),

                    mysql:execute(pool, sensor_update, [ServerTimestamp, LastValue, RrdSensor]),
                    mysql:execute(pool, event_insert, [Device, ?MEASUREMENT_RECEIVED_EVENT_ID, ServerTimestamp]);
    
                  %RRD files were not successfully updated
                  {error, RrdResponse} ->
                    logger(Uid, <<"rrdupdate.base">>, list_to_binary(RrdResponse), ?ERROR, ReqData)
              end;

            %Invalid Timestamp
            {error, RrdData} ->
              mysql:execute(pool, event_insert, [Device, ?INVALID_TIMESTAMP_EVENT_ID, ServerTimestamp]),
              RrdResponse = "Invalid Timestamp";

            %Invalid Measurements
            _ ->
              mysql:execute(pool, event_insert, [Device, ?CORRUPTED_MEASUREMENT_EVENT_ID, ServerTimestamp]),
              RrdResponse = "Invalid Measurements"
          end;

      %Unknown sensor
      _ ->
        RrdResponse = "Unknown Sensor"
    end,

    JsonResponse = mochijson2:encode({struct, [{<<"response">>, list_to_binary(RrdResponse)}]}),
    {RrdResponse == "ok", wrq:set_resp_body(JsonResponse, ReqData), State}.


parse_measurements(ServerTimestamp, RrdTimestamp, Measurements) ->

  try
    Sorted = lists:sort(fun([Time1, Counter1], [Time2, Counter2]) -> Time1 < Time2 end, Measurements),
    {Filtered, MultipleSend} = lists:partition(fun([Time1, Counter1]) -> RrdTimestamp < Time1 end, Sorted),

    %debuging
    %if
    %  length(MultipleSend) == 0 ->
    %    logger(Uid, <<"rrdupdate.base">>, "Filtered duplicated values", ?WARNING, ReqData)
    %end,

    {ok, [[integer_to_list(Time), ":", integer_to_list(Counter), " "] || [Time, Counter] <- Filtered]}
  catch
    _:_ ->
      {error, error}
  end.

delete_resource(ReqData, #state{rrdSensor = RrdSensor, digest = ClientDigest} = State) ->

    mysql:execute(pool, msgdump_delete, [RrdSensor]),
    mysql:execute(pool, sensor_agg_delete, [RrdSensor]),
    mysql:execute(pool, token_delete, [RrdSensor]),
    mysql:execute(pool, sensor_delete, [RrdSensor]),

    JsonResponse = mochijson2:encode({struct, [{<<"response">>, list_to_binary([])}]}),
    {true, wrq:set_resp_body(JsonResponse, ReqData), State}.
