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
        'DELETE' -> malformed_DELETE(ReqData, State)
    end.


malformed_POST(ReqData, _State) ->

    Return = case check_version(wrq:get_req_header("X-Version", ReqData)) of
      {Version, true} ->

        case check_digest(wrq:get_req_header("X-Digest", ReqData)) of
          {Digest, true} ->

            case check_sensor(wrq:path_info(sensor, ReqData)) of
              {RrdSensor, true} ->

                try
                  {struct, JsonData} = mochijson2:decode(wrq:req_body(ReqData)),
                  Payload = {
                    proplists:get_value(<<"measurements">>, JsonData),
                    proplists:get_value(<<"config">>, JsonData)},

                  ErrorCode = case Payload of
                    {undefined, undefined} -> ?HTTP_BAD_ARGUMENT;
                    {undefined, Config} -> ?HTTP_OK;
                    {Measurements, undefined} -> ?HTTP_OK;
                    _ -> ?HTTP_BAD_ARGUMENT
                  end,

                  case ErrorCode of
                    ?HTTP_OK -> {false, ReqData, #state{rrdSensor = RrdSensor, digest = Digest}};
                    _ -> ErrorCode
                  end
                catch  
                  _:_ -> ?HTTP_BAD_ARGUMENT
                end;
              _ -> ?HTTP_INVALID_ID
            end;
          _ -> ?HTTP_UNAUTHORIZED
        end;
      _ -> ?HTTP_NOT_IMPLEMENTED
    end,

    case Return of
      {false, ReqData, State} -> Return;
      _ -> {{halt, Return}, ReqData, undefined}
    end.


malformed_GET(ReqData, _State) ->

    Return = case check_version(wrq:get_req_header("X-Version", ReqData), wrq:get_qs_value("version", ReqData)) of
      {Version, true} ->

        case check_sensor(wrq:path_info(sensor, ReqData)) of
          {RrdSensor, true} ->

            case check_time(
              wrq:get_qs_value("interval", ReqData),
              wrq:get_qs_value("start", ReqData),
              wrq:get_qs_value("end", ReqData),
              wrq:get_qs_value("resolution", ReqData)) of

              {RrdStart, RrdEnd, RrdResolution, true} ->
                
                case check_unit(wrq:get_qs_value("unit", ReqData)) of
                  {UnitId, UnitFactor, true} ->

                    case check_jsonp_callback(wrq:get_qs_value("jsonp_callback", ReqData)) of
                      {JsonpCallback, true} ->

                        DigestHeader = wrq:get_req_header("X-Digest", ReqData),
                        {Digest, ValidDigest} = case DigestHeader of undefined -> {undefined, true}; _ -> check_digest(DigestHeader) end,
                        {Token, ValidToken} = check_token(wrq:get_req_header("X-Token", ReqData), wrq:get_qs_value("token", ReqData)),

                        Authenticated = case {Digest, ValidDigest, Token, ValidToken} of
                          {undefined, ValidDigest, undefined, ValidToken} -> false;
                          {Digest, true, undefined, ValidToken} -> true;
                          {undefined, ValidDigest, Token, true} -> true;
                          {Digest, true, Token, true} -> true;
                          _ -> false
                        end,
 
                        case Authenticated of
                          true ->
                            {false, ReqData, #state{
                              rrdSensor = RrdSensor,
                              rrdStart = RrdStart,
                              rrdEnd = RrdEnd,
                              rrdResolution = RrdResolution,
                              unitId = UnitId,
                              unitFactor = UnitFactor,
                              token = Token,
                              digest = Digest,
                              jsonpCallback = JsonpCallback}};

                          _ -> ?HTTP_UNAUTHORIZED
                        end;
                      _ -> ?HTTP_BAD_ARGUMENT
                    end;
                  _ -> ?HTTP_INVALID_UNIT
                end;
              _ -> ?HTTP_INVALID_TIME_PERIOD
            end;
          _ -> ?HTTP_INVALID_ID
        end;
      _ -> ?HTTP_NOT_IMPLEMENTED
    end,

    case Return of
      {false, ReqData, State} -> Return;
      _ -> {{halt, Return}, ReqData, undefined}
    end.


malformed_DELETE(ReqData, _State) ->

    Return = case check_version(wrq:get_req_header("X-Version", ReqData)) of
      {_Version, true} ->

        case check_digest(wrq:get_req_header("X-Digest", ReqData)) of
          {Digest, true} ->

            case check_sensor(wrq:path_info(sensor, ReqData)) of
              {RrdSensor, true} ->
                {false, ReqData, #state{rrdSensor = RrdSensor, digest = Digest}};

              _ -> ?HTTP_INVALID_ID
            end;
          _ -> ?HTTP_UNAUTHORIZED
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
        'DELETE' -> is_auth_DELETE(ReqData, State)
    end.


is_auth_POST(ReqData, #state{rrdSensor = Sensor, digest = ClientDigest} = State) ->

    {data, Result} = mysql:execute(pool, sensor_key, [Sensor]),

    [[Key]] = case mysql:get_result_rows(Result) of

      %Sensor is found
      [[_Key]] -> [[_Key]];

      %Sensor is not registered yet
      _ ->
        %This must be a config message
        {struct, JsonData} = mochijson2:decode(wrq:req_body(ReqData)),
        {struct, Params} = proplists:get_value(<<"config">>, JsonData),
        Device = proplists:get_value(<<"device">>, Params),
        {_data, _Result} = mysql:execute(pool, device_key, [Device]),
        mysql:get_result_rows(_Result)
    end,

    {check_digest(Key, ReqData, ClientDigest), ReqData, State}.


is_auth_GET(ReqData, #state{rrdSensor = RrdSensor, token = Token, digest = ClientDigest} = State) ->

    {case ClientDigest of
      undefined ->
      {data, Result} = mysql:execute(pool, permissions, [RrdSensor, Token]),
        case mysql:get_result_rows(Result) of
          [[62]] -> true;
          _Permission -> "Access denied"
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

    {case mysql:get_result_rows(Result) of
      %Sensor is found
      [[Key]] -> check_digest(Key, ReqData, ClientDigest);

      %Sensor is not found
      _ -> true 
    end,
    ReqData, State}.


content_types_provided(ReqData, State) -> 
    {[{"application/json", to_json}], ReqData, State}.


to_json(ReqData, #state{rrdSensor = RrdSensor, rrdStart = RrdStart, rrdEnd = RrdEnd, rrdResolution = RrdResolution, unitFactor = UnitFactor, jsonpCallback = JsonpCallback} = State) -> 

    % Check if sensor is virtual
    {data, Result} = mysql:execute(pool, sensor_agg, [RrdSensor]),

    AggSeries = case mysql:get_result_rows(Result) of

        % Ordinary sensor
        [] ->
            {ok, Series} = query_sensor(RrdSensor, RrdStart, RrdEnd, RrdResolution, UnitFactor),
            Series;

        % Virtual sensor
        RrdSensors ->
            AggSensors = RrdSensors,

            % Create a list of [Timestamp, Value] for every sensor
            AllSeries = [[query_sensor(AggSensor, RrdStart, RrdEnd, RrdResolution, UnitFactor)] || [AggSensor] <- AggSensors],

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


query_sensor(RrdSensor, RrdStart, RrdEnd, RrdResolution, UnitFactor) ->

    %debugging
    %io:format("~s~n", [erlrrd:c([[Path, [RrdSensor|".rrd"]], "AVERAGE", ["-s ", RrdStart], ["-e ", RrdEnd], ["-r ", RrdResolution]])]),

    {data, Result} = mysql:execute(pool, sensor_factor, [RrdSensor]),
    [[RrdFactor]] = mysql:get_result_rows(Result),

    case rrd_fetch(?BASE_PATH, RrdSensor, RrdStart, RrdEnd, RrdResolution) of
       {ok, Response} ->
           Filtered = [re:split(X, "[:][ ]", [{return,list}]) || [X] <- Response, string:str(X, ":") == 11],
           Datapoints = [[list_to_integer(X), list_to_float(Y) * RrdFactor * UnitFactor] || [X, Y] <- Filtered, string:len(Y) /= 4],
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


process_config({struct, Params}, ReqData, #state{rrdSensor = Sensor} = State) ->

    ExternalId = get_optional_value(<<"externalid">>, Params, Sensor),
    Function = proplists:get_value(<<"function">>, Params),
    Description = get_optional_value(<<"description">>, Params, ""),

    {data, Result} = mysql:execute(pool, sensor_props, [Sensor]),

    case mysql:get_result_rows(Result) of

      %Sensor is found
      [[_Uid, Device, UnitId, RrdFactor]] ->
        {updated, _Result} = mysql:execute(pool, sensor_config, [ExternalId, Function, Description, UnitId, Sensor]),
        RrdResponse = "ok",
        ErrorCode = ?HTTP_OK;

      %Sensor is not found
      _ ->
        Timestamp = unix_time(),
        Device = proplists:get_value(<<"device">>, Params),

        UnitString = get_optional_value(<<"unit">>, Params, "wh"),
        {_data, _Result} = mysql:execute(pool, unit_props, [UnitString]),

        case mysql:get_result_rows(_Result) of

          %Unit is valid
          [[UnitId, Factor, UnitType]] ->

            case rrd_create(?BASE_PATH, Sensor, UnitType) of

              %RRD file creation was successful
              {ok, _RrdResponse} ->
                B = term_to_binary({node(), now()}),
                L = binary_to_list(erlang:md5(B)),
                Token = lists:flatten(list_to_hex(L)),

                SensorType = case UnitType of
                  ?TEMPERATURE_UNIT_TYPE_ID -> ?TEMPERATURE_SENSOR_TYPE_ID;
                  ?PRESSURE_UNIT_TYPE_ID -> ?PRESSURE_SENSOR_TYPE_ID;
                  ?HUMIDITY_UNIT_TYPE_ID -> ?HUMIDITY_SENSOR_TYPE_ID; 
                  _ -> ?ENERGY_CONSUMPTION_SENSOR_TYPE_ID
                end,

                mysql:execute(pool, sensor_insert, [Sensor, Timestamp, SensorType, ExternalId, Function, Description, UnitId, Device]),
                mysql:execute(pool, token_insert, [Token, Sensor, 62]),

                RrdResponse = "ok",
                ErrorCode = ?HTTP_OK;

              % RRD file could not be created
              {error, RrdResponse} ->
                ErrorCode = ?HTTP_INTERNAL_SERVER_ERROR,
                logger(0, <<"rrdcreate.base">>, list_to_binary(RrdResponse), ?ERROR, ReqData)
            end;

          %Unit is not valid
          _ ->
            ErrorCode = ?HTTP_INVALID_UNIT,
            RrdResponse = "Invalid unit"
        end      
    end,

    JsonResponse = mochijson2:encode({struct, [{<<"response">>, list_to_binary(RrdResponse)}]}),
    {case ErrorCode of
        ?HTTP_OK -> true;
        _ -> {halt, ErrorCode}
     end,
    wrq:set_resp_body(JsonResponse, ReqData), State}.


process_measurements(Measurements, ReqData, #state{rrdSensor = RrdSensor} = State) ->

    {data, Result} = mysql:execute(pool, sensor_props, [RrdSensor]),

    case mysql:get_result_rows(Result) of

      %Sensor is found
      [[Uid, Device, UnitId, RrdFactor]] ->

        ServerTimestamp = unix_time(),

        RrdTimestamp = case rrd_last(string:concat(?BASE_PATH, [RrdSensor|".rrd"])) of
          % valid
          {ok, Response} -> Response;

          % error
          {error, Reason} -> 1
        end,

        %Old flukso devices have measurements stored in Wh, rather than Ws, so the unit factor has to be adjusted
        {data, _Result} = mysql:execute(pool, unit_factor, [UnitId]),
        [[UnitFactor]] = mysql:get_result_rows(_Result),
        Factor = UnitFactor * RrdFactor,

        case parse_measurements(ServerTimestamp, RrdTimestamp, Measurements, Factor) of

          %Measurements are valid
          {ok, RrdData} ->

            %debugging 
            %UnsortedList = [[integer_to_list(T), ":", float_to_list(float(C)), " "] || [T, C] <- Measurements],
            %logger(Uid, <<"rrdupdate.base">>,
            %  string:concat(string:concat("Unsorted Measurements:\n", UnsortedList), string:concat("\nSorted Measurements:\n", RrdData)),
            %  ?INFO, ReqData),

            case rrd_update(?BASE_PATH, RrdSensor, RrdData) of

              %RRD files were successfully updated
              {ok, _RrdResponse} ->
                [LastTimestamp, LastValue] = lists:last(Measurements),

                %debugging
                %io:fwrite(string:concat("RrdData=", erlrrd:c([RrdData]))),

                mysql:execute(pool, sensor_update, [ServerTimestamp, LastValue, RrdSensor]),
                mysql:execute(pool, event_insert, [Device, ?MEASUREMENT_RECEIVED_EVENT_ID, ServerTimestamp]),
                RrdResponse = "ok",
                ErrorCode = ?HTTP_OK;
    
              %RRD files were not successfully updated
              {error, RrdResponse} ->
                logger(Uid, <<"rrdupdate.base">>, list_to_binary(RrdResponse), ?ERROR, ReqData),
                ErrorCode = ?HTTP_FORBIDDEN
            end;

          %Invalid Timestamp
          {error, time} ->
            mysql:execute(pool, event_insert, [Device, ?INVALID_TIMESTAMP_EVENT_ID, ServerTimestamp]),
            RrdResponse = "Invalid Timestamp",
            ErrorCode = ?HTTP_INVALID_TIMESTAMP;

          %Invalid Measurements
          _ ->
            mysql:execute(pool, event_insert, [Device, ?CORRUPTED_MEASUREMENT_EVENT_ID, ServerTimestamp]),
            RrdResponse = "Invalid Measurement",
            ErrorCode = ?HTTP_INVALID_MEASUREMENT
        end;

      %Unknown sensor
      _ ->
        RrdResponse = "Unknown Sensor",
        ErrorCode = ?HTTP_FORBIDDEN
    end,

    JsonResponse = mochijson2:encode({struct, [{<<"response">>, list_to_binary(RrdResponse)}]}),
    {case ErrorCode of
        ?HTTP_OK -> true;
        _ -> {halt, ErrorCode}
     end,
    wrq:set_resp_body(JsonResponse, ReqData), State}.


parse_measurements(ServerTimestamp, RrdTimestamp, Measurements, Factor) ->

  try
    %Valid measurement timestamp: from -7 days to +5 minutes
    FromTime = ServerTimestamp - ?WEEK,
    ToTime = ServerTimestamp + 5 * ?MINUTE,
    InvalidTimestamps = lists:filter(fun([Time, Counter]) -> (Time < FromTime) or (Time > ToTime) end, Measurements),
    case length(InvalidTimestamps) of
      0 ->
        Sorted = lists:sort(fun([Time1, Counter1], [Time2, Counter2]) -> Time1 < Time2 end, Measurements),
        {Filtered, MultipleSend} = lists:partition(fun([Time1, Counter1]) -> RrdTimestamp < Time1 end, Sorted),

        if
          length(Filtered) > 0 ->
            {ok, [[integer_to_list(Time), ":", integer_to_list(trunc(float(Counter) / Factor)), " "] || [Time, Counter] <- Filtered]};

          true ->
            {error, time}
        end;
      _ -> {error, time}
    end
  catch
    _:_ ->
      {error, error}
  end.


delete_resource(ReqData, #state{rrdSensor = RrdSensor, digest = ClientDigest} = State) ->

    mysql:execute(pool, msgdump_delete, [RrdSensor]),
    mysql:execute(pool, sensor_agg_delete, [RrdSensor]),
    mysql:execute(pool, sensor_storage_delete, [RrdSensor]),
    mysql:execute(pool, token_delete, [RrdSensor]),
    mysql:execute(pool, sensor_delete, [RrdSensor]),

    JsonResponse = mochijson2:encode({struct, [{<<"response">>, list_to_binary([])}]}),
    {true, wrq:set_resp_body(JsonResponse, ReqData), State}.
