%% @author Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>
%% @copyright (C) 2011 Bart Van Der Meerssche
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
%% @doc Flukso API: /device/xyz resource specification 

-module(flukso_device_xyz).
-author('Bart Van Der Meerssche <bart.vandermeerssche@flukso.net>').

-export([init/1,
         allowed_methods/2,
         malformed_request/2,
         is_authorized/2,
         process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("flukso.hrl").

init([]) -> 
    {ok, undefined}.

% debugging
%init(Config) ->
%   {{trace, "/tmp"}, Config}.

allowed_methods(ReqData, State) ->
    {['POST'], ReqData, State}.

malformed_request(ReqData, State) ->
    case wrq:method(ReqData) of
        'POST' -> malformed_POST(ReqData, State)
    end.

malformed_POST(ReqData, _State) ->
    {_Version, ValidVersion} = check_version(wrq:get_req_header("X-Version", ReqData)),
    {Device, ValidDevice} = check_device(wrq:path_info(device, ReqData)),
    {Digest, ValidDigest} = check_digest(wrq:get_req_header("X-Digest", ReqData)),

    State = #state{device = Device,
                   digest = Digest},

    {case {ValidVersion, ValidDevice, ValidDigest} of
        {true, true, true} -> false;
        _ -> true
     end,
    ReqData, State}.

is_authorized(ReqData, State) ->
    case wrq:method(ReqData) of
        'POST' -> is_auth_POST(ReqData, State)
    end.

is_auth_POST(ReqData, #state{device = Device, digest = ClientDigest} = State) ->
    {data, Result} = mysql:execute(pool, device_key, [Device]),

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
            {true, ReqData, State}
    end.

% JSON: {"memtotal":13572,"version":210,"memcached":3280,"membuffers":1076,"memfree":812,"uptime":17394,"reset":1}
% Mochijson2: {struct,[{<<"memtotal">>,   13572},
%                      {<<"version">>,      210},
%                      {<<"memcached">>,   3280},
%                      {<<"membuffers">>,  1076},
%                      {<<"memfree">>,      812},
%                      {<<"uptime">>,     17394},
%                      {<<"reset">>,          1}]}
process_post(ReqData, #state{device = Device} = State) ->
    {data, Result} = mysql:execute(pool, device_props, [Device]),

    Timestamp = unix_time(),
    {struct, JsonData} = mochijson2:decode(wrq:req_body(ReqData)),

    case mysql:get_result_rows(Result) of

      %Device exists
      [[Key, Upgrade, Resets]] ->

        IsKeyInformed = proplists:is_defined(<<"key">>, JsonData),

        if
          %New Device Message - 2nd invocation
           IsKeyInformed == true ->

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

            NewKey = Key,
            Version = proplists:get_value(<<"version">>, JsonData),
            Reset = proplists:get_value(<<"reset">>, JsonData),
            Uptime = proplists:get_value(<<"uptime">>, JsonData),
            Memtotal = proplists:get_value(<<"memtotal">>, JsonData),
            Memcached = proplists:get_value(<<"memcached">>, JsonData),
            Membuffers = proplists:get_value(<<"membuffers">>, JsonData),
            Memfree = proplists:get_value(<<"memfree">>, JsonData),
            NewResets = Resets + Reset
        end,

        mysql:execute(pool, device_update,
            [Timestamp, Version, 0, NewResets, Uptime, Memtotal, Memfree, Memcached, Membuffers, NewKey, Device]),

        mysql:execute(pool, event_insert, [Device, ?HEARTBEAT_RECEIVED_EVENT_ID, Timestamp]);

      %New Device Message - 1st invocation
      _ ->
     
        %FIXME: find a better Serial Number generator
        Serial = Timestamp,
        Upgrade = 0,
        Key = proplists:get_value(<<"key">>, JsonData),

        mysql:execute(pool, device_insert,
            [Device, Serial, 0, Key, Timestamp, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, "DE"])
    end,

    JsonResponse = mochijson2:encode({ struct, [{<<"upgrade">>,   Upgrade}, {<<"timestamp">>, Timestamp}] }),

    <<X:160/big-unsigned-integer>> = crypto:sha_mac(Key, JsonResponse),
    Digest = lists:flatten(io_lib:format("~40.16.0b", [X])),

    DigestedReqData = wrq:set_resp_header("X-Digest", Digest, ReqData),
    EmbodiedReqData = wrq:set_resp_body(JsonResponse, DigestedReqData),

    {true, EmbodiedReqData, State}.
