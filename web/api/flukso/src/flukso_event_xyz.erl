%%
%% The /event/xyz resource implementation.
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

-module(flukso_event_xyz).
-author('Ely de Oliveira <ely.oliveira@itwm.fraunhofer.de>').

-export([init/1,
         allowed_methods/2,
         malformed_request/2,
         is_authorized/2,
         process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("flukso.hrl").


%init([]) ->
%    {ok, undefined}.

%Logging
init(Config) ->
   {{trace, "/var/log/erlang/flukso-api/trace"}, Config}.


allowed_methods(ReqData, State) ->
    {['POST'], ReqData, State}.


malformed_request(ReqData, State) ->
    case wrq:method(ReqData) of
        'POST' -> malformed_POST(ReqData, State)
    end.


malformed_POST(ReqData, _State) ->

    Return = case check_version(wrq:get_req_header("X-Version", ReqData), wrq:get_qs_value("version", ReqData)) of
      {Version, true} ->

        case check_digest(wrq:get_req_header("X-Digest", ReqData)) of
          {Digest, true} ->

            {struct, JsonData} = mochijson2:decode(wrq:req_body(ReqData)),

            case check_device(proplists:get_value(<<"device">>, JsonData)) of
              {Device, true} ->

                case check_event(wrq:path_info(event, ReqData)) of
                  {Event, true} ->
                    {false, ReqData, #state{device = Device, event = Event, digest = Digest}};

                  _ -> ?HTTP_INVALID_EVENT
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


is_authorized(ReqData, State) ->
    case wrq:method(ReqData) of
        'POST' -> is_auth_POST(ReqData, State)
    end.


is_auth_POST(ReqData, #state{event = Event, device = Device, digest = ClientDigest} = State) ->

    {data, Result} = mysql:execute(pool, device_key, [Device]),

    {case mysql:get_result_rows(Result) of
        [[Key]] ->
            check_digest(Key, ReqData, ClientDigest);
        _ ->
            "No proper provisioning for this device"
    end,
    ReqData, State}.


process_post(ReqData, #state{event = Event, device = Device} = State) ->

    {data, Result} = mysql:execute(pool, device_props, [Device]),
    [[Key, Resets, FirmwareId, Description]] = mysql:get_result_rows(Result),

    {struct, JsonData} = mochijson2:decode(wrq:req_body(ReqData)),
    Timestamp = unix_time(),

    mysql:execute(pool, event_insert, [Device, Event, Timestamp]),

    case Event of
       ?CONFIG_NETWORK_FAILED_EVENT_ID -> mysql:execute(pool, device_config_update, [2, Device]);
       ?CONFIG_NETWORK_OK_EVENT_ID -> mysql:execute(pool, device_config_update2, [Device]);
       _ -> {true}
    end,
    
    digest_response(Key, [{<<"timestamp">>, Timestamp}], ReqData, State).

