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
    {Digest, ValidDigest} = check_digest(wrq:get_req_header("X-Digest", ReqData)),
    {Event, ValidEvent} = check_event(wrq:path_info(event, ReqData)),

    {struct, JsonData} = mochijson2:decode(wrq:req_body(ReqData)),
    {Device, ValidDevice} = check_device(proplists:get_value(<<"device">>, JsonData)),

    State = #state{event = Event, device = Device, digest = Digest},

    {case {ValidVersion, ValidEvent, ValidDevice, ValidDigest} of
        {true, true, true, true} -> false;
        _ -> true
     end,
    ReqData, State}.


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


%
% Event message example:
%
% JSON: {"device":"01234567890123456789012345678901"}
% Mochijson2: {struct,[{<<"device">>,   "01234567890123456789012345678901"}]}
%
process_post(ReqData, #state{event = Event, device = Device} = State) ->
    {data, Result} = mysql:execute(pool, device_props, [Device]),
    [[Key, Upgrade, Resets]] = mysql:get_result_rows(Result),

    {struct, JsonData} = mochijson2:decode(wrq:req_body(ReqData)),

    Timestamp = unix_time(),

    mysql:execute(pool, event_insert, [Device, Event, Timestamp]),

    digest_response(Key, [{<<"timestamp">>, Timestamp}], ReqData, State).

