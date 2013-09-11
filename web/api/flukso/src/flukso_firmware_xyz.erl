%%
%% The /firmware/xyz resource implementation.
%%
%% Copyright (c) 2011 Fraunhofer Institut ITWM (www.itwm.fraunhofer.de)
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

-module(flukso_firmware_xyz).
-author('Ely de Oliveira <ely.oliveira@itwm.fraunhofer.de>').

-export([init/1,
         allowed_methods/2,
         malformed_request/2,
         is_authorized/2,
         content_types_provided/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("flukso.hrl").


%init([]) -> 
%    {ok, undefined}.


%Logging
init(Config) ->
   {{trace, "/var/log/erlang/flukso-api/trace"}, Config}.


allowed_methods(ReqData, State) ->
    {['GET'], ReqData, State}.


malformed_request(ReqData, State) ->
    case wrq:method(ReqData) of
        'GET'  -> malformed_GET(ReqData, State)
    end.


malformed_GET(ReqData, _State) ->

    Return = case check_version(wrq:get_req_header("X-Version", ReqData)) of
      {Version, true} ->

        case check_device(wrq:path_info(firmware, ReqData)) of
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
        'GET'  -> is_auth_GET(ReqData, State)
    end.


is_auth_GET(ReqData, #state{device = Device, digest = ClientDigest} = State) ->

    {data, Result} = mysql:execute(pool, device_key, [Device]),

    {case mysql:get_result_rows(Result) of

      [[Key]] -> check_digest(Key, ReqData, ClientDigest);
      _ -> false
    end, ReqData, State}.


content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.


to_json(ReqData, #state{device = Device, digest = ClientDigest} = State) ->

    {data, Result} = mysql:execute(pool, device_firmware, [Device]),

    case mysql:get_result_rows(Result) of

      [[Key, Upgrade, CurrentVersion, false]] ->
        {{halt, ?HTTP_NON_UPGRADABLE_FIRMWARE}, ReqData, State};

      [[Key, Upgrade, CurrentVersion, true]] ->

        if
          Upgrade > 0 ->

            Path = string:concat(?FIRMWARE_UPGRADES_PATH, "archives/"),
            Args = string:concat(Device, string:concat(" ", erlang:binary_to_list(CurrentVersion))),
            os:cmd(string:concat(string:concat(Path, "create-archive.sh "), Args)),
            FilePath = string:concat(Path, Device),
            
            case file:read_file(FilePath) of

              {ok, File} ->
                file:delete(FilePath),
                Answer = [{<<"data">>, base64:encode(File)}],

                mysql:execute(pool, device_upgrade_update, [0, Device]),

                digest_response(Key, Answer, ReqData, State, false);

              _ ->
                {{halt, ?HTTP_FORBIDDEN}, ReqData, State}
            end;

          true ->
            {{halt, ?HTTP_BAD_ARGUMENT}, ReqData, State}
        end;

      _ ->
        %Internal Server Error (logical error, that must never occur)
        {{halt, ?HTTP_INTERNAL_SERVER_ERROR}, ReqData, State}
    end.
