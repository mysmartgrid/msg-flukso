%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(flukso).
-author('author <author@example.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

mysql_prepare() ->
    mysql:prepare(permissions, <<"SELECT permissions FROM logger_tokens WHERE meter = ? AND token = ?">>).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    flukso_deps:ensure(),
    ensure_started(crypto),
    ensure_started(erlrrd),
    ensure_started(mysql),
    mysql_prepare(),
    ensure_started(webmachine),
    flukso_sup:start_link().

%% @spec start() -> ok
%% @doc Start the flukso server.
start() ->
    flukso_deps:ensure(),
    ensure_started(crypto),
    ensure_started(erlrrd),
    ensure_started(mysql),
    mysql_prepare(),
    ensure_started(webmachine),
    application:start(flukso).

%% @spec stop() -> ok
%% @doc Stop the flukso server.
stop() -> 
    Res = application:stop(flukso),
    application:stop(webmachine),
    application:stop(mysql),
    application:stop(erlrrd),
    application:stop(crypto),
    Res.
