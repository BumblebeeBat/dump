-module(key_value_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(normal, []) ->
    io:fwrite("ok\n"),
    Size = 5,
    key_value_sup:start_link(kv, Size).

stop(_State) ->
    ok.
