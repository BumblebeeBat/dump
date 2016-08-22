-module(dump_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(normal, [ID]) ->
    io:fwrite("ok\n"),
    Size = 5,
    dump_sup:start_link(ID, Size).

stop(_State) ->
    ok.
