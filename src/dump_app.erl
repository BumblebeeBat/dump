-module(dump_app).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, [ID, Size]) ->
    dump_sup:start_link(ID, Size).

stop(_State) ->
    ok.
