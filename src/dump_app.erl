-module(dump_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    %Amount = 1000000,
    Amount = 1000,
    dump_sup:start_link(dump, 32, Amount, hd).

stop(_State) ->
    ok.
