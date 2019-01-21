-module(dump_sup).
-behaviour(supervisor).
-export([start_link/4,init/1,stop/0]).
start_link(ID, Size, Amount, Mode) -> %Mode is ram or hd
    L = atom_to_list(ID),
    A = list_to_atom(L ++ "sup"),
    supervisor:start_link({global, A}, ?MODULE, [ID, Size, Amount, Mode, ""]).
stop() -> halt().

init([ID, Size, Amount, Mode, Location]) ->
    L = atom_to_list(ID),
    L1 = Location ++ "data/"++L++".db",
    A1 = ID,
    A2 = dump_ids:bits(ID),
    L2 = Location ++ "data/"++L++"_bits.db",
    A3 = dump_ids:file_manager(ID),
    Children = [{A1, {dump, start_link, [Size, A1]}, permanent, 50000, worker, [dump]},
		{A3, {file_manager, start_link, [L1, A3, Size*Amount, Mode]}, permanent, 5000, worker, [file_manager]},
		{A2, {bits, start_link, [A2, L2, Amount]}, permanent, 5000, worker, [bits]}
	       ], 
    {ok, { {one_for_one, 5, 10}, Children} }.

