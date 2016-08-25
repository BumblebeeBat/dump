-module(file_manager).
-behaviour(gen_server).
-export([start_link/2,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, write/3,read/3,size/1,grow/1]).
init({File}) -> 
    {ok, F} = file:open(File, [write, read, raw, binary]),
    {ok, F}.
start_link(File, Id) -> gen_server:start_link({global, Id}, ?MODULE, {File}, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, F) -> 
    file:close(F),
    io:format("file died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({write, Location, Data}, X) -> 
    file:pwrite(X, Location, Data),
    {noreply, X};
handle_cast(_, X) -> {noreply, X}.
handle_call(size, _From, X) -> 
    H = filelib:file_size(X),
    {reply, H, X};
handle_call({read, Location, Amount}, _From, X) -> 
    {reply, file:pread(X, Location, Amount), X};
handle_call(_, _From, X) -> {reply, X, X}.

write(ID, Location, Data) ->
    I = atom_to_list(ID),
    A = list_to_atom(I++"_file"),
    gen_server:cast({global, A}, {write, Location, Data}).
read(ID, Location, Amount) ->
    I = atom_to_list(ID),
    A = list_to_atom(I++"_file"),
    gen_server:call({global, A}, {read, Location, Amount}).
size(ID) ->
    I = atom_to_list(ID),
    A = list_to_atom(I++"_file"),
    gen_server:call({global, A}, size).
grow(ID) ->
    S = file_manager:size(ID),
    write(ID, S, <<0:80000>>).
