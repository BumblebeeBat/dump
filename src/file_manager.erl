-module(file_manager).
-behaviour(gen_server).
-export([start_link/4,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, write/3,read/3,write_ram/3]).
init({Name, Size, ram}) ->
    Z = case db:read(Name) of
	    "" -> hipe_bifs:bytearray(Size, 0);
	    X -> X
	end,
    {ok, {Z, Name, ram}};
init({Name, _, hd}) -> 
    {{ok, F}, _} = {file:open(Name, [write, read, raw, binary, sync]), Name},
    %{ok, F} = file:open(Name, [write, read, raw, binary]),
    {ok, {F, Name}}.
start_link(File, Id, Size, Mode) -> gen_server:start_link({global, Id}, ?MODULE, {File, Size, Mode}, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, {Bits, Name, ram}) -> 
    db:save(Name, Bits),
    io:format("file died!"), ok;
terminate(_, {F, _}) -> 
    file:close(F),
    io:format("file died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({write, Location, Data}, _From, {Bin, Name, ram}) -> 
    S = size(Data),
    BS = size(Bin),
    if
	Location+S > BS -> Bin;
	true -> 
	    file_manager:write_ram(Location, Data, Bin)
    end,
    {reply, ok, {Bin, Name, ram}};
handle_call({bad_write, Location, Data}, _From, {Bin, Name, ram}) -> 
    S = size(Data),
    BS = size(Bin),
    true = BS >= Location+S,
    spawn(file_manager, write_ram, [Location, Data, Bin]),
    {reply, ok, {Bin, Name, ram}};
handle_call({fast_write, Location, Data}, _From, {X, N}) -> 
    file:pwrite(X, Location, Data),
    {reply, ok, {X, N}};
handle_call({write, Location, Data}, _From, {X, N}) -> 
    file:pwrite(X, Location, Data),
    {reply, ok, {X, N}};
handle_call({read, Location, Amount}, _From, {X, Name, ram}) -> 
    A = if
	Location + Amount > size(X) ->
	    eof;
	true ->
		AA = Amount * 8,
		LL = Location * 8,
		<<_:LL, V:AA, _/bitstring>> = X,
		{ok, <<V:AA>>}
    end,
    {reply, A, {X, Name, ram}};
handle_call({read, Location, Amount}, _From, {X, N}) -> 
    {reply, file:pread(X, Location, Amount), {X, N}};
handle_call(_, _From, X) -> {reply, X, X}.
fast_write(ID, Location, Data) ->
    I = atom_to_list(ID),
    A = list_to_atom(I++"_file"),
    %gen_server:call({global, A}, {fast_write, Location, Data}).
    gen_server:call({global, A}, {write, Location, Data}).
write(ID, Location, Data) ->
    I = atom_to_list(ID),
    A = list_to_atom(I++"_file"),
    gen_server:call({global, A}, {write, Location, Data}).
read(ID, Location, Amount) ->
    I = atom_to_list(ID),
    A = list_to_atom(I++"_file"),
    gen_server:call({global, A}, {read, Location, Amount}).
%bytes(ID) ->
%    I = atom_to_list(ID),
%    A = list_to_atom(I++"_file"),
%    gen_server:call({global, A}, size).
%dgrow(ID) ->
%    S = bytes(ID),
%    write(ID, S, <<0:80000>>).
write_ram(_, <<>>, Bin) -> Bin;
write_ram(Location, <<D:8, Data/binary>>, Bin) -> 
    hipe_bifs:bytearray_update(Bin, Location, D),
    %spawn(hipe_bifs, bytearray_update, [Bin, Location, D]),
    write_ram(Location+1, Data, Bin).
