-module(dump).
-behaviour(gen_server).
-export([start_link/3,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, delete/2,put/2,get/2,word/1,highest/1]).
init({File, WordSize}) -> {ok, {File, WordSize}}.
start_link(File, WordSize, Id) -> gen_server:start_link({global, Id}, ?MODULE, {File, WordSize}, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({delete, Location, Id}, X) ->
    dump_bits:delete(Id, Location),
    {noreply, X};
handle_cast({update, Location, Data}, X) ->
    {File, Word} = X,
    Word = size(Data),
    {ok, F} = file:open(File, [write, read, raw, binary]),
    file:pwrite(F, Location*Word, Data),
    file:close(F),
    {noreply, X};
handle_cast(_, X) -> {noreply, X}.
handle_call({write, Data, Id}, _From, X) ->
    {File, Word} = X,
    Word = size(Data),
    {ok, F} = file:open(File, [write, read, raw, binary]),%need read and write
    Top = dump_bits:top(Id),
    file:pwrite(F, Top*Word, Data),
    file:close(F),
    dump_bits:write(Id),
    {reply, Top, X};
handle_call({read, Location}, _From, X) ->
    {File, Word} = X,
    {ok, F} = file:open(File, [read, raw, binary]),
    Z  = case file:pread(F, Location*Word, Word) of
	     eof -> {error, eof};
	     {ok, Y} -> Y
	 end,
    file:close(F),
    {reply, Z, X};
handle_call(word, _From, X) ->
    {_File, Word} = X,
    {reply, Word, X};
handle_call(highest, _From, X) ->
    {File, Word} = X,
    H = filelib:file_size(File),
    {reply, H div Word, X};
handle_call(_, _From, X) -> {reply, X, X}.

delete(X, ID) -> 
    L = atom_to_list(ID),
    %A1 = list_to_atom(L++"_db"),
    A2 = list_to_atom(L++"_bits"),
    gen_server:cast({global, ID}, {delete, X, A2}).
put(Data, ID) -> 
    L = atom_to_list(ID),
    %A1 = list_to_atom(L++"_db"),
    A2 = list_to_atom(L++"_bits"),
    gen_server:call({global, ID}, {write, Data, A2}).
get(X, ID) -> 
    %L = atom_to_list(ID),
    %A1 = list_to_atom(L++"_db"),
    gen_server:call({global, ID}, {read, X}).
word(ID) -> 
    %L = atom_to_list(ID),
    %A1 = list_to_atom(L++"_db"),
    gen_server:call({global, ID}, word).
highest(ID) -> 
    %L = atom_to_list(ID),
    %A1 = list_to_atom(L++"_db"),
    gen_server:call({global, ID}, highest).
