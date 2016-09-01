-module(dump_bits).
-behaviour(gen_server).
-export([start_link/1,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, write/1,top/1,delete/2,get/2]).
id2file(ID) -> atom_to_list(ID) ++ ".db".
init({ID}) -> 
    L = id2file(ID),
    Z = case db:read(L) of
	    "" ->
		K = <<0:160>>,
		db:save(L, K),
		K;
	    X -> X
	end,
    {Bits, Top} = top2(Z, 0),
    {ok, {Bits, Top, ID}}.
start_link(Id) -> gen_server:start_link({global, Id}, ?MODULE, {Id}, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, {Bits, _Top, ID}) -> 
    L = id2file(ID),
    db:save(L, Bits),
    io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({delete, Height}, {Bits, Top, ID}) -> 
    NewBits = flip_bit0(Bits, Height),
    {noreply, {NewBits, min(Top,Height), ID}};
handle_cast(write, {Bits, Top, ID}) -> 
    NewBits = flip_bit1(Bits, Top),
    {NewNewBits, NewTop} = top2(NewBits, Top),
    {noreply, {NewNewBits, NewTop, ID}};
handle_cast(_, X) -> {noreply, X}.
handle_call({get, N, ID}, _From, {Bits, Top, ID}) -> 
    G = get_internal(N, Bits),
    {reply, G, {Bits, Top, ID}};
handle_call(top, _From, {Bits, Top, File}) -> 
    {reply, Top, {Bits, Top, File}};
handle_call(_, _From, X) -> {reply, X, X}.
get_internal(Height, Bits) ->
    <<_:Height, X:1, _/bits>> = Bits,
    X.
get(ID, N) ->
    L = atom_to_list(ID),
    A2 = list_to_atom(L++"_bits"),
    gen_server:call({global, A2}, {get, N, A2}).
delete(ID, Height) -> 
    L = atom_to_list(ID),
    A2 = list_to_atom(L++"_bits"),
    gen_server:cast({global, A2}, {delete, Height}).
write(ID) -> 
    L = atom_to_list(ID),
    A2 = list_to_atom(L++"_bits"),
    gen_server:cast({global, A2}, write).
top(ID) -> 
    L = atom_to_list(ID),
    A2 = list_to_atom(L++"_bits"),
    gen_server:call({global, A2}, top).
top2(Bits, N) ->
    <<_:N, X/bitstring>> = Bits,
    {Many, T} = top3(X, N),
    {<<Bits/bitstring, 0:Many>>, T}.
top3(<<>>, N) ->
    {16000, N};
top3(<<0:1, _/bitstring>>, N) -> {0, N};
top3(<<4294967295:32, T/bitstring>>, N) -> 
    top3(T, N+32);
top3(<<65535:16, T/bitstring>>, N) -> top3(T, N+16);
top3(<<255:8, T/bitstring>>, N) -> top3(T, N+8);
top3(<<15:4, T/bitstring>>, N) -> top3(T, N+4);
top3(<<3:2, T/bitstring>>, N) -> top3(T, N+2);
top3(<<1:1, T/bitstring>>, N) -> top3(T, N+1).
flip_bit0(Bits, Number) ->
    <<A:Number, _:1, Y/bitstring>> = Bits,
    <<A:Number, 0:1, Y/bitstring>>.
flip_bit1(Bits, Number) ->
    <<A:Number, _:1, Y/bitstring>> = Bits,
    <<A:Number, 1:1, Y/bitstring>>.
