-module(bits).
-behaviour(gen_server).
-export([start_link/2,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, write/1,top/1,delete/2,get/2,test/0,empty_tuple/0]).
-define(Tuple, 10000).
-define(Bytes, 8000).

init({File}) ->
    Z = case db:read(File) of
	    "" -> empty_tuple();
	    X -> X
	end,
    {Bits, Top} = top2(Z, 0),
    {ok, {Bits, Top, File}}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
start_link(Id, File) -> gen_server:start_link({global, Id}, ?MODULE, {File}, []).
terminate(_, {Bits, _Top, File}) -> 
    db:save(File, Bits),
    io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({delete, Height}, {Bits, Top, File}) -> 
    NewBits = flip_bit(0, Bits, Height),
    {noreply, {NewBits, min(Top,Height), File}};
handle_cast(write, {Bits, Top, File}) -> 
    NewBits = flip_bit(1, Bits, Top),
    NewTop = top2(NewBits, Top+1),
    {noreply, {NewBits, NewTop, File}};
handle_cast(_, X) -> {noreply, X}.
handle_call({get, N}, _From, {Bits, Top, File}) -> 
    G = get_internal(N, Bits),
    {reply, G, {Bits, Top, File}};
handle_call(top, _From, {Bits, Top, File}) -> 
    {reply, Top, {Bits, Top, File}};
handle_call(_, _From, X) -> {reply, X, X}.
%get_internal(Height, Bits) ->
%    <<_:Height, X:1, _/bits>> = Bits,
%    X.
get(ID, N) ->
    L = atom_to_list(ID),
    A2 = list_to_atom(L++"_bits"),
    gen_server:call({global, A2}, {get, N}).
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
%top2(Bits, N) ->
%    <<_:N, X/bitstring>> = Bits,
%    {Many, T} = top3(X, N),
%    {<<Bits/bitstring, 0:Many>>, T}.
top3(<<>>, N) ->
    {more, N};
top3(<<0:1, _/bitstring>>, N) -> N;
top3(<<4294967295:32, T/bitstring>>, N) -> 
    top3(T, N+32);
top3(<<65535:16, T/bitstring>>, N) -> top3(T, N+16);
top3(<<255:8, T/bitstring>>, N) -> top3(T, N+8);
top3(<<15:4, T/bitstring>>, N) -> top3(T, N+4);
top3(<<3:2, T/bitstring>>, N) -> top3(T, N+2);
top3(<<1:1, T/bitstring>>, N) -> top3(T, N+1).
%flip_bit0(Bits, Number) ->
%    <<A:Number, _:1, Y/bitstring>> = Bits,
%    <<A:Number, 0:1, Y/bitstring>>.
%flip_bit1(Bits, Number) ->
%    <<A:Number, _:1, Y/bitstring>> = Bits,
%    <<A:Number, 1:1, Y/bitstring>>.
empty_bits() -> <<0:(?Bytes)>>.
empty_tuple() -> list_to_tuple(make_list(?Tuple, [])).
make_list(0, _) -> [];
make_list(N, X) -> [X|make_list(N-1, X)].
get_common(N, Bits) ->
    A = N rem ?Bytes,
    B = N div ?Bytes,
    C = B rem ?Tuple,
    D = B div ?Tuple,
    E = case element(D+1, Bits) of
	    [] -> empty_tuple();
	    T -> T
	end,
    F = case element(C+1, E) of
	    [] -> empty_bits();
	    K -> K
	end,
    {A, C, D, E, F}.
flip_bit(To, Bits, Number) ->
    {A,C,D,E,F} = get_common(Number, Bits), 
    <<G:A, _:1, H/bitstring>> = F,
    I = <<G:A, To:1, H/bitstring>>,
    J = setelement(C+1, E, I),
    setelement(D+1, Bits, J).
get_internal(N, Bits) ->
    {A,_,_,_,F} = get_common(N, Bits),
    <<_:A, X:1, _/bitstring>> = F,
    X.
top2(Bits, N) ->
    {A,_,_,_,F} = get_common(N, Bits),
    <<_:A, X/bitstring>> = F,
    case top3(X, 0) of
	{more, M} -> top2(Bits, N+M);
	P -> P+N
    end.
test() ->
    Bits = empty_tuple(),
    N = 50001,
    Bits2 = flip_bit(1, Bits, N),
    N = top2(Bits2, N)-1.
