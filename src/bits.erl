-module(bits).
-behaviour(gen_server).
-export([start_link/3,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, write/1,top/1,highest/1,delete/2,get/2]).
init({File, Size}) ->
    process_flag(trap_exit, true),
    {Z, Top, Highest} = 
	case db:read(File) of
	    "" -> 
		X = hipe_bifs:bitarray(Size, false),
		{X, 1, 1};
	    X ->  {X, top2(X, 1), highest2(X, Size-1)}
	end,
    {ok, {Z, Top, Highest, File, Size}}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
start_link(Id, File, Size) -> gen_server:start_link({global, Id}, ?MODULE, {File, Size}, []).
terminate(_, {Bits, _Top, _Highest, File, _Size}) -> 
    db:save(File, Bits),
    %io:format("died!"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({delete, Height}, _From, {Bits, Top, Highest, File, Size})-> 
    NewBits = hipe_bifs:bitarray_update(Bits, Height, false),
    NewHighest = highest2(NewBits, Highest),
    {reply, ok, {NewBits, min(Top,Height), NewHighest, File, Size}};
handle_call(write, _From, {Bits, Top, H, File, Size}) -> 
    NewBits = hipe_bifs:bitarray_update(Bits, Top, true),
    NewTop = top2(NewBits, Top+1),
    {reply, ok, {NewBits, NewTop, max(H, Top), File, Size}};
handle_call({get, N}, _From, {Bits, Top, H, File, Size}) -> 
    G = internal_get(Bits, N),
    {reply, G, {Bits, Top, H, File, Size}};
handle_call(top, _From, {Bits, Top, H, File, Size}) -> 
    {reply, Top, {Bits, Top, H, File, Size}};
handle_call(highest, _From, {Bits, Top, Highest, File, Size}) -> 
    {reply, Highest, {Bits, Top, Highest, File, Size}};
handle_call(_, _From, X) -> {reply, X, X}.
ider(ID) -> list_to_atom(atom_to_list(ID)++"_bits").
get(ID, N) -> gen_server:call({global, ider(ID)}, {get, N}).
delete(ID, Height) -> 
    gen_server:call({global, ider(ID)}, {delete, Height}).
write(ID) -> 
    gen_server:call({global, ider(ID)}, write).
    
highest(ID) -> gen_server:call({global, ider(ID)}, highest).
top(ID) -> gen_server:call({global, ider(ID)}, top).
top2(Bits, N) ->
    B = hipe_bifs:bitarray_sub(Bits, N),
    if
	B -> top2(Bits, N+1);
	true -> N
    end.
internal_get(Bits, N) ->
    hipe_bifs:bitarray_sub(Bits, N).
    
highest2(_, 1) -> 1;
highest2(Bits, N) -> 
    case internal_get(Bits, N) of
	true -> N;	    
	false -> highest2(Bits, N-1)
    end.
