-module(dump).
-behaviour(gen_server).
-export([start_link/4,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, delete/2,put/2,get/2,word/1,highest/1,update/3, put_batch/2]).
init({Mode, WordSize, ID, Loc}) -> 
    process_flag(trap_exit, true),
    case Mode of
        ram -> 
            case ets:file2tab(Loc) of
                {ok, ID} -> ok;
                {error, _} ->
                    io:fwrite("make table "),
                    io:fwrite(ID),
                    io:fwrite("\n"),
                    ets:new(ID, [set, named_table, {write_concurrency, false}, compressed])
            end;
        hd -> ok
    end,
    {ok, {Mode, WordSize, ID, Loc}}.
start_link(WordSize, Id, Mode, Loc) -> 
    X = case Mode of
             ram -> {ram, 1, Id, Loc};
             hd -> {hd, WordSize, Id, Loc}
         end,
    gen_server:start_link({global, Id}, ?MODULE, X, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, {_, _, ID, Loc}) -> 
    ets:tab2file(ID, Loc),
    io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({delete, Location, Id}, _From, X = {hd, _}) ->
    bits:delete(Id, Location),
    {reply, ok, X};
handle_call({delete, Location, Id}, _From, X = {ram, _}) ->
    ets:delete(Id, Location),
    {reply, ok, X};
handle_call({update, Location, Data, ID}, _From, X = {ram, _}) ->
    ets:insert(ID, [{Location, Data}]),
    {reply, ok, X};
handle_call({update, Location, Data, ID}, _From, X = {hd, _}) ->
    %{Word} = X,
    Word = size(Data),
    file_manager:write(ID, Location*Word, Data),
    {reply, ok, X};
%handle_call({fast_write, Data, ID}, _From, X = {hd, Word}) ->
%    Word = size(Data),
%    Top = bits:top(ID),
%    file_manager:fast_write(ID, Top*Word, Data),
%    bits:write(ID),
%    {reply, Top, X};
handle_call({write, Data, ID}, _From, {ram, Top}) ->
    ets:insert(ID, {Top, Data}),
    {reply, Top, {ram, Top+1}};
handle_call({write_batch, L, ID}, _From, {ram, Top}) ->
    %ets:insert(ID, {Top, Data}),
    ets:insert(ID, L),
    Top2 = max_second(L, Top),
    {reply, Top, {ram, Top2+1}};
handle_call({write, Data, ID}, _From, X = {hd, Word}) ->
    Word = size(Data),
    Top = bits:top(ID),
    file_manager:write(ID, Top*Word, Data),
    bits:write(ID),
    {reply, Top, X};
handle_call({read, Location, ID}, _From, X = {ram, _}) ->
    Y = case ets:lookup(ID, Location) of
            [] -> empty;
            Z -> element(2, hd(Z))
        end,
    {reply, Y, X};
handle_call({read, Location, ID}, _From, X = {hd, Word}) ->
    Z = case file_manager:read(ID, Location*Word, Word) of
	    {ok, A} -> A;
	    eof -> 
		<<0:(Word*8)>>
	end,

    %Z = case bits:get(ID, Location) of
    %        true ->
    %            case file_manager:read(ID, Location*Word, Word) of
    %                {ok, A} -> A;
    %                eof -> <<0:(Word*8)>>
    %            end;
    %        false ->
    %            <<0:(Word*8)>>
    %    end,
    {reply, Z, X};
handle_call(word, _From, X = {ram, _}) ->
    {reply, 0, X};
handle_call(word, _From, X = {hd, Word}) ->
    {reply, Word, X};
handle_call({highest, _ID}, _From, X = {ram, Top}) ->
    {reply, Top, X};
handle_call({highest, ID}, _From, X = {hd, Word}) ->
    A = bits:highest(ID),
    {reply, A*Word, X}.


max_second([], X) -> X;
max_second([{L, D}|T], X) ->
    max_second(T, max(X, L)).



delete(X, ID) -> gen_server:call({global, ID}, {delete, X, ID}).
fast_put(Data, ID) -> 
    %gen_server:call({global, ID}, {fast_write, Data, ID}).
    gen_server:call({global, ID}, {write, Data, ID}).
update(Location, Data, ID) -> 
    gen_server:call({global, ID}, {update, Location, Data, ID}).
put(Data, ID) -> 
    gen_server:call({global, ID}, {write, Data, ID}).
put_batch(L, ID) -> 
    gen_server:call({global, ID}, {write_batch, L, ID}).
get(X, ID) -> 
    true = X > 0,
    gen_server:call({global, ID}, {read, X, ID}).
word(ID) -> gen_server:call({global, ID}, word).
highest(ID) -> gen_server:call({global, ID}, {highest, ID}).
