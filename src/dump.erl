-module(dump).
-behaviour(gen_server).
-export([start_link/4,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, delete/2,put/2,get/2,word/1,highest/1,update/3, put_batch/2, mode/1,
         reload_ets/1,%only in ram mode,
        quick_save/1%only works in ram mode.
        ]).
init({Mode, WordSize, ID, Loc}) -> 
    process_flag(trap_exit, true),
    W = case Mode of
            ram -> 
                load_ets(ID, Loc),
                case db:read(loc2rest(Loc)) of
                    "" -> 1;
                    X -> 
                        {Y} = binary_to_term(X),
                        Y
                end;
            hd -> WordSize
        end,
    %io:fwrite("start dump0\n"),
    %io:fwrite(integer_to_list(W)),
    %io:fwrite("start dump1\n"),
    %io:fwrite("\n"),
    {ok, {Mode, W, ID, Loc}}.
start_link(WordSize, Id, Mode, Loc) -> 
    X = case Mode of
             ram -> {ram, 1, Id, Loc};
             hd -> {hd, WordSize, Id, Loc}
         end,
    gen_server:start_link({global, Id}, ?MODULE, X, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
loc2rest(Loc) ->
    {F, _} = lists:split(length(Loc) - 3, Loc),
    Loc2 = F ++ "_rest.db".
save_table(ID, Loc) ->
    %io:fwrite("trying to save table "),
    %io:fwrite(ID),
    %io:fwrite("\n"),
    case ets:tab2file(ID, Loc, [{sync, true}]) of
    %case ets:tab2file(ID, Loc) of
        ok -> ok;
        {error, R} ->
            %io:fwrite(R),
            %timer:sleep(200),
            save_table(ID, Loc)
    end.
%terminate(_, {ram, Max, ID, Loc}) -> 
    %Loc2 = loc2rest(Loc),
    %db:save(Loc2, term_to_binary({Max})),
    %save_table(ID, Loc),
    %ets:tab2file(ID, Loc, [{sync, true}]),
    %io:fwrite(Loc),
    %io:fwrite("\n"),
%    io:format("ram dump died\n"), 
%    ok;
terminate(_, {ram, Top, ID, Loc}) -> 
    Loc2 = loc2rest(Loc),
    db:save(Loc2, term_to_binary({Top})),
    save_table(ID, Loc),
    ok;
terminate(_, _) -> 
    %io:format("dump died!\n"), 
    ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(reload_ets, {ram, Top, ID, Loc}) -> 
    ets:delete(ID),
    load_ets(ID, Loc),
    {noreply, {ram, Top, ID, Loc}};
handle_cast(quick_save, {ram, Top, ID, Loc}) -> 
    Loc2 = loc2rest(Loc),
    db:save(Loc2, term_to_binary({Top})),
    save_table(ID, Loc),
    {noreply, {ram, Top, ID, Loc}};
handle_cast(_, X) -> {noreply, X}.
%handle_call(_, _, []) -> 
%    {reply, {error, off}, []};
%handle_call(off, _, X = {ram, Top, ID, Loc}) -> 
%    Loc2 = loc2rest(Loc),
%    db:save(Loc2, term_to_binary({Top})),
%    save_table(ID, Loc),
%    io:format("ram dump saved\n"), 
%    {reply, ok, []};
handle_call(mode, _From, X = {Y, _, _, _}) ->
    {reply, Y, X};
handle_call({delete, Location, _Id}, _From, X = {hd, _, Id, _}) ->
    bits:delete(Id, Location),
    {reply, ok, X};
handle_call({write_batch, L, _ID}, _, {ram, Top, ID, Loc}) ->
    %ets:insert(ID, {Top, Data}),
    ets:insert(ID, L),
    Top2 = max_second(L, Top),
    {reply, ok, {ram, Top2+1, ID, Loc}};
handle_call({delete, Location, _}, _From, X = {ram, _, Id, _}) ->
    ets:delete(Id, Location),
    {reply, ok, X};
handle_call({update, Location, Data, _ID}, _From, X = {ram, _, ID, _}) ->
    ets:insert(ID, [{Location, Data}]),
    {reply, ok, X};
handle_call({update, Location, Data, _ID}, _From, X = {hd, _, ID, _}) ->
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
handle_call({write, Data, _ID}, _From, {ram, Top, ID, Loc}) ->
    ets:insert(ID, {Top, Data}),
    {reply, Top, {ram, Top+1, ID, Loc}};
handle_call({write, Data, _ID}, _From, X = {hd, Word, ID, Loc}) ->
    Word = size(Data),
    Top = bits:top(ID),
    file_manager:write(ID, Top*Word, Data),
    bits:write(ID),
    {reply, Top, X};
handle_call({read, Location, _ID}, _From, X = {ram, _, ID, Loc}) ->
    Y = case ets:lookup(ID, Location) of
            [] -> empty;
            Z -> element(2, hd(Z))
        end,
    {reply, Y, X};
handle_call({read, Location, _ID}, _From, X = {hd, Word, ID, Loc}) ->
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
handle_call(word, _From, X = {ram, _, _, _}) ->
    {reply, 0, X};
handle_call(word, _From, X = {hd, Word, _, _}) ->
    {reply, Word, X};
handle_call({highest, _ID}, _From, X = {ram, Top, _, _}) ->
    {reply, Top, X};
handle_call({highest, ID}, _From, X = {hd, Word, _, _}) ->
    A = bits:highest(ID),
    {reply, A*Word, X};
handle_call(off, _, X) -> {reply, ok, X};
handle_call(Other, _, X) ->
    io:fwrite("dump cannot handle that command\n"),
    io:fwrite("\n"),
    %io:fwrite({Other, X}),
    {reply, ok, X}.


max_second([], X) -> X;
max_second([{L, D}|T], X) ->
    max_second(T, max(X, L)).
load_ets(ID, Loc) ->
    case ets:info(ID) of
        undefined ->
            case ets:file2tab(Loc) of
                {ok, ID} -> ok;
                {error, R} ->
                    %io:fwrite(R),
                    %io:fwrite("make table "),
                    %io:fwrite(ID),
                    %io:fwrite("\n"),
                    ets:new(ID, [set, named_table, {write_concurrency, false}, compressed])
            end;
        _ -> ok
    end.
    


quick_save(ID) ->
    gen_server:cast({global, ID}, quick_save).
reload_ets(ID) ->
    gen_server:cast({global, ID}, reload_ets).
off(ID) -> gen_server:call({global, ID}, off).
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
mode(ID) -> gen_server:call({global, ID}, mode).
