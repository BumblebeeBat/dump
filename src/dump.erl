-module(dump).
-behaviour(gen_server).
-export([start_link/2,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2, delete/2,put/2,get/2,word/1,highest/1,update/3]).
init({WordSize}) -> {ok, {WordSize}}.
start_link(WordSize, Id) -> gen_server:start_link({global, Id}, ?MODULE, {WordSize}, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, _) -> io:format("died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(_, X) -> {noreply, X}.
handle_call({delete, Location, Id}, _From, X) ->
    bits:delete(Id, Location),
    {reply, ok, X};
handle_call({fast_write, Data, ID}, _From, X) ->
    {Word} = X,
    Word = size(Data),
    Top = bits:top(ID),
    file_manager:fast_write(ID, Top*Word, Data),
    bits:write(ID),
    {reply, Top, X};
handle_call({update, Location, Data, ID}, _From, X) ->
    {Word} = X,
    Word = size(Data),
    file_manager:write(ID, Location*Word, Data),
    {reply, ok, X};
handle_call({write, Data, ID}, _From, X) ->
    {Word} = X,
    Word = size(Data),
    Top = bits:top(ID),
    file_manager:write(ID, Top*Word, Data),
    bits:write(ID),
    {reply, Top, X};
handle_call({read, Location, ID}, _From, X) ->
    {Word} = X,
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
handle_call(word, _From, X) ->
    {Word} = X,
    {reply, Word, X};
handle_call({highest, ID}, _From, X) ->
    {Word} = X,
    A = bits:highest(ID),
    {reply, A*Word, X}.
delete(X, ID) -> gen_server:call({global, ID}, {delete, X, ID}).
fast_put(Data, ID) -> 
    gen_server:call({global, ID}, {fast_write, Data, ID}).
update(Location, Data, ID) -> 
    gen_server:call({global, ID}, {update, Location, Data, ID}).
put(Data, ID) -> 
    gen_server:call({global, ID}, {write, Data, ID}).
get(X, ID) -> 
    true = X > 0,
    gen_server:call({global, ID}, {read, X, ID}).
word(ID) -> gen_server:call({global, ID}, word).
highest(ID) -> gen_server:call({global, ID}, {highest, ID}).

