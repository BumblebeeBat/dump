-module(test_dump).
-export([test/0, test2/0, test3/0, test4/0, test_main/2, testlta/0]).
%timer:tc(test_dump, test, []).
%{5457078,success}
test_init(ID, Size) ->
    dump_sup:start_link(ID, Size, 100002, ram, "").

testlta() ->
    %41077 = 0.04 seconds
    X = abcdefghijk,
    testlta2(100000, X).
testlta2(0, _) -> ok;
testlta2(N, X) -> 
    _ = list_to_atom(atom_to_list(X)),
    testlta2(N-1, X).
test() ->
    ID = kv2,
    Size = 100,
    test_init(ID, Size),
    timer:tc(test_dump, test_main, [ID, Size]).
test_main(ID, Size) ->
    V1 = <<1:(8*Size)>>,
    V2 = <<2:(8*Size)>>,
    V3 = <<3:(8*Size)>>,
    A1 = 1,
    A2 = 2,
    A3 = 3,
    A1 = dump:put(V1, ID),
    V1 = dump:get(A1, ID),
    A2 = dump:put(V2, ID),
    V1 = dump:get(A1, ID),
    V2 = dump:get(A2, ID),
    A3 = dump:put(V3, ID),
    V1 = dump:get(A1, ID),
    V2 = dump:get(A2, ID),
    V3 = dump:get(A3, ID),
    dump:delete(A1, ID),
    A1 = dump:put(V2, ID),
    Times = 1000,
    put_times(Times, Size, ID),
    get_times(Times, Size, ID),
    V2 = dump:get(A1, ID),
    dump:update(A1, V1, ID),
    V1 = dump:get(A1, ID).
    
   
put_times(0, _, _) -> success;
put_times(N, Size, ID) -> 
    dump:put(<<1:(8*Size)>>, ID),
    put_times(N-1, Size, ID).
get_times(3, _, _) -> success;
get_times(N, Size, ID) ->
    SS = 8*Size, 
    <<1:SS>> = dump:get(N, ID),
    get_times(N-1, Size, ID).

test2() ->
    ID = test2,
    Size = 100,
    test_init(ID, Size),
    cprof:start(),
    test_dump:test_main(ID, Size),
    cprof:pause(),
    X = cprof:analyse(),
    cprof:stop(),
    X.

test3() ->
    ID = test3,
    Size = 100,
    test_init(ID, Size),
    fprof:apply(test_dump, test_main, [ID, Size]),
    fprof:profile(),
    fprof:analyse().

test4() ->
    ID = test4,
    Size = 100,
    {ok, _X} = test_init(ID, Size),
    eprof:start(),
    %eprof:start_profiling([self()]),
    eprof:start_profiling([dump_ids:bits(ID), ID, dump_ids:file_manager(ID)]),
    test_main(ID, Size),
    eprof:stop_profiling(),
    eprof:analyze(total).
