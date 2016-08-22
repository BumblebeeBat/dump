-module(test).
-export([test/0]).
test() ->
    dump_sup:start_link(kv, 5),
    V1 = <<5,4,3,2,1>>,
    V2 = <<5,4,3,2,1>>,
    V3 = <<5,4,3,2,1>>,
    A1 = dump:put(V1, kv),
    V1 = dump:get(A1, kv),
    A2 = dump:put(V2, kv),
    V1 = dump:get(A1, kv),
    V2 = dump:get(A2, kv),
    A3 = dump:put(V3, kv),
    V1 = dump:get(A1, kv),
    V2 = dump:get(A2, kv),
    V3 = dump:get(A3, kv),
    dump:delete(A2, kv),
    A2 = dump:put(V1, kv),
    test_times(10000).
test_times(0) -> success;
test_times(N) -> 
    dump:put(<<0,0,0,0,0>>, kv),
    test_times(N-1).
