-module(test_dump).
-export([test/0]).
%timer:tc(test_dump, test, []).
%{5457078,success}


test() ->
    ID = kv2,
    Size = 100,
    dump_sup:start_link(ID, Size),
    V1 = <<0:(8*Size)>>,
    V2 = <<2:(8*Size)>>,
    V3 = <<3:(8*Size)>>,
    A1 = dump:put(V1, ID),
    V1 = dump:get(A1, ID),
    A2 = dump:put(V2, ID),
    V1 = dump:get(A1, ID),
    V2 = dump:get(A2, ID),
    A3 = dump:put(V3, ID),
    V1 = dump:get(A1, ID),
    V2 = dump:get(A2, ID),
    V3 = dump:get(A3, ID),
    dump:delete(A2, ID),
    A2 = dump:put(V1, ID),
    Times = 100000,
    test_times(Times, Size, ID).
test_times(0, _, _) -> success;
test_times(N, Size, ID) -> 
    dump:put(<<0:(8*Size)>>, ID),
    test_times(N-1, Size, ID).
