-module(test_lists).
-export([test/2]).

test(X, D) ->
    L = list_to_tuple(make_list(X, D)),
    {L, timer:tc(erlang, element, [X-1, L])}.%lists:nth(X-1, L).

make_list(0, _) -> [];
make_list(N, D) -> [D|make_list(N-1, D)].

