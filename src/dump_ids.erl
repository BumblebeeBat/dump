-module(dump_ids).
-export([bits/1, file_manager/1]).

bits(ID) ->
    A = atom_to_list(ID),
    list_to_atom(A ++ "_bits").

file_manager(ID) ->
    A = atom_to_list(ID),
    list_to_atom(A ++ "_file").
    
