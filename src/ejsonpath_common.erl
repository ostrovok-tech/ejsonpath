-module(ejsonpath_common).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-export([
    buildpath/2,
    insert_list/3,
    index/2,
    type/1
]).


buildpath(Key, Path) when is_binary(Key) ; is_list(Key) ; is_atom(Key) ->
    lists:flatten(io_lib:format("~s['~s']", [Path, Key]));
buildpath(Idx, Path) when is_number(Idx) ->
    lists:flatten(io_lib:format("~s[~p]", [Path, Idx])).

insert_list(Idx, Value, List) when Idx > 0, Idx =< length(List) ->
    insert_list_i(Idx, Value, List);
insert_list(_, _, _) ->
    erlang:error(badarg).

insert_list_i(Idx, Value, List) ->
    lists:sublist(List, Idx-1) ++ 
    [ Value ] ++ 
    lists:nthtail(Idx, List).
index(N, Sz) when N >= 0 ->
    index_i(N+1, Sz);
index(N, Sz) ->
    index_i(Sz+N, Sz).
index_i(N, Sz) when N > 0, N =< Sz ->
    N;
index_i(_,_) ->
    erlang:error(badarg).

type(L) when is_list(L) ->
    array;
type(Map) when is_map(Map) ->
    hash;
type(Bin) when is_binary(Bin) ->
    string;
type(Bool) when is_boolean(Bool) ->
    boolean;
type(Num) when is_number(Num) ->
    number;
type(null) ->
    null;
type(Atom) when is_atom(Atom) ->
    string.


