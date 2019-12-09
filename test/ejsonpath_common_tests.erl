-module(ejsonpath_common_tests).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

buildpath_test() ->
    ?assertEqual("$['xyz']", ejsonpath_common:buildpath("xyz", "$")),
    ?assertEqual("$['xyz']", ejsonpath_common:buildpath(<<"xyz">>, "$")),
    ?assertEqual("$['xyz']", ejsonpath_common:buildpath(xyz, "$")),
    ?assertEqual("$[0]", ejsonpath_common:buildpath(0, "$")),
    ok.

type_test() ->
    % array
    ?assertEqual(array, ejsonpath_common:type([])),
    ?assertEqual(array, ejsonpath_common:type([1,2,3])),
    ?assertEqual(array, ejsonpath_common:type("xyz")),
    
    % hash
    ?assertEqual(hash, ejsonpath_common:type(#{})),
    ?assertEqual(hash, ejsonpath_common:type(#{ <<"x">> => 1})),
    
    % string
    ?assertEqual(string, ejsonpath_common:type(<<>>)),
    ?assertEqual(string, ejsonpath_common:type(xyz)),
    ?assertEqual(string, ejsonpath_common:type(<<"xyz">>)),

    % number
    ?assertEqual(number, ejsonpath_common:type(0)),
    ?assertEqual(number, ejsonpath_common:type(0.0)),
    
    % boolean
    ?assertEqual(boolean, ejsonpath_common:type(true)),
    ?assertEqual(boolean, ejsonpath_common:type(false)),
    
    % null
    ?assertEqual(null, ejsonpath_common:type(null)),
    
    ok.

index_test() ->
    ?assertError(badarg, ejsonpath_common:index(0, -1)),
    ?assertError(badarg, ejsonpath_common:index(0, 0)),
    ?assertError(badarg, ejsonpath_common:index(1, 0)),
    ?assertError(badarg, ejsonpath_common:index(1, 1)),

    ?assertEqual(1, ejsonpath_common:index(0, 1)),
    ?assertEqual(5, ejsonpath_common:index(4, 10)),

    ok.

insert_list_test() ->
    ?assertError(badarg, ejsonpath_common:insert_list(-1, x, [a])),
    ?assertError(badarg, ejsonpath_common:insert_list(0, x, [a])),
    ?assertError(badarg, ejsonpath_common:insert_list(2, x, [a])),

    ?assertEqual([x], ejsonpath_common:insert_list(1, x, [a])),

    ?assertEqual([x,y,z], ejsonpath_common:insert_list(1, x, [a,y,z])),
    ?assertEqual([x,y,z], ejsonpath_common:insert_list(2, y, [x,a,z])),
    ?assertEqual([x,y,z], ejsonpath_common:insert_list(3, z, [x,y,a])),
    
    ok.