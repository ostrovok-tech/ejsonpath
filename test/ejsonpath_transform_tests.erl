-module(ejsonpath_transform_tests).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(ast(Query), 
    element(2,
        ejsonpath_parse:parse(
            element(2, 
                ejsonpath_scan:string(Query)
            )
        )
    )
).

get_doc() ->
    {ok, Bin} = file:read_file("./test/doc.json"),
    jsx:decode(Bin, [return_maps]).

root_test() ->
    ?assertEqual({ok, ["$"]}, 
        ejsonpath_transform:transform(?ast("$"), get_doc(), fun (_) -> ok end, #{}, [])),
    
    ?assertError(not_implemented, 
        ejsonpath_transform:transform(?ast("$."), get_doc(), fun (_) -> ok end, #{}, [])),
    ?assertError(not_implemented, 
        ejsonpath_transform:transform(?ast("$.."), get_doc(), fun (_) -> ok end, #{}, [])),        

    ok.

key_access_test() ->
    ?assertEqual({#{<<"a">> => y},  ["$['a']"]}, 
        ejsonpath_transform:transform(?ast("$.a"), #{<<"a">> => x}, fun (x) -> y end, #{}, [])),

    ?assertError(not_found, 
        ejsonpath_transform:transform(?ast("$.b"), #{<<"a">> => x}, fun (x) -> y end, #{}, [])),

    ?assertEqual({#{<<"a">> => #{<<"a">> => y}},  ["$['a']['a']"]}, 
        ejsonpath_transform:transform(?ast("$.a.a"), #{<<"a">> => #{<<"a">> => x}}, fun (x) -> y end, #{}, [])),
    ok.

index_access_test() ->
    ?assertEqual({#{<<"a">> => [y, y, z]},  ["$['a'][0]"]}, 
        ejsonpath_transform:transform(?ast("$.a[0]"), #{<<"a">> => [x, y, z]}, fun (x) -> y end, #{}, [])),

    ?assertError(badarg,
        ejsonpath_transform:transform(?ast("$.a[3]"), #{<<"a">> => [x, y, z]}, fun (_) -> y end, #{}, [])),
    ok.
access_list_test() ->
    ?assertEqual(
        { #{<<"a">> => xxx, <<"b">> => xxx, <<"c">> => yyy},  ["$['a']", "$['b']"]}, 
        ejsonpath_transform:transform(?ast("$['a', 'b']"), #{<<"a">> => yyy, <<"b">> => yyy, <<"c">> => yyy}, fun (_) -> xxx end, #{}, [])),

    ?assertError(not_found,
        ejsonpath_transform:transform(?ast("$['a', 'd']"), #{<<"a">> => yyy, <<"b">> => yyy, <<"c">> => yyy}, fun (_) -> xxx end, #{}, [])),
    ok.

multimatch_test() ->
    O = #{
        <<"items">> => [
            #{<<"id">> => 0, <<"value">> => yyy},
            #{<<"id">> => 1, <<"value">> => yyy}
        ]
    },
    ?assertEqual(
        { #{
            <<"items">> => [
            #{<<"id">> => 0, <<"value">> => xxx},
            #{<<"id">> => 1, <<"value">> => xxx}
        ]
        },  
          ["$['items'][0]['value']","$['items'][1]['value']"]
        }, 
        ejsonpath_transform:transform(?ast("$.items.*.value"), O, fun (_) -> xxx end, #{}, [])),
    
    O1 = #{
        <<"nums">> => [
            [0, 1, 2],
            [1, 2, 3],
            [2, 3, 4]
        ]
    },
    
    ?assertEqual(
        { #{
            <<"nums">> => [
                [0, 1, 2, 3],
                [1, 2, 3, 4],
                [2, 3, 4, 5]
            ]
        },  
          ["$['nums'][0]","$['nums'][1]","$['nums'][2]"]
        }, 
        ejsonpath_transform:transform(?ast("$.nums.*"), O1, fun (E) -> E ++ [hd(lists:reverse(E)) + 1] end, #{}, [])),

    O2 = #{
        <<"outer">> => [
            #{ <<"id">> => 0, <<"inner">> => [#{ <<"id">> => a}]},
            #{ <<"id">> => 1, <<"inner">> => [#{ <<"id">> => a}]}
        ]
    },
    ?assertEqual({#{<<"outer">> =>
                        [#{<<"id">> => 0,<<"inner">> => [#{<<"id">> => b}]},
                         #{<<"id">> => 1,<<"inner">> => [#{<<"id">> => b}]}]},
                  ["$['outer'][0]['inner'][0]['id']",
                   "$['outer'][1]['inner'][0]['id']"]}, 
        ejsonpath_transform:transform(?ast("$.outer.*.inner[0].id"), O2, fun (a) -> b end, #{}, [])),
    ok.

filter_expr_test() ->
    O = #{
        <<"items">> => [
            #{<<"id">> => 0, <<"value">> => yyy},
            #{<<"id">> => 1, <<"value">> => yyy}
        ]
    },
    ?assertEqual(
        { #{
            <<"items">> => [
            #{<<"id">> => 0, <<"value">> => xxx},
            #{<<"id">> => 1, <<"value">> => yyy}
        ]
        },  
          ["$['items'][0]['value']"]
        }, 
        ejsonpath_transform:transform(?ast("$.items[?(@.id == 0)].value"), O, fun (_) -> xxx end, #{}, [])),

    ?assertError(not_found, 
        ejsonpath_transform:transform(?ast("$.items[?(@.id == 2)].value"), O, fun (_) -> xxx end, #{}, [])),
    ok.

slice_test() ->
    O = [x, y, z, a, b, c],
    ?assertEqual({[xxx,xxx,xxx,a,b,c],["$[0]","$[1]","$[2]"]},
        ejsonpath_transform:transform(?ast("$[0:3]"), O, fun (_) -> xxx end, #{}, [])),
    ok.
