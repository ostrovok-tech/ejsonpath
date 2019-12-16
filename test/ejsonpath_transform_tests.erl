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
        ejsonpath_transform:transform(?ast("$"), get_doc(), fun (_) -> {ok, ok} end, #{}, [])),
    
    ?assertError(not_implemented, 
        ejsonpath_transform:transform(?ast("$."), get_doc(), fun (_) -> {ok, ok} end, #{}, [])),
    ?assertError(not_implemented, 
        ejsonpath_transform:transform(?ast("$.."), get_doc(), fun (_) -> {ok, ok} end, #{}, [])),

    ok.

key_access_test() ->
    ?assertEqual({#{<<"a">> => y},  ["$['a']"]}, 
        ejsonpath_transform:transform(?ast("$.a"), #{<<"a">> => x}, fun ({match, #{node := x}}) -> 
            {ok, y} 
        end, #{}, [])),

    ?assertError(not_found, 
        ejsonpath_transform:transform(?ast("$.b"), #{<<"a">> => x}, fun ({match, #{node := x}}) -> {ok, y} end, #{}, [])),

    ?assertEqual({#{<<"a">> => #{<<"a">> => y}},  ["$['a']['a']"]}, 
        ejsonpath_transform:transform(?ast("$.a.a"), #{<<"a">> => #{<<"a">> => x}}, fun ({match, #{node := x}}) -> {ok, y} end, #{}, [])),
    ok.

index_access_test() ->
    ?assertEqual({#{<<"a">> => [y, y, z]},  ["$['a'][0]"]}, 
        ejsonpath_transform:transform(?ast("$.a[0]"), #{<<"a">> => [x, y, z]}, fun ({match, #{node := x}}) -> {ok, y} end, #{}, [])),
    ?assertEqual({#{<<"a">> => [x, y, y]},  ["$['a'][-1]"]}, % TODO: try and put the derived index
        ejsonpath_transform:transform(?ast("$.a[-1]"), #{<<"a">> => [x, y, z]}, fun ({match, #{node := z}}) -> {ok, y} end, #{}, [])),

    ?assertError(not_found,
        ejsonpath_transform:transform(?ast("$.a[3]"), #{<<"a">> => [x, y, z]}, fun (_) -> {ok, y} end, #{}, [])),
    
    ok.
access_list_test() ->
    ?assertEqual(
        { #{<<"a">> => xxx, <<"b">> => xxx, <<"c">> => yyy},  ["$['a']", "$['b']"]}, 
        ejsonpath_transform:transform(?ast("$['a', 'b']"), #{<<"a">> => yyy, <<"b">> => yyy, <<"c">> => yyy}, fun (_) -> {ok, xxx} end, #{}, [])),

    ?assertError(not_found,
        ejsonpath_transform:transform(?ast("$['a', 'd']"), #{<<"a">> => yyy, <<"b">> => yyy, <<"c">> => yyy}, fun (_) -> {ok, xxx} end, #{}, [])),
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
        ejsonpath_transform:transform(?ast("$.items.*.value"), O, fun (_) -> {ok, xxx} end, #{}, [])),
    
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
        ejsonpath_transform:transform(?ast("$.nums.*"), O1, fun ({match, #{type := array, node := E}}) -> {ok, E ++ [hd(lists:reverse(E)) + 1]} end, #{}, [])),

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
        ejsonpath_transform:transform(?ast("$.outer.*.inner[0].id"), O2, fun ({match, #{node := a}}) -> {ok, b} end, {ok, #{}}, [])),
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
        ejsonpath_transform:transform(?ast("$.items[?(@.id == 0)].value"), O, fun (_) -> {ok, xxx} end, #{}, [])),

    ?assertError(not_found, 
        ejsonpath_transform:transform(?ast("$.items[?(@.id == 2)].value"), O, fun (_) -> {ok, xxx} end, #{}, [])),
    ok.

slice_test() ->
    O = [x, y, z, a, b, c],
    ?assertEqual({[xxx,xxx,xxx,a,b,c],["$[0]","$[1]","$[2]"]},
        ejsonpath_transform:transform(?ast("$[0:3]"), O, fun (_) -> {ok, xxx} end, #{}, [])),
    ok.


delete_test() ->
    % simple key delete
    O = #{ <<"a">> => 10, <<"b">> => 10},
    %% error on deleting unexisting key
    ?assertError(not_found, 
        ejsonpath_transform:transform(?ast("$.c"), O, fun ({not_found,_,_,_}) -> delete end, #{}, [])),
    %% ok deleting unexisting key with 'handle_not_found' option
    ?assertEqual({O, ["$['c']"]}, 
        ejsonpath_transform:transform(?ast("$.c"), O, fun ({not_found,_,_,_}) -> delete end, #{}, [handle_not_found])),
    %% ok deleting existing key key
    ?assertEqual({#{<<"a">> => 10}, ["$['b']"]}, 
        ejsonpath_transform:transform(?ast("$.b"), O, fun ({match,_}) -> delete end, #{}, [])),
    ?assertEqual({#{<<"a">> => 10}, ["$['b']"]}, 
        ejsonpath_transform:transform(?ast("$.b"), O, fun ({match,_}) -> delete end, #{}, [handle_not_found])),

    % nested key delete
    O1 = #{ <<"a">> => #{<<"b">> => #{ <<"a">> => 10, <<"b">> => 10}}},
    %% nested path doesn't exist
    ?assertError(not_found, 
        ejsonpath_transform:transform(?ast("$.b.c"), O1, fun ({not_found, _,_,_}) -> delete end, #{}, [])),
    ?assertError(not_found, 
        ejsonpath_transform:transform(?ast("$.b.c"), O1, fun ({not_found, _,_,_}) -> delete end, #{}, [handle_not_found])),
    
    %% nested key in path doesn't exist
    ?assertError(not_found, 
        ejsonpath_transform:transform(?ast("$.a.b.c"), O1, fun ({not_found, _,_,_}) -> delete end, #{}, [])),
    ?assertEqual({O1, ["$['a']['b']['c']"]}, 
        ejsonpath_transform:transform(?ast("$.a.b.c"), O1, fun ({not_found, _,_,_}) -> delete end, #{}, [handle_not_found])),
    %% nested key delete
    ?assertEqual({#{ <<"a">> => #{<<"b">> => #{ <<"a">> => 10}}}, ["$['a']['b']['b']"]}, 
        ejsonpath_transform:transform(?ast("$.a.b.b"), O1, fun ({match,_}) -> delete end, #{}, [])),
    ?assertEqual({#{ <<"a">> => #{<<"b">> => #{ <<"a">> => 10}}}, ["$['a']['b']['b']"]}, 
        ejsonpath_transform:transform(?ast("$.a.b.b"), O1, fun ({match,_}) -> delete end, #{}, [handle_not_found])),
    
    %% array delete
    O2 = [x, y, z],
    % array element doesn't exist
    ?assertError(not_found, 
        ejsonpath_transform:transform(?ast("$[3]"), O2, fun ({not_found,_,_,_}) -> delete end, #{}, [])),
    ?assertEqual({O2, ["$[3]"]}, 
        ejsonpath_transform:transform(?ast("$[3]"), O2, fun ({not_found,_,_,_}) -> delete end, #{}, [handle_not_found])),
    % array element delete
    ?assertEqual({[y, z], ["$[0]"]}, 
        ejsonpath_transform:transform(?ast("$[0]"), O2, fun ({match,_}) -> delete end, #{}, [])),
    ?assertEqual({[x, z], ["$[1]"]}, 
        ejsonpath_transform:transform(?ast("$[1]"), O2, fun ({match,_}) -> delete end, #{}, [])),
    ?assertEqual({[x, y], ["$[2]"]}, 
        ejsonpath_transform:transform(?ast("$[2]"), O2, fun ({match,_}) -> delete end, #{}, [])),
    ?assertEqual({[x, y], ["$[-1]"]}, 
        ejsonpath_transform:transform(?ast("$[-1]"), O2, fun ({match,_}) -> delete end, #{}, [])),
    
    % multimatch delete
    ?assertEqual({[], ["$[0]", "$[1]", "$[2]"]}, 
        ejsonpath_transform:transform(?ast("$.*"), O2, fun ({match,_}) -> delete end, #{}, [])),
    ?assertEqual({#{},["$['a']","$['b']"]}, 
        ejsonpath_transform:transform(?ast("$.*"), O, fun ({match,_}) -> delete end, #{}, [])),
    ?assertEqual({#{<<"a">> => #{<<"b">> => #{}}}, ["$['a']['b']['a']","$['a']['b']['b']"]},
        ejsonpath_transform:transform(?ast("$.a.b.*"), O1, fun ({match,_}) -> delete end, #{}, [])),

    O3 = #{
        <<"items">> => [
            #{<<"id">> => 0, <<"value">> => yyy},
            #{<<"id">> => 1, <<"value">> => yyy}
        ]
    },
    ?assertEqual(
        { #{
            <<"items">> => [
                #{<<"id">> => 1, <<"value">> => yyy}
        ]
        },  
          ["$['items'][0]"]
        }, 
    ejsonpath_transform:transform(?ast("$.items[?(@.id == 0)]"), O3, fun (_) -> delete end, #{}, [])),
    
    ?assertEqual(
        { #{
            <<"items">> => [
                #{<<"id">> => 0},
                #{<<"id">> => 1, <<"value">> => yyy}
            ]
        },
          ["$['items'][0]['value']"]
        }, 
    ejsonpath_transform:transform(?ast("$.items[?(@.id == 0)].value"), O3, fun (_) -> delete end, #{}, [])),

    ?assertError(not_found,
        ejsonpath_transform:transform(?ast("$.items[?(@.id == 10)]"), O3, fun (_) -> delete end, #{}, [handle_not_found])),

    ok.

