-module(ejsonpath_eval_tests).

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
    O1 = #{},
    ?assertEqual({[O1], ["$"]}, ejsonpath_eval:eval(?ast("$"), O1, #{}, [])),
    
    O2 = [],
    ?assertEqual({[O2], ["$"]}, ejsonpath_eval:eval(?ast("$"), O2, #{}, [])),

    O3 = #{
        <<"a">> => [1,2,3],
        <<"b">> => [#{ <<"id">> => 0}, #{ <<"id">> => 1}]
    },
    ?assertEqual({[O3], ["$"]}, ejsonpath_eval:eval(?ast("$."), O3, #{}, [])),

    ?assertEqual(
        { [ #{<<"a">> => [1,2,3],<<"b">> => [#{<<"id">> => 0},#{<<"id">> => 1}]},
            [1,2,3],
            1,2,3,
            [#{<<"id">> => 0},#{<<"id">> => 1}],
            #{<<"id">> => 0},
            0,
            #{<<"id">> => 1},
            1],
            [
                "$",
                "$['a']",
                "$['a'][0]",
                "$['a'][1]",
                "$['a'][2]",
                "$['b']",
                "$['b'][0]",
                "$['b'][0]['id']",
                "$['b'][1]",
                "$['b'][1]['id']"
            ]}, ejsonpath_eval:eval(?ast("$.."), O3, #{}, [])),
    
    ok.

key_access_test() ->
    ?assertEqual({[], []}, ejsonpath_eval:eval(?ast("$.none"), get_doc(), #{}, [])),
    ?assertEqual({[], []}, ejsonpath_eval:eval(?ast("$.none"), [1,2,3], #{}, [])),
    
    ?assertEqual({[maps:get(<<"store">>, get_doc())], ["$['store']"]}, ejsonpath_eval:eval(?ast("$.store"), get_doc(), #{}, [])),

    ?assertEqual({[<<"red">>], ["$['store']['bicycle']['color']"]}, ejsonpath_eval:eval(?ast("$.store.bicycle.color"), get_doc(), #{}, [])),

    ?assertEqual({[<<"reference">>], ["$['store']['book'][0]['category']"]},
                 ejsonpath_eval:eval(?ast("$.store.book.0.category"), get_doc(), #{}, [])),

    ?assertEqual({[<<"an-integer-key">>], ["$['store']['3']"]}, ejsonpath_eval:eval(?ast("$.store.3"), get_doc(), #{}, [])),
    
    ok.

descendant_test() ->
    ?assertEqual(
        { [19.95, 8.95, 12.99, 8.99, 22.99],
        [
            "$['store']['bicycle']['price']",
            "$['store']['book'][0]['price']",
            "$['store']['book'][1]['price']",
            "$['store']['book'][2]['price']",
            "$['store']['book'][3]['price']"]
        },
        ejsonpath_eval:eval(?ast("$..price"), get_doc(), #{}, [])),
        
    ?assertEqual(
        { [0,1,2,3],
            [
                "$['store']['book'][0]['id']",
                "$['store']['book'][1]['id']",
                "$['store']['book'][2]['id']",
                "$['store']['book'][3]['id']"]
        },
        ejsonpath_eval:eval(?ast("$..book..id"), get_doc(), #{}, [])),

    ok.

access_list_test() ->
    ?assertEqual({[], []}, ejsonpath_eval:eval(?ast("$['none']"), get_doc(), #{}, [])),
    ?assertEqual({[], []}, ejsonpath_eval:eval(?ast("$['none']"), [1,2,3], #{}, [])),
    ?assertEqual({[], []}, ejsonpath_eval:eval(?ast("$[3]"), [1,2,3], #{}, [])),
    ?assertEqual({[], []}, ejsonpath_eval:eval(?ast("$[-4]"), [1,2,3], #{}, [])),

    ?assertEqual({[maps:get(<<"store">>, get_doc())], ["$['store']"]}, ejsonpath_eval:eval(?ast("$['store']"), get_doc(), #{}, [])),

    ?assertEqual({[<<"red">>], ["$['store']['bicycle']['color']"]}, ejsonpath_eval:eval(?ast("$['store']['bicycle']['color']"), get_doc(), #{}, [])),

    ?assertEqual(
        {[<<"red">>, 19.95], ["$['store']['bicycle']['color']", "$['store']['bicycle']['price']"]}, 
        ejsonpath_eval:eval(?ast("$['store']['bicycle']['color', 'price']"), get_doc(), #{}, [])),
    ?assertEqual(
        {[<<"red">>, 19.95, <<"red">>], 
        [ 
            "$['store']['bicycle']['color']", 
            "$['store']['bicycle']['price']",
            "$['store']['bicycle']['color']"
        ]}, 
        ejsonpath_eval:eval(?ast("$['store']['bicycle']['color', 'price', 'noway', 0, 'color']"), get_doc(), #{}, [])),

    ?assertEqual({[1], ["$[0]"]}, ejsonpath_eval:eval(?ast("$[0]"), [1,2,3], #{}, [])),
    ?assertEqual({[1,3], ["$[0]", "$[2]"]}, ejsonpath_eval:eval(?ast("$[0,2]"), [1,2,3], #{}, [])),

    ?assertEqual({[3], ["$[2]"]}, ejsonpath_eval:eval(?ast("$[-1]"), [1,2,3], #{}, [])),
    ?assertEqual({[2], ["$[1]"]}, ejsonpath_eval:eval(?ast("$[-2]"), [1,2,3], #{}, [])),
    ?assertEqual({[1], ["$[0]"]}, ejsonpath_eval:eval(?ast("$[-3]"), [1,2,3], #{}, [])),
    ?assertEqual({[3,2,1], ["$[2]", "$[1]", "$[0]"]}, ejsonpath_eval:eval(?ast("$[-1,-2,-3]"), [1,2,3], #{}, [])),
    
    ?assertEqual({[3,2,1,1], ["$[2]", "$[1]", "$[0]", "$[0]"]}, ejsonpath_eval:eval(?ast("$[-1,-2,-3, 'noway', 0]"), [1,2,3], #{}, [])),
    ?assertEqual(
        { [8.95, 12.99, 8.99, 22.99],
        [
            "$['store']['book'][0]['price']",
            "$['store']['book'][1]['price']",
            "$['store']['book'][2]['price']",
            "$['store']['book'][3]['price']"
        ]}, 
        ejsonpath_eval:eval(?ast("$.store.book.*.price"), get_doc(), #{}, [])),
    ok.

slice_test() ->
    ?assertEqual({[], []}, ejsonpath_eval:eval(?ast("$[:]"), [], #{}, [])),
    ?assertEqual({[], []}, ejsonpath_eval:eval( ?ast("$[0:0]"), [x,y,z], #{}, [])),

    ?assertEqual({[x,y,z], ["$[0]", "$[1]", "$[2]"]}, ejsonpath_eval:eval( ?ast("$[:]"), [x,y,z], #{}, [])),
    ?assertEqual({[], []}, ejsonpath_eval:eval( ?ast("$[0:0]"), [x,y,z], #{}, [])),
    ?assertEqual({[x], ["$[0]"]}, ejsonpath_eval:eval( ?ast("$[0:1]"), [x,y,z], #{}, [])),

    ?assertEqual({[y,z], ["$[1]", "$[2]"]}, ejsonpath_eval:eval( ?ast("$[1:]"), [x,y,z], #{}, [])),
    ?assertEqual({[x], ["$[0]"]}, ejsonpath_eval:eval( ?ast("$[:1]"), [x,y,z], #{}, [])),
    ok.

filter_expr_test() ->
    Subject = #{
        <<"name">> => <<"name">>,
        <<"name1">> => <<"name1">>,
        <<"lastname">> => <<"lastname">>,
        <<"items">> => [1,2,3,4,5]
    },
    Funcs = #{
        is_even => fun ({Curr, _}, []) ->
            Curr rem 2 =:= 0
        end,
        is_name => fun 
            ({<<"name">>, _}, []) -> true;
            ({_, _}, []) -> false
        end
    },
    ?assertEqual({[2,4],["$['items'][1]","$['items'][3]"]}, 
        ejsonpath_eval:eval(?ast("$.items[?(is_even())]"), Subject, Funcs, [])),

    ?assertEqual({[<<"name">>],["$['name']"]},
        ejsonpath_eval:eval(?ast("$[ ?( is_name() ) ]"), Subject, Funcs, [])),

    ?assertEqual({ [8.95,8.99],
        [
            "$['store']['book'][0]['price']",
            "$['store']['book'][2]['price']"
        ]},
        ejsonpath_eval:eval(?ast("$.store.book.*.price[ ?( gt(8) && lt(10) ) ]"), get_doc(), #{
            gt => fun ({N, _}, [V]) -> N > V end,
            lt => fun ({N, _}, [V]) -> N < V end
        }, [])),

    ?assertEqual(
        {[8.95],["$['store']['book'][0]['price']"]},
         ejsonpath_eval:eval(?ast("$.store.book[?(@.id == 0)].price"), get_doc(), #{}, [])),

    ?assertEqual(
        {[3, 22.99],
        [
            "$['store']['book'][3]['id']",
            "$['store']['book'][3]['price']"]
        },
        ejsonpath_eval:eval(?ast("$.store.book[?(@.id == $.last_id)]['id', 'price']"), get_doc(), #{}, [])),

    ?assertEqual({[8.99,22.99],
        [
            "$['store']['book'][2]['price']",
            "$['store']['book'][3]['price']"
        ]}, ejsonpath_eval:eval(?ast("$.store.book[?(@.isbn)].price"), get_doc(), #{}, [])),
    
    ok.

transform_expr_test() ->
    ?assertEqual({
        [89.5,129.9,89.9,229.89999999999998],
        [ 
            "$['store']['book'][0]['price']",
            "$['store']['book'][1]['price']",
            "$['store']['book'][2]['price']",
            "$['store']['book'][3]['price']"
        ]}, 
        ejsonpath_eval:eval(?ast("$..book..price[(mul(10))]"), get_doc(), #{
            mul => fun ({N, _}, [M]) -> N * M end
        }, [])),

    ok.
