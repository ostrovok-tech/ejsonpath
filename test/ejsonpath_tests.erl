%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2013, Sergey Prokhorov
%%% @doc
%%%
%%% @end
%%% Created : 12 Aug 2013 by Sergey Prokhorov <me@seriyps.ru>

-module(ejsonpath_tests).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

cases() ->
    [
     {"Name refine dot",
      "$.store.bicycle.color", {[<<"red">>],["$['store']['bicycle']['color']"]}},
     {"Name refine bracket",
      "$['store']['bicycle']['color']", {[<<"red">>],["$['store']['bicycle']['color']"]}},

     {"Array idx",
      "$.store.book[0].category", {[<<"reference">>],["$['store']['book'][0]['category']"]}},
     {"Array slice list",
      "$.store.book[0,-1].author", {[<<"Nigel Rees">>,<<"J. R. R. Tolkien">>],
                  ["$['store']['book'][0]['author']",
                   "$['store']['book'][3]['author']"]}},

     {"Array slice python",
      "$.store.book[0:2].price", {[8.95,12.99],
                  ["$['store']['book'][0]['price']",
                   "$['store']['book'][1]['price']"]}},
     {"Array slice python default begin",
      "$.store.book[:2].price", {[8.95,12.99],
                  ["$['store']['book'][0]['price']",
                   "$['store']['book'][1]['price']"]}},
     {"Array slice python default end",
      "$.store.book[2:].price", {[8.99,22.99],
                  ["$['store']['book'][2]['price']",
                   "$['store']['book'][3]['price']"]}},
     {"Array slice python minus begin",
      "$.store.book[-2:].price", {[8.99,22.99],
                  ["$['store']['book'][2]['price']",
                   "$['store']['book'][3]['price']"]}},
     {"Array slice python minus end",
      "$.store.book[:-2].price", {[8.95,12.99],
                  ["$['store']['book'][0]['price']",
                   "$['store']['book'][1]['price']"]}},

     {"Name refine bracket plus idx",
      "$.store['book'][0].category", {[<<"reference">>],["$['store']['book'][0]['category']"]}},
     {"Array idx",
      "$.store.book[0].category", {[<<"reference">>],["$['store']['book'][0]['category']"]}},
     {"Asterisk array",
      "$.store.book[*].category", {[<<"reference">>,<<"fiction">>,<<"fiction">>,<<"fiction">>],
                  ["$['store']['book'][0]['category']",
                   "$['store']['book'][1]['category']",
                   "$['store']['book'][2]['category']",
                   "$['store']['book'][3]['category']"]}},
     {"Asterisk hash",
      "$.store.bicycle[*]", {[<<"red">>,19.95],
                  ["$['store']['bicycle']['color']",
                   "$['store']['bicycle']['price']"]}},
     {"Hash slice list",
      "$.store.book[0]['category','author']", {[<<"reference">>,<<"Nigel Rees">>],
                  ["$['store']['book'][0]['category']",
                   "$['store']['book'][0]['author']"]}},

     {"Bin-eval array literal-string expr",
      "$.store[?('ok')].color", {[<<"red">>],["$['store']['bicycle']['color']"]}},
     {"Bin-eval array literal-empty-string expr",
      "$.store[?('')].color", {[],[]}},
     {"Bin-eval array literal-integer expr",
      "$.store[?(1)].color", {[<<"red">>],["$['store']['bicycle']['color']"]}},
     {"Bin-eval array literal-zero expr",
      "$.store[?(0)].color", {[],[]}},
     %% {"Bin-eval array literal-boolean-true expr",
     %%  "$.store[?(true)].color", [<<"red">>]},
     %% {"Bin-eval array literal-boolean-false expr",
     %%  "$.store[?(false)].color", []},
     {"Bin-eval array subst-self expr",
      "$.store[?(@)].color", {[<<"red">>],["$['store']['bicycle']['color']"]}},
     {"Bin-eval array function call",
      "$.store[?(my_fun())].color", {[<<"red">>],["$['store']['bicycle']['color']"]},
      #{my_fun => fun(_, []) -> true end}},
     {"Bin-eval array function call",
      "$.store[?(my_fun())].color", {[],[]},
      #{my_fun => fun(_, []) -> false end}},
     {"Bin-eval array function call with args",
      "$.store[?(my_fun('a1', 42))].color", {[],[]},
      #{my_fun => fun(_, [<<"a1">>, 42]) -> false end}},

     {"Bin-eval array function call",
      "$.store.book[?(filter_reference())].author", {[<<"Nigel Rees">>],["$['store']['book'][0]['author']"]},
      #{filter_reference =>
        fun({Hash, _Doc}, []) ->
                Val = case Hash of
                          {KV} ->
                              proplists:get_value(<<"category">>, KV);
                          Map ->
                              maps:get(<<"category">>, Map, nil)
                      end,
                Val == <<"reference">>
        end}},

     {"Script simple path",
      "$.store.book[?(@.isbn)].price", {[8.99,22.99],
                  ["$['store']['book'][2]['price']",
                   "$['store']['book'][3]['price']"]}},
     {"Script greater than expr",
      "$.store.book[?(@.price > 9)].isbn", {[<<"0-395-19395-8">>],["$['store']['book'][3]['isbn']"]}},
     {"Script equality expr",
      "$.store.book[?('reference' == @.category)].author",
      {[<<"Nigel Rees">>],["$['store']['book'][0]['author']"]}},
     {"Script inequality expr",
      "$.store.book[?(@.category != 'fiction')].author",
      {[<<"Nigel Rees">>],["$['store']['book'][0]['author']"]}},
     {"Script comparing constants",
      "$.store.book[?('asdf'=='asdf')].category",
      {[<<"reference">>,<<"fiction">>,<<"fiction">>,<<"fiction">>],
                  ["$['store']['book'][0]['category']",
                   "$['store']['book'][1]['category']",
                   "$['store']['book'][2]['category']",
                   "$['store']['book'][3]['category']"]}},
     {"Script comparing paths",
      "$.store.book[?(@.category == @.category)].category",
      {[<<"reference">>,<<"fiction">>,<<"fiction">>,<<"fiction">>],
                  ["$['store']['book'][0]['category']",
                   "$['store']['book'][1]['category']",
                   "$['store']['book'][2]['category']",
                   "$['store']['book'][3]['category']"]}},

    % TODO: this behaviour makes sense??
    %{"Index-eval on array",
      %"$.store.book[(1)].author", [<<"Evelyn Waugh">>]}
     %{"Index-eval on hash",
     % "$.store.book[(1)][('author')]", [<<"Evelyn Waugh">>]},
     {"Capitals should work in paths",
      "$.store.LOLs.CHEEZBURG", {[<<"no">>],["$['store']['LOLs']['CHEEZBURG']"]}},
     {"Capitals should work in paths",
      "$.store.LOLs.Cats", {[<<"yes">>],["$['store']['LOLs']['Cats']"]}}
    ].

cases_test() ->
    Doc = get_doc(),
    Tests = cases(),
    do_test(Doc, Tests).

do_test(Doc, Pairs) ->
    lists:map(
      fun({Name, Expr, Expected}) ->
              {Name,
               fun() ->
                    Result = ejsonpath:q(Expr, Doc),
                    % io:format(user, "~p~n~p~n~n", [Expr, Result]),
                    ?assertEqual(Expected, Result)
               end()};
         ({Name, Expr, Expected, Funs}) ->
              {Name,
               fun() ->
                    Result = ejsonpath:q(Expr, Doc, Funs),
                    %% io:format(user, "~p~n~p~n~n", [Expr, Result]),
                    ?assertEqual(Expected, Result)
               end()}
      end, Pairs).

get_doc() ->
    {ok, Bin} = file:read_file("./test/doc.json"),
    jsx:decode(Bin, [return_maps]).
