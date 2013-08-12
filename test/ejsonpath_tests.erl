%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2013, Sergey Prokhorov
%%% @doc
%%%
%%% @end
%%% Created : 12 Aug 2013 by Sergey Prokhorov <me@seriyps.ru>

-module(ejsonpath_tests).

-include_lib("eunit/include/eunit.hrl").


all_test_() ->
    Doc = get_doc(),
    Pairs = [
             {"Name refine dot",
              "$.store.bicycle.color", [<<"red">>]},
             {"Name refine bracket",
              "$['store']['bicycle']['color']", [<<"red">>]},

             {"Array idx",
              "$.store.book[0].category", [<<"reference">>]},
             {"Array slice list",
              "$.store.book[0,-1].author", [<<"Nigel Rees">>,
                                            <<"J. R. R. Tolkien">>]},

             {"Array slice python",
              "$.store.book[0:2].price", [8.95, 12.99]},
             {"Array slice python default begin",
              "$.store.book[:2].price", [8.95, 12.99]},
             {"Array slice python default end",
              "$.store.book[2:].price", [8.99, 22.99]},
             %% {"Array slice python minus begin",
             %%  "$.store.book[-2:].price", [8.99, 22.99]},
             {"Array slice python minus end",
              "$.store.book[:-2].price", [8.95, 12.99]},

             {"Name refine bracket plus idx",
              "$.store['book'][0].category", [<<"reference">>]},
             {"Array idx",
              "$.store.book[0].category", [<<"reference">>]},
             {"Asterisk array",
              "$.store.book[*].category", [<<"reference">>,
                                           <<"fiction">>,
                                           <<"fiction">>,
                                           <<"fiction">>]},
             {"Asterisk hash",
              "$.store.bicycle[*]", [<<"red">>,
                                     19.95]},
             {"Hash slice list",
              "$.store.book[0]['category','author']", [<<"reference">>,
                                                       <<"Nigel Rees">>]},

             {"Bin array literal-string expr",
              "$.store[?('ok')].color", [<<"red">>]},
             {"Bin array literal-empty-string expr",
              "$.store[?('')].color", []},
             {"Bin array literal-integer expr",
              "$.store[?(1)].color", [<<"red">>]},
             {"Bin array literal-zero expr",
              "$.store[?(0)].color", []},
             %% {"Bin array literal-boolean-true expr",
             %%  "$.store[?(true)].color", [<<"red">>]},
             %% {"Bin array literal-boolean-false expr",
             %%  "$.store[?(false)].color", []},
             {"Bin array subst-self expr",
              "$.store[?(@)].color", [<<"red">>]},
             {"Bin array function call",
              "$.store[?(my_fun())].color", [<<"red">>],
              [{<<"my_fun">>, fun(_, []) -> true end}]},
             {"Bin array function call",
              "$.store[?(my_fun())].color", [],
              [{<<"my_fun">>, fun(_, []) -> false end}]},

             {"Bin array function call",
              "$.store.book[?(filter_reference())].author", [<<"Nigel Rees">>],
              [{<<"filter_reference">>,
                fun({{Pairs}, _Doc}, []) ->
                        case proplists:get_value(<<"category">>, Pairs) of
                            <<"reference">> -> true;
                            _ -> false
                        end
                end}]}

            ],
    lists:map(
      fun({Name, Expr, Expected}) ->
              {Name,
               fun() ->
                       Result = ejsonpath:execute(Expr, Doc),
                       io:format(user, "~p~n~p~n~n", [Expr, Result]),
                       ?assertEqual(Expected, Result)
               end};
         ({Name, Expr, Expected, Funs}) ->
              {Name,
               fun() ->
                       Result = ejsonpath:execute(Expr, Doc, Funs),
                       io:format(user, "~p~n~p~n~n", [Expr, Result]),
                       ?assertEqual(Expected, Result)
               end}
      end,  Pairs).


get_doc() ->
    {ok, Bin} = file:read_file("../test/doc.json"),
    jiffy:decode(Bin).
