%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% @copyright (C) 2013, Sergey Prokhorov
%%% @doc
%%%
%%% @end
%%% Created : 12 Aug 2013 by Sergey Prokhorov <me@seriyps.ru>

-module(ejsonpath).

-export([execute/2, execute/3]).

-export_type([json_node/0]).

-record(
   context,
   {root,
    set=[],
    functions=[]}).


-type json_node() :: null                           % null
                     | boolean()                    % true/false
                     | binary()                     % string
                     | number()                     % int/float
                     | [json_node()]                % array
                     | {[{binary(), json_node()}]}. % hash (object)
-type jsonpath() :: string().


execute(Path, Doc) ->
    execute(Path, Doc, []).

-spec execute(jsonpath(), json_node(), []) -> [json_node()].
execute(Path, Doc, Functions) ->
    {ok, Tokens, _} = ejsonpath_scan:string(Path),
    {ok, Tree} = ejsonpath_parse:parse(Tokens),
    Context = #context{root=Doc, set=[], functions=Functions},
    try
        #context{set=Result} = execute_tree(Tree, Context),
        Result
    catch Class:Reason ->
            io:format(user, "~p~n~p~n~p~n",
                      [Class, Reason, erlang:get_stacktrace()]),
            {error, Class, Reason, erlang:get_stacktrace()}
    end.

execute_tree({root, {steps, Steps}}, #context{root=Root} = Ctx) ->
    execute_step(Steps, Ctx#context{set=[Root]}).

execute_step([{child, {refine, Predicate}} | Next], #context{set=NodeList}=Ctx) ->
    NewNodeList = lists:foldl(
                    fun(Elem, Acc) ->
                            AppendList = apply_predicate(Predicate, Elem, element_type(Elem), Ctx),
                            Acc ++ AppendList   %TODO: optimize
                    end, [], NodeList),
    execute_step(Next, Ctx#context{set=NewNodeList});
execute_step([], Ctx) ->
    Ctx.


apply_predicate(Key, {Pairs}, hash, _Ctx) when is_binary(Key) ->
    case proplists:get_value(Key, Pairs) of
        undefined -> [];
        Value -> [Value]
    end;
apply_predicate(Key, _, _, _Ctx) when is_binary(Key) ->
    [];
%% apply_predicate({index_expr, Script}, Ctx) ->
%%     index_value(eval_script(Script, Ctx));
apply_predicate({bin_expr, Script}, {Pairs}, hash, Ctx) ->
    [V || {_K, V} <- Pairs, boolean_value(eval_script(Script, V, Ctx))];
apply_predicate({bin_expr, Script}, L, array, Ctx) ->
    [V || V <- L, boolean_value(eval_script(Script, V, Ctx))];
apply_predicate({slice_list, Items}, L, array, _Ctx) ->
    slice_list(Items, L, length(L));
apply_predicate({slice_list, Items}, {Pairs}, hash, _Ctx) ->
    [proplists:get_value(K, Pairs)
    || K <- Items];
apply_predicate({slice, Begin, End, Step}, L, array, _Ctx) ->
    slice_step(Begin, End, Step, L);
apply_predicate('*', {Pairs}, hash, _Ctx) ->
    [V || {_K, V} <- Pairs];
apply_predicate('*', L, array, _Ctx) ->
    L.

eval_script(Key, _, _) when is_binary(Key) ->
    Key;
eval_script(Idx, _, _) when is_number(Idx) ->
    Idx;
eval_script({function_call, Name, Args}, CurNode, #context{functions=Funs, root=Root}) ->
    Fun = proplists:get_value(Name, Funs),
    Fun({CurNode, Root}, Args);
eval_script({bin_op, _Op, _L, _R}, _, _) ->
    error({not_implemented, bin_op});
eval_script('@', CurNode, _Ctx) ->
    CurNode.


slice_list([Idx | Rest], L, Len) when Idx < 0 ->
    NewIdx = Len + Idx,
    slice_list([NewIdx | Rest], L, Len);
slice_list([Idx | Rest], L, Len) ->
    [lists:nth(Idx + 1, L) | slice_list(Rest, L, Len)];
slice_list([], _, _) ->
    [].


slice_step(Begin, End, S, L) when (Begin < 0) ->
    %% [-5:]
    slice_step(max(length(L) + Begin, 0), End, S, L);
slice_step(Begin, End, S, L) when (End < 0) ->
    %% [:-5]
    Len = length(L),
    slice_step(Begin, min(Len + End, Len), S, L);
slice_step(Begin, End, 1, L) when (Begin >= 0) and (End >= 0) ->
    lists:sublist(L, Begin + 1, Begin + End);
slice_step(_Begin, _End, _Step, _L) ->
    error({not_implemented, slice}).

%% type casts
boolean_value([]) ->
    false;
boolean_value({[]}) ->
    false;
boolean_value(<<>>) ->
    false;
boolean_value(null) ->
    false;
boolean_value(0) ->
    false;
boolean_value(false) ->
    false;
boolean_value(_) ->
    true.






element_type(L) when is_list(L) ->
    array;
element_type({L}) when is_list(L) ->
    hash;
element_type({_, _}) ->
    hash_pair;
element_type(Bin) when is_binary(Bin) ->
    string;
element_type(Num) when is_number(Num) ->
    number.
