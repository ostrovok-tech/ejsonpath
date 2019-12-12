-module(ejsonpath_eval).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-include("./ejsonpath.hrl").
-import(ejsonpath_common, [argument/2, argument/3, script_eval/3]).

-export([
    eval/4,
    eval_step/3
]).

eval({root, '$'}, Node, _, _) ->
    {[Node], ["$"]};
eval({root, Descendant}, Node, _, _) when is_atom(Descendant) ->
    ejsonpath_common:unzip(children(Descendant, [argument(Node, "$")]));
eval({root, Predicates}, Node, Funcs, Options) ->
    Context = #{
        root => Node,
        opts => Options,
        funcs => Funcs,

        eval_root => fun (SubQuery) -> 
            eval(SubQuery, Node, Funcs, Options)
        end,
        eval_step => fun (SubQuery, CurrNode, Ctx) -> 
            eval_step(SubQuery, [CurrNode], Ctx)
        end
    },
    eval_step(Predicates, [argument(Node, "$")], Context).

eval_step([{Children, {predicate, Predicate}} | Rest], Result, Cxt) -> 
    % erlang:display({enter, Predicate}),
    NewResult = lists:foldl(
        fun (Arg, Acc) ->
            Acc ++ apply_eval(Predicate, Arg, Cxt)
        end, [], children(Children, Result)),
    eval_step(Rest, NewResult, Cxt);
eval_step([], Result, _) -> ejsonpath_common:unzip(Result);
eval_step(_, _, _) ->
    erlang:error(not_implemented).

%% {key, _}
apply_eval({key, '*'}, #argument{type = hash, node = Node} = Arg, Ctx) ->
    Keys = maps:keys(Node),
    apply_eval({access_list, Keys}, Arg, Ctx);
apply_eval({key, '*'}, #argument{type = array, node = Node} = Arg, Ctx) ->
    Idxs = lists:seq(0, erlang:length(Node)-1),
    apply_eval({access_list, Idxs}, Arg, Ctx);
apply_eval({key, Key}, #argument{type = hash} = Arg, Ctx) -> 
    apply_eval({access_list, [Key]}, Arg, Ctx);
apply_eval({key, _}, _, _) ->
    [];
% {access_list, KeysOrIdxs}
apply_eval({access_list, Idxs}, #argument{type = array, node = Node, path = Path}, _) ->
    lists:reverse(lists:foldl(fun (Idx0, Acc) -> 
        case ejsonpath_common:index(Idx0, erlang:length(Node)) of
            {error, _} -> Acc;
            {ok, Idx} -> [argument(lists:nth(Idx, Node), Path, Idx-1)|Acc]
        end
    end, [], Idxs));
apply_eval({access_list, Keys}, #argument{type = hash, node = Node, path = Path}, _) ->
    lists:reverse(lists:foldl(fun (Key, Acc) -> 
        case maps:get(Key, Node, '$undefined') of
            '$undefined' -> Acc;
            Child -> [argument(Child, Path, Key)|Acc]
        end
    end, [], Keys));

% {filter_expr, Script}
apply_eval({filter_expr, Script}, #argument{type = hash, node = Node, path = Path} = Arg, Ctx) ->
    Keys = lists:reverse(maps:fold(fun (Key, Value, Acc) -> 
        case ejsonpath_common:to_boolean(script_eval(Script, argument(Value, Path, Key), Ctx)) of
            false -> Acc;
            _ -> [Key|Acc]
        end
    end, [], Node)),
    apply_eval({access_list, Keys}, Arg, Ctx);
apply_eval({filter_expr, Script}, #argument{type = array, node = Node, path = Path} = Arg, Ctx) ->
    {_, Idxs} = lists:foldl(fun (Item, {Idx, Acc}) ->
        case ejsonpath_common:to_boolean(script_eval(Script, argument(Item, Path, Idx), Ctx)) of
            false -> {Idx+1, Acc};
            _ -> {Idx+1, [Idx|Acc]}
        end
    end, {0, []}, Node),
    apply_eval({access_list, lists:reverse(Idxs)}, Arg, Ctx);
apply_eval({filter_expr, Script}, #argument{} = Arg, Ctx) ->
    case ejsonpath_common:to_boolean(script_eval(Script, Arg, Ctx)) of
        false -> [];
        _ -> [Arg]
    end;

%% {transform_expr, Script}
apply_eval({transform_expr, Script}, #argument{type = hash, node = Node, path = Path}, Ctx) ->
    lists:reverse(maps:fold(fun (Key, Value, Acc) -> 
        Result = script_eval(Script, argument(Value, Path, Key), Ctx),
        [ argument(Result, Path, Key) | Acc ]
    end, [], Node));
apply_eval({transform_expr, Script}, #argument{type = array, node = Node, path = Path}, Ctx) ->
    {_, NewNode} = lists:foldl(fun (Item, {Idx, Acc}) ->
        Result = script_eval(Script, argument(Item, Path, Idx), Ctx),
        [ argument(Result, Path, Idx) | Acc ]
    end, {0, []}, Node),
    lists:reverse(NewNode);
apply_eval({transform_expr, Script}, #argument{path = Path} = Arg, Ctx) ->
    Result = script_eval(Script, Arg, Ctx),
    [#argument{type = ejsonpath_common:type(Result), node = Result, path = Path}];

%% {slice, S, E, Step}
apply_eval({slice, Start, End, Step}, #argument{type = array, node = Node} = Arg, Ctx) ->
    case ejsonpath_common:slice_seq(Start, End, Step, length(Node)) of
        {error, _} -> [];
        Seq ->
            apply_eval({access_list, Seq}, Arg, Ctx)
    end;
apply_eval(P, _, _) ->
    erlang:display({not_implemented, P}),
    erlang:error(not_implemented).

children(child, Nodes) -> Nodes;
children(descendant, Nodes) ->
    children_i(Nodes, []).

children_i([], Acc) -> 
    Acc;
children_i([#argument{type = array, node = Node, path = Path } = Arg | Rest], Acc) ->
    {_, AddAcc} = lists:foldl(fun (Child, {Idx, InnerAcc}) -> 
        {Idx+1, InnerAcc ++ children_i([argument(Child, Path, Idx)], [])}
    end, {0, []}, Node),
    children_i(Rest, Acc ++ [Arg] ++ AddAcc);
children_i([#argument{type = hash, node = Node, path = Path} = Arg | Rest], Acc) ->
    AddAcc = maps:fold(fun (Key, Child, InnerAcc) -> 
        InnerAcc ++ children_i([argument(Child, Path, Key)], [])
    end, [], Node),
    children_i(Rest, Acc ++ [Arg] ++ AddAcc);
children_i([Arg = #argument{}| Rest], Acc) ->
    children_i(Rest, Acc ++ [Arg]).