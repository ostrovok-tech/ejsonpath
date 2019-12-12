-module(ejsonpath_eval).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-record(argument, {type, node, path}).

-export([
    eval/4
]).

eval({root, '$'}, Node, _, _) ->
    {[Node], ["$"]};
eval({root, Descendant}, Node, _, _) when is_atom(Descendant) ->
    unzip(children(Descendant, [argument(Node, "$")]));
eval({root, Predicates}, Node, Funcs, Options) ->
    Context = #{
        root => Node,
        opts => Options,
        funcs => Funcs
    },
    eval_step(Predicates, [argument(Node, "$")], Context).

eval_step([{Children, {predicate, Predicate}} | Rest], Result, Cxt) -> 
    % erlang:display({enter, Predicate}),
    NewResult = lists:foldl(
        fun (Arg, Acc) ->
            Acc ++ apply_eval(Predicate, Arg, Cxt)
        end, [], children(Children, Result)),
    eval_step(Rest, NewResult, Cxt);
eval_step([], Result, _) -> unzip(Result);
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
        case to_boolean(script_eval(Script, argument(Value, Path, Key), Ctx)) of
            false -> Acc;
            _ -> [Key|Acc]
        end
    end, [], Node)),
    apply_eval({access_list, Keys}, Arg, Ctx);
apply_eval({filter_expr, Script}, #argument{type = array, node = Node, path = Path} = Arg, Ctx) ->
    {_, Idxs} = lists:foldl(fun (Item, {Idx, Acc}) ->
        case to_boolean(script_eval(Script, argument(Item, Path, Idx), Ctx)) of
            false -> {Idx+1, Acc};
            _ -> {Idx+1, [Idx|Acc]}
        end
    end, {0, []}, Node),
    apply_eval({access_list, lists:reverse(Idxs)}, Arg, Ctx);
apply_eval({filter_expr, Script}, #argument{} = Arg, Ctx) ->
    case to_boolean(script_eval(Script, Arg, Ctx)) of
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

script_eval({op, _} = Op, Arg, Ctx) ->
    op_eval(Op, Arg, Ctx);
script_eval({function_call, Name, Args}, #argument{node = Node}, #{funcs := Funcs, root := Root}) -> 
    Func = maps:get(Name, Funcs),
    Func({Node, Root}, Args);
script_eval({bin_op, '==', L, R}, Arg = #argument{}, Ctx) ->
    binary_op_eval(L, R, Arg, Ctx, fun(X, Y) -> X == Y end);
script_eval({bin_op, '!=', L, R}, Arg = #argument{}, Ctx) ->
    binary_op_eval(L, R, Arg, Ctx, fun(X, Y) -> X /= Y end);
script_eval({bin_op, '>', L, R}, Arg = #argument{}, Ctx) ->
    binary_op_eval(L, R, Arg, Ctx, fun(X, Y) -> X > Y end);
script_eval({bin_op, '<', L, R}, Arg = #argument{}, Ctx) ->
    binary_op_eval(L, R, Arg, Ctx, fun(X, Y) -> X < Y end);
script_eval({bin_op, '&&', L, R}, Arg = #argument{}, Ctx) ->
    binary_op_eval(L, R, Arg, Ctx, fun(X, Y) -> X andalso Y end);
script_eval({bin_op, '||', L, R}, Arg = #argument{}, Ctx) ->
    binary_op_eval(L, R, Arg, Ctx, fun(X, Y) -> X orelse Y end);
script_eval({bin_op, Op, _L, _R}, _, _) ->
    erlang:error({not_implemented, bin_op, Op});
script_eval(S, _, _) ->
    erlang:display({not_implemented, S}),
    erlang:error(not_implemented).

binary_op_eval(Lhs, Rhs, Node, Ctx, Op) ->
    case {op_eval(Lhs, Node, Ctx), op_eval(Rhs, Node, Ctx)} of
        {[L], [R]} -> Op(L, R);
        { L,  [R]} -> Op(L, R);
        {[L],  R } -> Op(L, R);
        { L,   R } -> Op(L, R)
    end.

op_eval({op, '@'}, Arg, _) ->
    Arg;
op_eval({op, Operand}, _, _) when is_binary(Operand) orelse is_number(Operand) ->
    Operand;
op_eval({op, Predicates}, Arg, Ctx) when is_list(Predicates) ->
    {Nodes, _ } = eval_step(Predicates, [Arg], Ctx),
    Nodes;
op_eval({op, {root, _} = SubQuery}, _, #{root := Root, opts := Options, funcs := Funcs}) ->
    {Nodes, _} = eval(SubQuery, Root, Funcs, Options),
    Nodes;
op_eval(Script, Node, Ctx) ->
    script_eval(Script, Node, Ctx).

argument(Node, Path) ->
    #argument{
        type = ejsonpath_common:type(Node),
        node = Node,
        path = Path
    }.
argument(Node, ParentPath, Key) ->
    #argument{
        type = ejsonpath_common:type(Node),
        node = Node,
        path = ejsonpath_common:buildpath(Key, ParentPath)
    }.

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

unzip(L) when is_list(L) -> 
    unzip_i(L, {[], []}).
unzip_i([], {Acc0, Acc1}) ->
    {lists:reverse(Acc0), lists:reverse(Acc1)};
unzip_i([ #argument{ node = Node, path = Path } | Rest], {AccNode, AccPath}) ->
    unzip_i(Rest, {[Node|AccNode], [Path|AccPath]}).

to_boolean(false) -> false;
to_boolean(<<>>) -> false;
to_boolean(0) -> false;
to_boolean([]) -> false;
to_boolean(_) -> true.