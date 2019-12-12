-module(ejsonpath_transform).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-include("./ejsonpath.hrl").
-import(ejsonpath_common, [argument/2, argument/3, script_eval/3]).

-export([
    transform/5
]).

transform({root, '$'}, Node, Transform, _, _) ->
    {Transform(Node), ["$"]};

transform({root, Predicates}, Node, Transform, Funcs, Options) ->
    Context = #{
        root => Node,
        opts => Options,
        funcs => Funcs,

        transform => Transform,
         eval_root => fun (SubQuery) -> 
            ejsonpath_eval:eval(SubQuery, Node, Funcs, Options)
        end,
        eval_step => fun (SubQuery, CurrNode, Ctx) -> 
            ejsonpath_eval:eval_step(SubQuery, [CurrNode], Ctx)
        end
    },
    %try
    transform_step(Predicates, argument(Node, "$"), Context)
    %catch
    %    Class:Reason:_ -> {Class, Reason}
    %end
    .

% {key, '*'}
transform_step([{child, {predicate, {key, '*'}}} | Rest], #argument{type = hash, node = Node, path = Path} = Arg, Ctx) ->
    % erlang:display({enter, key, '*', Path, Node}),
    Keys = maps:keys(Node),
    transform_step([{child, {predicate, {access_list, Keys}}}] ++ Rest, Arg, Ctx);

transform_step([{child, {predicate, {key, '*'}}} | Rest], #argument{type = array, node = Node, path = Path} = Arg, Ctx) ->
    % erlang:display({enter, key, '*', Path, Node}),
    Idxs = lists:seq(0, erlang:length(Node)-1),
    transform_step([{child, {predicate, {access_list, Idxs}}}] ++ Rest, Arg, Ctx);

% {key, Key}
transform_step([{child, {predicate, {key, Key}}} | Rest], #argument{type = hash, node = Node, path = Path} = Arg, Ctx) ->
    % erlang:display({enter, key, Key, Path, Node}),
    apply_transform([Key], Arg, fun(_, Value) -> 
        transform_step(Rest, argument(Value, Path, Key), Ctx)
    end, Ctx);
% {access_list, Keys}
transform_step([{child, {predicate, {access_list, Keys}}} | Rest], #argument{path = Path} = Arg, Ctx) ->
    % erlang:display({enter, access_list, Keys, Path}),
    apply_transform(Keys, Arg, fun(Key, Value) -> 
        transform_step(Rest, argument(Value, Path, Key), Ctx)
    end, Ctx);

% {filter_expr, Script}
transform_step([{child, {predicate, {filter_expr, Script}}} | Rest], #argument{type = hash, node = Node, path = Path} = Arg, Ctx) ->
    % erlang:display({enter, filter_expr, hash, Script, Path}),
    Keys = lists:reverse(maps:fold(fun (Key, Value, Acc) -> 
        case ejsonpath_common:to_boolean(script_eval(Script, argument(Value, Path, Key), Ctx)) of
            false -> Acc;
            _ -> [Key|Acc]
        end
    end, [], Node)),

    case Keys of
        [] -> erlang:error(not_found);
        _ -> 
            apply_transform(Keys, Arg, fun(Key, Value) -> 
                transform_step(Rest, argument(Value, Path, Key), Ctx)
            end, Ctx)
        end;
transform_step([{child, {predicate, {filter_expr, Script}}}|Rest], #argument{type = array, node = Node, path = Path} = Arg, Ctx) ->
    % erlang:display({enter, filter_expr, array, Script, Path}),
    {_, Idxs} = lists:foldl(fun (Item, {Idx, Acc}) ->
        case ejsonpath_common:to_boolean(script_eval(Script, argument(Item, Path, Idx), Ctx)) of
            false -> {Idx+1, Acc};
            _ -> {Idx+1, [Idx|Acc]}
        end
    end, {0, []}, Node),

    case Idxs of
        [] -> erlang:error(not_found);
        _ -> 
            apply_transform(lists:reverse(Idxs), Arg, fun(Idx, Value) -> 
                transform_step(Rest, argument(Value, Path, Idx), Ctx)
            end, Ctx)
    end;
transform_step([{child, {predicate, {filter_expr, Script}}} | Rest], #argument{path = Path} = Arg, Ctx) ->
    % erlang:display({enter, filter_expr, Script, Path}),
    case ejsonpath_common:to_boolean(script_eval(Script, Arg, Ctx)) of
        false -> erlang:error(not_found);
        _ -> transform_step(Rest, Arg, Ctx)
    end;

%% {slice, S, E, Step}
transform_step([{child, {predicate, {slice, Start, End, Step}}}|Rest], #argument{type = array, node = Node} = Arg, Ctx) ->
    case ejsonpath_common:slice_seq(Start, End, Step, length(Node)) of
        {error, _} -> [];
        Seq ->
            transform_step([{child, {predicate, {access_list, Seq}}}] ++ Rest, Arg, Ctx)
    end;

transform_step([], #argument{node = Node, path = Path}, #{transform := Transform}) ->
    % erlang:display({match, Node, Path}),
    {Transform(Node), [Path]};

transform_step(_Pr, _A, _) ->
    % erlang:display({not_implemented, _Pr, _A}),
    erlang:error(not_implemented).

apply_transform(Keys, #argument{type = hash, node = Acc0}, Func, _) ->
    lists:foldl(fun (Key, {Acc, Paths}) -> 
        case maps:get(Key, Acc, '$undefined') of
            '$undefined' -> erlang:error(not_found);
            Value -> 
                {NewValue, NewPaths} = Func(Key, Value),
                {maps:put(Key, NewValue, Acc), Paths ++ NewPaths}
        end
    end, {Acc0, []}, Keys);

apply_transform(Idxs, #argument{type = array, node = Acc0}, Func, _) ->
    lists:foldl(fun (Idx0, {Acc, Paths}) -> 
        case ejsonpath_common:index(Idx0, erlang:length(Acc)) of
            {error, Err} -> erlang:error(Err);
            {ok, Idx1} ->
                {Value, NewPaths} = Func(Idx0, lists:nth(Idx1, Acc)),
                {ejsonpath_common:insert_list(Idx1, Value, Acc), Paths ++ NewPaths}
        end
    end, {Acc0, []}, Idxs);
apply_transform(_,_,_,_) ->
    erlang:error(not_found).