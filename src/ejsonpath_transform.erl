-module(ejsonpath_transform).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-export([
    transform/5
]).

transform({root, '$'}, RootNode, Transform, _, _) ->
    {Transform(RootNode), ["$"]};

transform({root, Predicates}, RootNode, Transform, Funcs, Options) ->
    Context = #{
        root => RootNode,
        transform => Transform,
        opts => Options,
        funcs => Funcs
    },
    %try
    transform_step(Predicates, ejsonpath_common:type(RootNode), RootNode, "$", Context)
    %catch
    %    Class:Reason:_ -> {Class, Reason}
    %end
    .

transform_step([{child, {predicate, {key, '*'}}} | Rest], hash, Node, Path, Ctx) ->
    erlang:display({enter, key, '*', Path, Node}),
    Keys = maps:keys(Node),
    transform_step([{child, {predicate, {access_list, Keys}}}] ++ Rest, array, Node, Path, Ctx);

transform_step([{child, {predicate, {key, '*'}}} | Rest], array, Node, Path, Ctx) ->
    erlang:display({enter, key, '*', Path, Node}),
    
    Idxs = lists:seq(0, erlang:length(Node)-1),
    transform_step([{child, {predicate, {access_list, Idxs}}}] ++ Rest, array, Node, Path, Ctx);

transform_step([{child, {predicate, {key, Key}}} | Rest], hash, Node, Path, Ctx) ->
    erlang:display({enter, key, Key, Path, Node}),
    
    apply_transform([Key], hash, Node, fun(_, Value) -> 
        transform_step(Rest, ejsonpath_common:type(Value), Value, ejsonpath_common:buildpath(Key, Path), Ctx)
    end, Ctx);

transform_step([{child, {predicate, {access_list, Keys}}} | Rest], Type, Node, Path, Ctx) ->
    erlang:display({enter, access_list, Keys, Path}),
    
    apply_transform(Keys, Type, Node, fun(Key, Value) -> 
        transform_step(Rest, ejsonpath_common:type(Value), Value, ejsonpath_common:buildpath(Key, Path), Ctx)
    end, Ctx);

transform_step([], _, Node, Path, #{transform := Transform}) ->
    erlang:display({match, Node, Path}),
    {Transform(Node), [Path]};

transform_step(_Pr, _T, _N, _P, _) ->
    erlang:display({not_implemented, _Pr, _T, _N, _P}),
    erlang:error(not_implemented).


apply_transform(Keys, hash, Acc0, Func, _) ->
    lists:foldl(fun (Key, {Acc, Paths}) -> 
        case maps:get(Key, Acc, '$undefined') of
            '$undefined' -> erlang:error(not_found);
            Value -> 
                {NewValue, NewPaths} = Func(Key, Value),
                {maps:put(Key, NewValue, Acc), Paths ++ NewPaths}
        end
    end, {Acc0, []}, Keys);

apply_transform(Idxs, array, Acc0, Func, _) ->
    lists:foldl(fun (Idx0, {Acc, Paths}) -> 
        case ejsonpath_common:index(Idx0, erlang:length(Acc)) of
            {error, Err} -> erlang:error(Err);
            {ok, Idx1} ->
                {Value, NewPaths} = Func(Idx0, lists:nth(Idx1, Acc)),
                {ejsonpath_common:insert_list(Idx1, Value, Acc), Paths ++ NewPaths}
        end
    end, {Acc0, []}, Idxs);
apply_transform(_, _, _, _, _) ->
    erlang:error(not_found).

apply_script(Key, _, _) when is_binary(Key) ->
    Key;
apply_script(Idx, _, _) when is_number(Idx) ->
    Idx;
apply_script({function_call, Name, Args}, CurrentNode, #{funcs := Funs, root := RootNode}) ->
    Fun = maps:get(Name, Funs),
    Fun({CurrentNode, RootNode}, Args).