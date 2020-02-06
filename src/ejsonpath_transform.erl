-module(ejsonpath_transform).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

% -define(EJSONPATH_DEBUG, true).
-include("./ejsonpath.hrl").
-import(ejsonpath_common, [argument/2, argument/3, script_eval/3]).

-export([
    transform/5
]).

transform({root, '$'}, Node, Transform, _, _) ->
    case Transform({match, to_arg(argument(Node, "$"))}) of
        {error, Error} -> erlang:error(Error);
        {ok, Result} -> {Result, ["$"]};
        delete -> erlang:error(not_supported);

        Invalid -> erlang:error({badreturn, Invalid})
    end;

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
    transform_step(Predicates, argument(Node, "$"), Context).

% {key, '*'}
transform_step([{child, {predicate, {key, '*'}}} | Rest], #argument{type = hash, node = Node, path = _Path} = Arg, Ctx) ->
    ?EJSONPATH_LOG({enter, key, '*', _Path}),
    Keys = maps:keys(Node),
    transform_step([{child, {predicate, {access_list, Keys}}}] ++ Rest, Arg, Ctx);

transform_step([{child, {predicate, {key, '*'}}} | Rest], #argument{type = array, node = Node, path = _Path} = Arg, Ctx) ->
    ?EJSONPATH_LOG({enter, key, '*', _Path}),
    Idxs = lists:seq(0, erlang:length(Node)-1),
    transform_step([{child, {predicate, {access_list, Idxs}}}] ++ Rest, Arg, Ctx);
transform_step([{child, {predicate, {key, Key}}} | Rest],
               #argument{type = array, node = _Node, path = _Path} = Arg, Ctx) when is_integer(Key) ->
    ?EJSONPATH_LOG({enter, key, Key, _Path}),
    transform_step([{child, {predicate, {access_list, [Key]}}}] ++ Rest, Arg, Ctx);

transform_step([{child, {predicate, {key, Key}}} | Rest], #argument{type = hash, path = Path} = Arg, Ctx) when is_integer(Key) ->
    ?EJSONPATH_LOG({enter, key, Key, Path}),
    apply_transform([integer_to_binary(Key)], Arg, fun(_, Value) -> 
        transform_step(Rest, argument(Value, Path, integer_to_binary(Key)), Ctx)
    end, Ctx, Rest /= []);

% {key, Key}
transform_step([{child, {predicate, {key, Key}}} | Rest], #argument{type = hash, path = Path} = Arg, Ctx) ->
    ?EJSONPATH_LOG({enter, key, Key, Path}),
    apply_transform([Key], Arg, fun(_, Value) -> 
        transform_step(Rest, argument(Value, Path, Key), Ctx)
    end, Ctx, Rest /= []);
% {access_list, Keys}
transform_step([{child, {predicate, {access_list, Keys}}} | Rest], #argument{path = Path} = Arg, Ctx) ->
    ?EJSONPATH_LOG({enter, access_list, Keys, Path}),
    apply_transform(Keys, Arg, fun(Key, Value) -> 
        transform_step(Rest, argument(Value, Path, Key), Ctx)
    end, Ctx, Rest /= []);

% {filter_expr, Script}
transform_step([{child, {predicate, {filter_expr, Script}}} | Rest], #argument{type = hash, node = Node, path = Path} = Arg, Ctx) ->
    ?EJSONPATH_LOG({enter, filter_expr, hash, Path, Script}),
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
            end, Ctx, Rest /= [])
        end;
transform_step([{child, {predicate, {filter_expr, Script}}}|Rest], #argument{type = array, node = Node, path = Path} = Arg, Ctx) ->
    ?EJSONPATH_LOG({enter, filter_expr, array, Path, Script}),
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
            end, Ctx, Rest /= [])
    end;
transform_step([{child, {predicate, {filter_expr, Script}}} | Rest], #argument{path = _Path} = Arg, Ctx) ->
    ?EJSONPATH_LOG({enter, filter_expr, _Path, Script}),
    case ejsonpath_common:to_boolean(script_eval(Script, Arg, Ctx)) of
        false -> erlang:error(not_found);
        _ -> transform_step(Rest, Arg, Ctx)
    end;

%% {slice, S, E, Step}
transform_step([{child, {predicate, {slice, Start, End, Step}}} | Rest], #argument{type = array, node = Node} = Arg, Ctx) ->
    ?EJSONPATH_LOG({slice, Start, End, Step}),
    case ejsonpath_common:slice_seq(Start, End, Step, length(Node)) of
        {error, _} -> [];
        Seq ->
            transform_step([{child, {predicate, {access_list, Seq}}}] ++ Rest, Arg, Ctx)
    end;

transform_step([], #argument{node = _Node, path = Path} = Arg, #{transform := Transform}) ->
    ?EJSONPATH_LOG({match, _Node, Path}),
    
    case Transform({match, to_arg(Arg)}) of
        {error, Error} -> erlang:error(Error);
        {ok, Result} -> {Result, [Path]};
        delete -> {'$delete', [Path]};
        
        Invalid -> erlang:error({badreturn, Invalid})
    end;

transform_step(_Pr, _A, _) ->
    ?EJSONPATH_LOG({not_implemented, _Pr, _A}),
    erlang:error(not_implemented).

apply_transform(Keys, #argument{type = hash, node = Acc0, path = Path} = Arg, Func, Ctx, HaveOngoingQuery) ->
    lists:foldl(fun (Key, {Acc, Paths}) -> 
        case maps:get(Key, Acc, '$undefined') of
            '$undefined' -> 
                case HaveOngoingQuery of
                    true -> erlang:error(not_found);
                    false ->
                        % we handle not_found event only if last query item
                        %% if no more query and item doesn't exists
                        PathToBe = ejsonpath_common:buildpath(Key, Path),
                        NotFoundHandler = notfound_func(Ctx, fun(_) -> erlang:error(not_found) end),
                        case NotFoundHandler({not_found, PathToBe, Key, to_arg(Arg)}) of
                            {error, Err} -> erlang:error(Err);
                            {ok, NewAcc} -> {NewAcc, Paths ++ [PathToBe]};
                        
                            % on not_found and result is delete command
                            % we return the current node (without the key)
                            delete -> {Acc, Paths ++ [PathToBe]};

                            Invalid -> erlang:error(Invalid)
                        end
                end;
            Value -> 
                case Func(Key, Value) of
                    {'$delete', NewPaths} ->
                        {maps:remove(Key, Acc), Paths ++ NewPaths};
                    {NewValue, NewPaths} ->
                        {maps:put(Key, NewValue, Acc), Paths ++ NewPaths}
                end
        end
    end, {Acc0, []}, Keys);

apply_transform(Idxs, #argument{type = array, node = Acc0, path = Path} = Arg, Func, Ctx, HaveOngoingQuery) ->
    {NewNode, NewPaths} = lists:foldl(fun (Idx0, {Acc, Paths}) -> 
        case ejsonpath_common:index(Idx0, erlang:length(Acc)) of
            {error, _} -> 
                case HaveOngoingQuery of
                    true -> erlang:error(not_found);
                    false -> 
                        PathToBe = ejsonpath_common:buildpath(Idx0, Path),
                        NotFoundHandler = notfound_func(Ctx, fun(_) -> erlang:error(not_found) end),
                        case NotFoundHandler({not_found, PathToBe, Idx0, to_arg(Arg)}) of
                            {error, Err} -> erlang:error(Err);
                            {ok, NewAcc} -> {NewAcc, Paths ++ [PathToBe]};
                            delete -> {Acc, Paths ++ [PathToBe]};

                            Invalid -> erlang:error(Invalid)
                        end
                end;
            {ok, Idx1} ->
                case Func(Idx0, lists:nth(Idx1, Acc)) of
                    {'$delete', NewPaths } ->
                        {ejsonpath_common:insert_list(Idx1, '$deletion_marker', Acc), Paths ++ NewPaths};
                    {Value, NewPaths} -> 
                        {ejsonpath_common:insert_list(Idx1, Value, Acc), Paths ++ NewPaths}
                end
        end
    end, {Acc0, []}, Idxs),
    {lists:filter(fun (X) -> X /= '$deletion_marker' end, NewNode), NewPaths};
apply_transform(_,_,_,_,_) ->
    erlang:error(not_found).

to_arg(#argument{type = Type, node = Node, path = Path}) ->
    #{ type => Type, node => Node, path => Path}.

notfound_func(#{opts := Options, transform := Transform}, DefaultFunc) ->
    case proplists:get_value(handle_not_found, Options, DefaultFunc) of
        true -> Transform;
        Func when is_function(Func) -> Func;
        
        _Invalid -> erlang:error(badarg)
    end.