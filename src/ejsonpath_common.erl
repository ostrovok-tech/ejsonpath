-module(ejsonpath_common).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-include("./ejsonpath.hrl").

-export([
    buildpath/2,
    argument/2,
    argument/3,
    unzip/1,
    type/1,

    insert_list/3,
    slice_seq/4,
    index/2,

    script_eval/3,
    to_boolean/1
]).

buildpath(Key, Path) when is_binary(Key) ; is_list(Key) ; is_atom(Key) ->
    lists:flatten(io_lib:format("~s['~s']", [Path, Key]));
buildpath(Idx, Path) when is_number(Idx) ->
    lists:flatten(io_lib:format("~s[~p]", [Path, Idx])).

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

unzip(L) when is_list(L) -> 
    unzip_i(L, {[], []}).
unzip_i([], {Acc0, Acc1}) ->
    {lists:reverse(Acc0), lists:reverse(Acc1)};
unzip_i([ #argument{ node = Node, path = Path } | Rest], {AccNode, AccPath}) ->
    unzip_i(Rest, {[Node|AccNode], [Path|AccPath]}).

type(L) when is_list(L) ->
    array;
type(Map) when is_map(Map) ->
    hash;
type(Bin) when is_binary(Bin) ->
    string;
type(Bool) when is_boolean(Bool) ->
    boolean;
type(Num) when is_number(Num) ->
    number;
type(null) ->
    null;
type(Atom) when is_atom(Atom) ->
    string.

insert_list(Idx, Value, List) when Idx > 0, Idx =< length(List) ->
    insert_list_i(Idx, Value, List);
insert_list(_, _, _) ->
    erlang:error(badarg).

slice_seq(Start, End, Step, Sz) when (Step > 0) andalso (Sz >= 0) ->
    slice_seq_i(Start, End, Step, Sz);
slice_seq(_, _, _, _) ->
    {error, badarg}.

slice_seq_i(Start, '$end', Step, Sz) ->
    slice_seq_i(Start, Sz, Step, Sz);
slice_seq_i(Start, End, Step, Sz) 
    when Start < 0 ->
    slice_seq_i(max(Sz+Start, 0), End, Step, Sz);
slice_seq_i(Start, End, Step, Sz) 
    when End < 0 ->
    slice_seq_i(Start, min(Sz+End, Sz), Step, Sz);
slice_seq_i(Start, End, Step, Sz) 
    when (Start >= 0) andalso (End >= 0) ->

    % range: [S, E)
    S = min(Start, Sz),
    E = min(End, Sz),
    lists:seq(S, E, Step) -- [E];
slice_seq_i(_, _, _, _) ->
    {error, badarg}.

insert_list_i(Idx, Value, List) ->
    lists:sublist(List, Idx-1) ++ 
    [ Value ] ++ 
    lists:nthtail(Idx, List).
index(N, Sz) when is_number(N), N >= 0 ->
    index_i(N+1, Sz);
index(N, Sz) when is_number(N) ->
    index_i(Sz+N+1, Sz);
index(_, _) ->
    {error, badarg}.
index_i(N, Sz) when N > 0, N =< Sz ->
    {ok, N};
index_i(_,_) ->
    {error, badarg}.

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
op_eval({op, Predicates}, Arg, #{eval_step := EvalStep} = Ctx) when is_list(Predicates) ->
    {Nodes, _ } = EvalStep(Predicates, Arg, Ctx),
    Nodes;
op_eval({op, {root, _} = SubQuery}, _, #{eval_root := EvalRoot}) ->
    {Nodes, _} = EvalRoot(SubQuery),
    Nodes;
op_eval(Script, Node, Ctx) ->
    script_eval(Script, Node, Ctx).

to_boolean(false) -> false;
to_boolean(<<>>) -> false;
to_boolean(0) -> false;
to_boolean([]) -> false;
to_boolean(_) -> true.

