%% "Spec" http://goessner.net/articles/JsonPath/
%% Online tester http://ashphy.com/JSONPathOnlineEvaluator/
%% Java impl https://github.com/jayway/JsonPath

%% $.store.book[*].author
%% $..author
%% $.store.*
%% $.store..price
%% $..book[2]
%% $..book[(@.length-1)]
%% $..book[-1:]
%% $..book[0,1]
%% $..book[:2]
%% $..book[?(@.isbn)]
%% $..book[?(@.price<10)]
%% $..*
%% $..book[?(@.ratings><'good')]

%% $['store']['book'][*]['category']
%% $.store.book.*.category

%% { "store": {
%%     "book": [ 
%%       { "category": "reference",
%%         "author": "Nigel Rees",
%%         "title": "Sayings of the Century",
%%         "price": 8.95
%%       },
%%       { "category": "fiction",
%%         "author": "Evelyn Waugh",
%%         "title": "Sword of Honour",
%%         "price": 12.99
%%       },
%%       { "category": "fiction",
%%         "author": "Herman Melville",
%%         "title": "Moby Dick",
%%         "isbn": "0-553-21311-3",
%%         "price": 8.99
%%       },
%%       { "category": "fiction",
%%         "author": "J. R. R. Tolkien",
%%         "title": "The Lord of the Rings",
%%         "isbn": "0-395-19395-8",
%%         "price": 22.99
%%       }
%%     ],
%%     "bicycle": {
%%       "color": "red",
%%       "price": 19.95
%%     }
%%   }
%% }

Nonterminals
expr axis
predicate step steps raw_predicate
slice comma_slice sint index_expr
binary_expr script operand bin_operator
function_call function_args function_argument.

Terminals
'$' '..' '@' '[' ']' '.' key int
'*' ',' ':'
'?' '(' ')' string '==' '&&' '+' '-'.

Rootsymbol expr.


expr -> '$' steps : {root, '$2'}.

steps -> step : {steps, ['$1']}.
steps -> step steps : append_step('$1', '$2').

step -> predicate : {child, '$1'}.
step -> axis raw_predicate : {'$1', '$2'}.  %% WTF?

axis -> '.' : child.
axis -> '..' : descendant.

predicate -> '[' binary_expr ']' : {refine, '$2'}.
predicate -> '[' index_expr ']' : {refine, '$2'}.
predicate -> '[' slice ']' : {refine, '$2'}.
predicate -> '[' comma_slice ']' : {refine, '$2'}.
predicate -> '[' '*' ']' : {refine, value('$2')}.
predicate -> '[' string ']' : {refine, value('$2')}.

raw_predicate -> key : {refine, value('$1')}.
raw_predicate -> '*' : {refine, value('$1')}.

%% ?(a=="b")
%% ?("ololol")
%% ?(100500)
%% ?(@==100500)
binary_expr -> '?' '(' script ')' : {bin_expr, '$3'}.

index_expr -> '(' script ')' : {index_expr, '$2'}.

script -> operand : '$1'.
script -> script bin_operator script : {bin_op, '$2', '$1', '$3'}.
script -> function_call : '$1'.

function_call -> key '(' ')' : {function_call, value('$1'), []}.
function_call -> key '(' function_args ')' : {function_call, value('$1'), '$3'}.

function_args -> function_argument : ['$1'].
function_args -> function_argument ',' function_args : ['$1' | '$3'].

%% TODO: function_argument == operand
function_argument -> string : value('$1').
function_argument -> sint : value('$1').

operand -> string : value('$1').
operand -> '@' : value('$1').
operand -> int : value('$1').
%% operand -> expr.

bin_operator -> '==' : value('$1').
bin_operator -> '&&' : value('$1').
bin_operator -> '+' : value('$1').
bin_operator -> '-' : value('$1').

%% [1:-1]
%% [1:-1:2]
%% [:-1]
%% slice -> sint : {slice, value('$1'), value('$1'), 1}.
slice -> int ':' sint ':' int : {slice, value('$1'), value('$3'), value('$5')}.
slice -> int ':' sint : {slice, value('$1'), value('$3'), 1}.
slice -> int ':' : {slice, value('$1'), -1, 1}.
slice -> ':' sint : {slice, 0, value('$2'), 1}.

%% [1]
%% [1,2,3]
%% ["a","b"]
comma_slice -> sint : {slice_list, [value('$1')]}.
comma_slice -> sint ',' comma_slice : append_comma_slice(value('$1'), '$3').
comma_slice -> string : {slice_list, [value('$1')]}.
comma_slice -> string ',' comma_slice : append_comma_slice(value('$1'), '$3').

sint -> int : '$1'.
sint -> '-' int : {int, 0, -value('$2')}.


Erlang code.
value({Token, _Line}) ->
    Token;
value({_Token, _Line, Value}) ->
    Value.

append_comma_slice(Sint, {slice_list, List}) ->
    {slice_list, [Sint | List]}.

append_step(Step, {steps, Steps}) ->
    {steps, [Step | Steps]}.
