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
expr
axis steps step
predicate key_predicate
access_list 
filter_expr transform_expr
slice
script function_call function_args function_argument
operand bin_operator sint
.

Terminals
'$' '..' '.' '*' 
'[' ']' ',' ':' 
'?' '(' ')'
'+' '/' '-' 
'@' '==' '!=' '<' '>' '&&' '||' 
string key int
.

Rootsymbol expr.

Nonassoc 200 '=='.
Nonassoc 200 '!='.
Nonassoc 200 '<'.
Nonassoc 200 '>'.
Left 300 '+'.
Left 300 '-'.
Left 400 '/'.
Left 400 '*'.
Nonassoc 400 '&&'.
Nonassoc 400 '||'.

expr -> '$' steps : {root, '$2'}.
expr -> '$' axis  : {root, '$2'}.
expr -> '$'       : {root, '$'}.

steps -> step       : ['$1'].
steps -> step steps : ['$1' | '$2'].

step -> predicate          : {child, '$1'}.
step -> axis key_predicate : {'$1', '$2'}.

axis -> '.'  : child.
axis -> '..' : descendant.


predicate -> '[' filter_expr ']'    : {predicate, '$2'}.
predicate -> '[' transform_expr ']' : {predicate, '$2'}.
predicate -> '[' access_list ']'    : {predicate, '$2'}.
predicate -> '[' slice ']'          : {predicate, '$2'}.
predicate -> '[' '*' ']'            : {predicate, {key, value('$2')}}.

key_predicate -> key : {predicate, {key, value('$1')}}.
key_predicate -> int : {predicate, {key, integer_to_binary(value('$1'))}}.
key_predicate -> '*' : {predicate, {key, value('$1')}}.

%% ?(a=="b")
%% ?("ololol")
%% ?(100500)
%% ?(@==100500)
filter_expr -> '?' '(' script ')' : {filter_expr, '$3'}.

transform_expr -> '(' script ')' : {transform_expr, '$2'}.

script -> operand                    : '$1'.
script -> script bin_operator script : {bin_op, '$2', '$1', '$3'}.
script -> function_call              : '$1'.

function_call -> key '(' ')'               : {function_call, func_value('$1'), []}.
function_call -> key '(' function_args ')' : {function_call, func_value('$1'), '$3'}.

function_args -> function_argument                   : ['$1'].
function_args -> function_argument ',' function_args : ['$1' | '$3'].

%% TODO: function_argument == operand
function_argument -> string : value('$1').
function_argument -> sint   : value('$1').

operand -> '@'       : {op, value('$1')}.
operand -> '@' steps : {op, '$2'}.
operand -> int       : {op, value('$1')}.
operand -> string    : {op, value('$1')}.
operand -> expr      : {op, '$1'}.

bin_operator -> '==' : value('$1').
bin_operator -> '!=' : value('$1').
bin_operator -> '>'  : value('$1').
bin_operator -> '<'  : value('$1').
bin_operator -> '&&' : value('$1').
bin_operator -> '||' : value('$1').
bin_operator -> '+'  : value('$1').
bin_operator -> '-'  : value('$1').
bin_operator -> '/'  : value('$1').
bin_operator -> '*'  : value('$1').

%% [1:-1]
%% [1:-1:2]
%% [:-1]
%% [:]
slice -> sint ':' sint ':' int : {slice, value('$1'), value('$3'), value('$5')}.
slice -> sint ':' sint         : {slice, value('$1'), value('$3'), 1}.
slice -> sint ':'              : {slice, value('$1'), '$end', 1}.
slice -> ':' sint              : {slice, 0, value('$2'), 1}.
slice -> ':'                   : {slice, 0, '$end', 1}.

%% [1]
%% [1,2,3]
%% ["a","b"]
access_list -> sint                  : {access_list, [value('$1')]}.
access_list -> sint ',' access_list   : append_access_list(value('$1'), '$3').
access_list -> string                : {access_list, [value('$1')]}.
access_list -> string ',' access_list : append_access_list(value('$1'), '$3').

sint -> int     : '$1'.
sint -> '-' int : {int, 0, -value('$2')}.


Erlang code.
value({Token, _Line}) ->
    Token;
value({_Token, _Line, Value}) ->
    Value.

func_value({_Token, _Line, Value}) when is_binary(Value) ->
    erlang:binary_to_atom(Value, utf8).

append_access_list(Sint, {access_list, List}) ->
    {access_list, [Sint | List]}.
