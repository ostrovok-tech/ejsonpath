

Definitions.

Rules.

%% main rules
\$ : {token, {'$', TokenLine}}.
\.\. : {token, {'..', TokenLine}}.
\@ : {token, {'@', TokenLine}}.
\[ : {token, {'[', TokenLine}}.
\] : {token, {']', TokenLine}}.
\. : {token, {'.', TokenLine}}.

[a-zA-Z][a-zA-Z0-9_]* : {token, {key, TokenLine, list_to_binary(TokenChars)}}.
[0-9]+ : {token, {int, TokenLine, list_to_integer(TokenChars)}}.

%% indexing
\* : {token, {'*', TokenLine}}.
\, : {token, {',', TokenLine}}.
\: : {token, {':', TokenLine}}.

%% scripting
\? : {token, {'?', TokenLine}}.
\( : {token, {'(', TokenLine}}.
\) : {token, {')', TokenLine}}.

\"[^"]*\" : {token, {string, TokenLine, list_to_binary(string:substr(TokenChars, 2, TokenLen - 2))}}.
\'[^']*\' : {token, {string, TokenLine, list_to_binary(string:substr(TokenChars, 2, TokenLen - 2))}}.

== : {token, {'==', TokenLine}}.
&& : {token, {'&&', TokenLine}}.
\+ : {token, {'+', TokenLine}}.
\- : {token, {'-', TokenLine}}.

[\s]+ : skip_token.

Erlang code.
