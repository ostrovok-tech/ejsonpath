%%% Copyright 2013 Sergey Prokhorov
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

%%% @author Sergey Prokhorov <me@seriyps.ru>
%%% Created : 12 Aug 2013 by Sergey Prokhorov <me@seriyps.ru>

-module(ejsonpath).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

-export([q/2, q/3, q/4]).
-export([tr/3, tr/4, tr/5]).

-export_type([json_node/0, jsonpath_funcspecs/0, jsonpath_func/0]).

-type jsonpath() :: string().
-type json_node() 
    :: null                        % null
    | boolean()                    % true/false
    | binary()                     % string
    | number()                     % int/float
    | [json_node()]                % array
    | #{binary() => json_node()}.  % hash (object)

-type jsonpath_funcspecs() :: #{ Name :: atom() => Fun :: jsonpath_func() }.

-type jsonpath_func() 
    :: jsonpath_transform_func() 
    | jsonpath_filter_func().

-type jsonpath_transform_func() 
    :: fun( ({CurrentNode :: json_node(), RootNode :: json_node()}, Args :: [any()] ) ->
    Return :: json_node() ).

-type jsonpath_filter_func() 
    :: fun( ( { CurrentNode :: json_node(), RootNode :: json_node()}, Args :: [any()] ) ->
    Return :: boolean() ).
        

q(Query, Root) ->
    q(Query, Root, #{}, []).
q(Query, Root, Functions) ->
    q(Query, Root, Functions, []).

-spec q(jsonpath(), json_node(), jsonpath_funcspecs(), [any()]) -> {[json_node()], [string()]}.

q(Query, Root, Functions, Options) ->
    {ok, Tokens, _} = ejsonpath_scan:string(Query),
    {ok, Tree}      = ejsonpath_parse:parse(Tokens),
    %% try
    ejsonpath_eval:eval(Tree, Root, Functions, Options)
    %% catch Class:Reason ->
    %%         io:format(user, "~p~n~p~n~p~n",
    %%                   [Class, Reason, erlang:get_stacktrace()]),
    %%         {error, Class, Reason, erlang:get_stacktrace()}
    %% end
    .

tr(Query, Root, Transform) ->
    tr(Query, Root, Transform, #{}, []).
tr(Query, Root, Transform, Functions) ->
    tr(Query, Root, Transform, Functions, []).
tr(Query, Root, Transform, Functions, Options) ->
    {ok, Tokens, _} = ejsonpath_scan:string(Query),
    {ok, Tree}      = ejsonpath_parse:parse(Tokens),
    %% try
    ejsonpath_transform:transform(Tree, Root, Transform, Functions, Options)
    %% catch Class:Reason ->
    %%         io:format(user, "~p~n~p~n~p~n",
    %%                   [Class, Reason, erlang:get_stacktrace()]),
    %%         {error, Class, Reason, erlang:get_stacktrace()}
    %% end
    .