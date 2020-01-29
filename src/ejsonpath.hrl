-ifndef(_ejsonpath_h_).
-define(_ejsonpath_h_, true).

-record(argument, {type, node, path}).

-ifdef(EJSONPATH_DEBUG).
-define(EJSONPATH_LOG(X), erlang:display({?MODULE, ?LINE, X})).
-else.
-define(EJSONPATH_LOG(_),true).
-endif.
-endif.