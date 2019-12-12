eJSONPath - jsonpath for erlang
===============================

eJSONPath is pure-erlang implementation of [JSONPath](http://goessner.net/articles/JsonPath/).
It supports both `map()` and jiffy JSON structure (`{[ {key(), value()}, ...]}` for kv-objects)
and implements most of the JSONPath description (I don't say specification, because there is no
such thing like jsonpath spec).

* Robust extensible parser (leex + yecc).
* Extensible by custom functions.
* No dependencies (but you may want jiffy or any other json parser).

Examples
--------

```erlang
{ok, Bin} = file:read_file("test/doc.json").
Doc = jiffy:decode(Bin, [return_maps]).

%% return 1'st book author
{[<<"Nigel Rees">>], ["$['store']['book'][0]['author']"]} = ejsonpath:q("$.store.book[0].author", Doc).

%% return 1'st book categody and author
{[<<"reference">>,<<"Nigel Rees">>],
 ["$['store']['book'][0]['category']",
  "$['store']['book'][0]['author']"]} = ejsonpath:q("$.store.book[0]['category','author']", Doc).

%% return only reference book authors
%% `Funs' is a map or propist of `Name` and `Fun` pairs (see Fun spec in the sources)
Funs = #{
 filter_category =>
  fun({Map, _Doc}, [CategoryName]) ->
     case maps:find(<<"category">>, Map) of
         {ok, CategoryName} -> true;
         _ -> false
     end
  end
},
{[<<"Nigel Rees">>],["$['store']['book'][0]['author']"]} = ejsonpath:q("$.store.book[?(filter_category('reference'))].author", Doc, Funs).

%% update only item id == 0
O = #{
  <<"items">> => [
    #{<<"id">> => 0, <<"value">> => yyy},
    #{<<"id">> => 1, <<"value">> => yyy}
  ]
},

{#{<<"items">> =>
       [#{<<"id">> => 0,<<"value">> => xxx},
        #{<<"id">> => 1,<<"value">> => yyy}]},
 ["$['items'][0]['value']"]} = ejsonpath:tr("$.items[?(@.id == 0)].value", O, fun(_) -> xxxend).

```
More examples in tests.

JSONPath coverage
-----------------

Since there is no such thing as JSONPath specification, every implementer create
it's own variations. But I try to follow description from [JSONPath](http://goessner.net/articles/JsonPath/)
as close as possible.

```
+-----------------------+------------------------+-----------+
| Feature               | Example                |Implemented|
+-----------------------+------------------------+-----------+
|Dot filtering          |`$.one.two`             | Y         |
+-----------------------+------------------------+-----------+
|Brace filtering        |`$['one']['two']`       | Y         |
+-----------------------+------------------------+-----------+
|Array slicing          | `$[1,2,3]` `$.o[2]`    | Y         |
+-----------------------+------------------------+-----------+
|Hash slicing           | `$['one', 'two']`      | Y         |
+-----------------------+------------------------+-----------+
|Asterisk (hash, array) | `$.one.*` `$.one[*]`   | Y         |
+-----------------------+------------------------+-----------+
|Python-slicing         | `$[1:-1:2]`            | Partial*  |
+-----------------------+------------------------+-----------+
|Eval binary filter     | `$[?(true)]`           | Partial** |
|                       | `$[?(@.cat != 'mew')]` |           |
+-----------------------+------------------------+-----------+
|Eval index             | `$[('one')]`           | Partial** |
+-----------------------+------------------------+-----------+
|Recursive descent      | `..`                   | Y         |
+-----------------------+------------------------+-----------+

* Only step=1 supported now
** Limited scripting language: string, integer, binary operators and function calls
```

Most of the missing features can be implemented as custom functions

```
$.one[?( custom_function_call('arg1', 42) )]
$.two[( custom_function_call() )]
````

TODO
----

* Implement missing features
* Python slicing step support. (Currently only step==1 supported)
* Eval filter / index - allow path expressions and operators `$[?(@.category)]`

Other implementations
---------------------

* [GeneStevens/jsonpath](https://github.com/GeneStevens/jsonpath) - very simple Erlang implementation.
  Support only dot-keys and integer indexes `one[0].two.three`.
* [Reference JS implementation](https://code.google.com/p/jsonpath/source/browse/trunk/src/js/jsonpath.js)
  Reference implementation. Implemented as mix of regexps and javascript eval's.
* [Java implementaion](https://code.google.com/p/json-path/) - powerfull Java implementation
* [Limited python impl](https://github.com/kennknowles/python-jsonpath-rw) - limited Python implementation
* [Online evaluator](http://ashphy.com/JSONPathOnlineEvaluator/) - realy useful only with patch
`window.dump = function(v) { return JSON.stringify(v, null, "    ") }`

License
-------

Apache v2
