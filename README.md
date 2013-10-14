eJSONPath - jsonpath for erlang
===============================

eJSONPath is pure-erlang implementation of [JSONPath](http://goessner.net/articles/JsonPath/).
It uses jiffy JSON structure (`{[ {key(), value()}, ...]}` for structs) and implements most of the
JSONPath description (I don't say specification, because there is no such thing like jsonpath spec).

* Robust extensible parser (leex + yecc).
* Extensible by custom functions.
* No dependencies (but you may want jiffy as json parser).

Examples
--------

```erlang
{ok, Bin} = file:read_file("test/doc.json").
Doc = jiffy:decode(Bin).

%% return 1'st book author
[<<"Nigel Rees">>] = ejsonpath:execute("$.store.book[0].author", Doc).

%% return 1'st book categody and author
[<<"reference">>,
 <<"Nigel Rees">>] = ejsonpath:execute("$.store.book[0]['category','author']", Doc).

%% return only reference book authors
%% `Funs' is a list of `{Name, Fun}' pairs (see Fun spec on sources)
Funs = [
{<<"filter_category">>,
 fun({{Pairs}, _Doc}, [CategoryName]) ->
     case proplists:get_value(<<"category">>, Pairs) of
         CategoryName -> true;
         _ -> false
     end
 end}
],
[<<"Nigel Rees">>] = ejsonpath:execute(
                     "$.store.book[?(filter_category('reference'))].author", Doc, Funs).
```
More examples in tests.

JSONPath coverage
-----------------

Since there is no such thing as JSONPath specification, every implementer create
it's own variations. But I try to follow description from [JSONPath](http://goessner.net/articles/JsonPath/)
as close as possible.

```
+-----------------------+---------------------+-----------+
| Feature               | Example             |Implemented|
+-----------------------+---------------------+-----------+
|Dot filtering          |`$.one.two`          | Y         |
+-----------------------+---------------------+-----------+
|Brace filtering        |`$['one']['two']`    | Y         |
+-----------------------+---------------------+-----------+
|Array slicing          | `$[1,2,3]` `$.o[2]` | Y         |
+-----------------------+---------------------+-----------+
|Hash slicing           | `$['one', 'two']`   | Y         |
+-----------------------+---------------------+-----------+
|Asterisk (hash, array) | `$.one.*` `$.one[*]`| Y         |
+-----------------------+---------------------+-----------+
|Python-slicing         | `$[1:-1:2]`         | Partial*  |
+-----------------------+---------------------+-----------+
|Eval binary filter     | `$[?(true)]`        | Partial** |
+-----------------------+---------------------+-----------+
|Eval index             | `$[('one')]`        | Partial** |
+-----------------------+---------------------+-----------+
|Recursive descent      | `..`                | N         |
+-----------------------+---------------------+-----------+

* Only step=1 supported now
** Very limited scripting language: string, integer and function calls
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
 * Recursive descent (supported by parser, need evaluator)
 * Eval filter / index - allow path expressions and operators `$[?(@.category)]`
 * Eval filter / index - allow binary operators `$[?(@.category!='reference')]`
* Support for alternative JSON representations (mochijson2, EEP-18)

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
