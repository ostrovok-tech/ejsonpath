-module(fixture_tests).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

%% Helpers

get_file(FileName) ->
    Path = filename:join("./test/fixtures/", FileName),
    {ok, Bin} = file:read_file(Path),
    jsx:decode(Bin, [return_maps]).

-define(check_doc(Doc, Pattern, Expected),
    ?assertEqual(Expected, ejsonpath:q(Pattern, Doc))).

%% Tests

simple_test() ->
    Doc = get_file("simple.json"),
    ?check_doc(Doc, "$.hello", {[<<"world">>],["$['hello']"]}).

simple_array_root_test() ->
    Doc = get_file("simple_array_root.json"),
    ?check_doc(Doc, "$.*", {[<<"hello">>,<<"work">>,1,2,3], ["$[0]","$[1]","$[2]","$[3]","$[4]"]}).

complicated_test() ->
    Doc = get_file("complicated.json"),
    ?check_doc(Doc, "$.name", {[<<"Korg The Destroyer">>], ["$['name']"]}),
    ?check_doc(Doc, "$.class", {[<<"Fighter">>],["$['class']"]}),
    ?check_doc(Doc, "$.awesome", {[true],["$['awesome']"]}),

    ?check_doc(Doc, "$.nested.name", {[<<"Korgy Korg">>],["$['nested']['name']"]}),
    ?check_doc(Doc, "$.nested.class", {[<<"Fighter">>],["$['nested']['class']"]}),
    ?check_doc(Doc, "$.nested.specialization", {[<<"Champion">>],["$['nested']['specialization']"]}),
    ?check_doc(Doc, "$.nested.awesome", {[true],["$['nested']['awesome']"]}),
    ?check_doc(Doc, "$.nested['awesome levels']", {[<<"over 9000">>],["$['nested']['awesome levels']"]}),

    ?check_doc(Doc, "$.stats.strength", {[22],["$['stats']['strength']"]}),
    ?check_doc(Doc, "$.stats.dexterity", {[12], ["$['stats']['dexterity']"]}),
    ?check_doc(Doc, "$.stats.constitution", {[12], ["$['stats']['constitution']"]}),
    ?check_doc(Doc, "$.stats.resistance", {[12], ["$['stats']['resistance']"]}),
    ?check_doc(Doc, "$.stats.intelligence", {[8], ["$['stats']['intelligence']"]}),

    {BashOwnFace,_} = ejsonpath:q("$.skills['bash own face']", Doc),
    ?assertNotEqual([], BashOwnFace),
    ?check_doc(Doc, "$.skills['bash own face']['effects']['aoe buff']['area']", 
                {[<<"tiles">>], ["$['skills']['bash own face']['effects']['aoe buff']['area']"]}),
    ?check_doc(Doc, "$.skills['cooking']['description']",  {[<<"PC is able to cook tasty meals">>],["$['skills']['cooking']['description']"]}),

    ?check_doc(Doc, "$['fallen enemies']", {[[<<"evil lord">>,<<"XXxx 3V1L L0RD xxXX">>,<<"reason">>,
                    <<"Greg the Evil Guy">>,<<"too many goblins">>,<<"imps">>,
                    <<"trolls">>,<<"beehive (it was a bad time)">>,5,false,
                    <<"dignity">>]],
                  ["$['fallen enemies']"]}),
    ok.

stringified_inside_json_test() ->
    %% Testing in the odd case that your JSON contains JSON
    Doc = get_file("complicated.json"),
    ?check_doc(Doc, "$.metadata.game_difficulty", {[<<"ultra super permadeath hardcore">>],
                  ["$['metadata']['game_difficulty']"]}),

    {[Result],_} = ejsonpath:q("$.metadata.self", Doc),
    
    StringExistsIn =
        fun(Subject, String) ->
                case re:run(Subject, String) of
                    {match, _} -> ?assert(true);
                    nomatch -> ?assert(false, "could not find string")
                end
        end,

    StringExistsIn(Result, "name"),
    StringExistsIn(Result, "Korg The Destroyer"),
    StringExistsIn(Result, "cooking"),
    StringExistsIn(Result, "bash own face"),
    StringExistsIn(Result, "Fighter"),
    StringExistsIn(Result, "permadeath hardcore").

numbers_test() ->
    %% mainly jiffy concerns, but this is here for sanity and
    %% safeguarding against possible in between bad things (TM).
    Doc = get_file("numbers.json"),
    ?check_doc(Doc, "$.t_000", {[1],["$['t_000']"]}),
    ?check_doc(Doc, "$.t_001", {[-0],["$['t_001']"]}),
    ?check_doc(Doc, "$.t_002", {[-12],["$['t_002']"]}),
    ?check_doc(Doc, "$.t_003", {[1.23e-7],["$['t_003']"]}),
    ?check_doc(Doc, "$.t_004", {[-1.23e-7],["$['t_004']"]}),
    ?check_doc(Doc, "$.t_005", {[1.123e10],["$['t_005']"]}),
    ?check_doc(Doc, "$.t_006", {[1.123e-10],["$['t_006']"]}),
    ?check_doc(Doc, "$.t_007", {[1.0e-5],["$['t_007']"]}),
    ?check_doc(Doc, "$.t_008", {[1.0e-5],["$['t_008']"]}),
    ?check_doc(Doc, "$.t_009", {[1.0e9],["$['t_009']"]}).
