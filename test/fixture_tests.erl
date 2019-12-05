-module(fixture_tests).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

%% Helpers

get_file(FileName) ->
    Path = filename:join("./test/fixtures/", FileName),
    {ok, Bin} = file:read_file(Path),
    jsx:decode(Bin, [return_maps]).

-define(check_doc(Doc, Pattern, Expected),
    ?assertEqual(Expected, ejsonpath:execute(Pattern, Doc))).

%% Tests

simple_test() ->
    Doc = get_file("simple.json"),
    ?check_doc(Doc, "$.hello", [<<"world">>]).

simple_array_root_test() ->
    Doc = get_file("simple_array_root.json"),
    ?check_doc(Doc, "$.*", [<<"hello">>,<<"work">>,1,2,3]).

complicated_test() ->
    Doc = get_file("complicated.json"),
    ?check_doc(Doc, "$.name", [<<"Korg The Destroyer">>]),
    ?check_doc(Doc, "$.class", [<<"Fighter">>]),
    ?check_doc(Doc, "$.awesome", [true]),

    ?check_doc(Doc, "$.nested.name", [<<"Korgy Korg">>]),
    ?check_doc(Doc, "$.nested.class", [<<"Fighter">>]),
    ?check_doc(Doc, "$.nested.specialization", [<<"Champion">>]),
    ?check_doc(Doc, "$.nested.awesome", [true]),
    ?check_doc(Doc, "$.nested['awesome levels']", [<<"over 9000">>]),

    ?check_doc(Doc, "$.stats.strength", [22]),
    ?check_doc(Doc, "$.stats.dexterity", [12]),
    ?check_doc(Doc, "$.stats.constitution", [12]),
    ?check_doc(Doc, "$.stats.resistance", [12]),
    ?check_doc(Doc, "$.stats.intelligence", [8]),

    BashOwnFace = ejsonpath:execute("$.skills['bash own face']", Doc),
    ?assertNotEqual([], BashOwnFace),
    ?check_doc(Doc, "$.skills['bash own face']['effects']['aoe buff']['area']", [<<"tiles">>]),
    ?check_doc(Doc, "$.skills['cooking']['description']", [<<"PC is able to cook tasty meals">>]),

    ?check_doc(Doc, "$['fallen enemies']",
               [[<<"evil lord">>, <<"XXxx 3V1L L0RD xxXX">>, <<"reason">>,
                <<"Greg the Evil Guy">>, <<"too many goblins">>, <<"imps">>,
                <<"trolls">>, <<"beehive (it was a bad time)">>, 5, false,
                <<"dignity">>]]).

stringified_inside_json_test() ->
    %% Testing in the odd case that your JSON contains JSON
    Doc = get_file("complicated.json"),
    ?check_doc(Doc, "$.metadata.game_difficulty", [<<"ultra super permadeath hardcore">>]),

    [Result] = ejsonpath:execute("$.metadata.self", Doc),

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
    ?check_doc(Doc, "$.t_000", [1]),
    ?check_doc(Doc, "$.t_001", [-0]),
    ?check_doc(Doc, "$.t_002", [-12]),
    ?check_doc(Doc, "$.t_003", [0.000000123]),
    ?check_doc(Doc, "$.t_004", [-0.000000123]),
    ?check_doc(Doc, "$.t_005", [1.123e10]),
    ?check_doc(Doc, "$.t_006", [1.123e-10]),
    ?check_doc(Doc, "$.t_007", [1.0e-5]),
    ?check_doc(Doc, "$.t_008", [1.0e-5]),
    ?check_doc(Doc, "$.t_009", [1.0e9]).
