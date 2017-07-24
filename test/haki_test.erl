-module(haki_test).

-include_lib("eunit/include/eunit.hrl").

-record(complex_record_test, {
    some_list = [1, 2, 3],
    map = #{
        a => 'A-OK'
    },
    tuple = {1, 2, 3},
    string = "1234",
    binary = <<"1234">>
}).

simple_test() ->
    Data = "MY_DATA",
    haki:cache(test_key, Data),
    Data = haki:get(test_key).

rewrite_test() ->
    Data = "MY_DATA",
    haki:cache(test_key, Data),
    Data = haki:get(test_key),

    Data2 = "MY_DATA_2",
    haki:cache(test_key, Data2),
    Data2 = haki:get(test_key).

record_test() ->
    Data = #complex_record_test{},
    haki:cache(test_record_key, Data),
    Data = haki:get(test_record_key).

record_large_test() ->
    Data = [#complex_record_test{} || _ <- lists:seq(1, 2000)],
    haki:cache(test_record_large_key, Data),
    Data = haki:get(test_record_large_key).

record_large_unicode_test() ->
    Data = [#complex_record_test{binary = <<"∆">>} || _ <- lists:seq(1, 2000)],
    haki:cache(test_record_large_unicode_key, Data),
    Data = haki:get(test_record_large_unicode_key).

record_huge_unicode_test() ->
    Data = [#complex_record_test{binary = <<"∆">>} || _ <- lists:seq(1, 20000)],
    haki:cache(test_record_large_unicode_key, Data),
    Data = haki:get(test_record_large_unicode_key).

record_syntax_test() ->
    Data = #complex_record_test{},
    haki:cache(test_record_syntax_key, Data, haki_syntax_compiler),
    Data = haki:get(test_record_syntax_key).

record_asm_test() ->
    Data = #complex_record_test{},
    haki:cache(test_record_asm_key, Data, haki_asm_compiler),
    Data = haki:get(test_record_asm_key).

record_beam_test() ->
    Data = #complex_record_test{},
    haki:cache(test_record_beam_key, Data, haki_beam_compiler),
    Data = haki:get(test_record_beam_key).