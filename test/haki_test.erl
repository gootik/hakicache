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

simple_bad_key_test() ->
    bad_key = haki:get(blah).

snapshot_test() ->
    Data = "SNAPSHOT",
    ok = haki:cache(test_snapshot, Data, #{save_snapshot => true}),

    FileName = atom_to_list(haki_compiler:mod_name(test_snapshot)) ++ ".beam",
    ?assert(filelib:file_size(FileName) > 0),

    file:delete(FileName).

snapshot_load_test() ->
    Data = "SNAPSHOT",
    ok = haki:cache(test_snapshot, Data, #{save_snapshot => true}),
    Data = haki:get(test_snapshot),

    ModName = haki_compiler:mod_name(test_snapshot),
    FileName = atom_to_list(ModName) ++ ".beam",
    ?assert(filelib:file_size(FileName) > 0),

    code:soft_purge(ModName),
    code:delete(ModName),
    bad_key = haki:get(test_snapshot),

    haki:load_snapshot(test_snapshot),
    Data = haki:get(test_snapshot),

    file:delete(FileName).

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
    haki:cache(test_record_syntax_key, Data, #{compiler => haki_syntax_compiler}),
    Data = haki:get(test_record_syntax_key).

record_asm_test() ->
    Data = #complex_record_test{},
    haki:cache(test_record_asm_key, Data, #{compiler => haki_asm_compiler}),
    Data = haki:get(test_record_asm_key).

-ifdef('HAS_BEAM_ASM').
record_beam_test() ->
    Data = #complex_record_test{},
    haki:cache(test_record_beam_key, Data, #{compiler => haki_beam_compiler}),
    Data = haki:get(test_record_beam_key).
-endif.

bucket_syntax_test() ->
    Data = #{
        a => ok,
        b => not_ok,
        c => #complex_record_test{}
    },

    haki:cache_bucket(test_bucket_syntax, Data, #{compiler => haki_syntax_compiler}),

    ok = haki:get(test_bucket_syntax, a),
    not_ok = haki:get(test_bucket_syntax, b),
    #complex_record_test{} = haki:get(test_bucket_syntax, c).

bucket_empty_syntax_test() ->
    Data = #{},
    haki:cache_bucket(test_bucket_empty_beam, Data, #{compiler => haki_syntax_compiler}),
    bad_key = haki:get(test_bucket_empty_beam, some_key).

-ifdef('HAS_BEAM_ASM').
bucket_beam_test() ->
    Data = #{
        a => ok,
        b => not_ok,
        c => #complex_record_test{}
    },

    haki:cache_bucket(test_bucket_beam, Data, #{compiler => haki_beam_compiler}),

    ok = haki:get(test_bucket_beam, a),
    not_ok = haki:get(test_bucket_beam, b),
    #complex_record_test{} = haki:get(test_bucket_beam, c).

bucket_empty_beam_test() ->
    Data = #{},
    haki:cache_bucket(test_bucket_empty_beam, Data, #{compiler => haki_beam_compiler}),
    bad_key = haki:get(test_bucket_empty_beam, some_key).
-endif.

snapshot_bucket_test() ->
    Data = #{
        a => ok,
        b => not_ok,
        c => #complex_record_test{}
    },

    ok = haki:cache_bucket(test_snapshot_bucket, Data, #{save_snapshot => true}),

    ok = haki:get(test_snapshot_bucket, a),
    not_ok = haki:get(test_snapshot_bucket, b),
    #complex_record_test{} = haki:get(test_snapshot_bucket, c),

    ModName = haki_compiler:mod_name(test_snapshot_bucket),
    FileName = atom_to_list(ModName) ++ ".beam",
    ?assert(filelib:file_size(FileName) > 0),

    code:soft_purge(ModName),
    code:delete(ModName),
    bad_bucket = haki:get(test_snapshot_bucket, a),

    haki:load_snapshot(test_snapshot_bucket),

    ok = haki:get(test_snapshot_bucket, a),
    not_ok = haki:get(test_snapshot_bucket, b),
    #complex_record_test{} = haki:get(test_snapshot_bucket, c),

    file:delete(FileName).
