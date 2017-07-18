-module(estatic_test).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    Data = "MY_DATA",
    estatic:cache(test_key, Data),
    Data = estatic:get(test_key).

rewrite_test() ->
    Data = "MY_DATA",
    estatic:cache(test_key, Data),
    Data = estatic:get(test_key),

    Data2 = "MY_DATA_2",
    estatic:cache(test_key, Data2),
    Data2 = estatic:get(test_key).

