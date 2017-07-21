%%%-------------------------------------------------------------------
%%% @doc Using the syntax compiler has an over head of creating an AST
%%%      and having the compiler to lint/expand records/optimize.
%%%      Moreover, building an AST and compiling has a huge overhead on
%%%      memory, if the data is large.
%%%
%%%      However for the most usecases of this library you are caching
%%%      a literal value, so if we hack the ASM file on the fly it should
%%%      produce a decent module that can be then compiled and loaded.
%%% @end
%%%-------------------------------------------------------------------
-module(haki_asm_compiler).

-behavior(haki_compiler).

-include("internal.hrl").
-include("types.hrl").

-export([
    compile/2,
    compile_bucket/2,
    asm_bucket_template/2
]).

-spec compile(cache_module_name(), cache_value()) -> compile_ret().
compile(ModName, Val) ->
    ModNameB = atom_to_binary(ModName, utf8),

    FileName = atom_to_list(ModName) ++ ".S",
    {ok, File} = file:open(FileName, write),

    ValB = io_lib:format(<<"~p~n">>, [Val]),
    Asm = asm_template(ModNameB, ValB),
    ?timed(file, file:write(File, Asm)),
    ok = file:close(File),

    Compile = compile:file(FileName, ?COMPILER_OPTS ++ [from_asm]),

    case Compile of
        {ok, Module, Bin} ->
            code:soft_purge(Module),
            F = atom_to_list(ModName) ++ ".erl",
            {module, Module} = code:load_binary(Module, F, Bin),

            file:delete(FileName);

        Error ->
            io:format(user, "~p~n", [Error]),
            Error
    end.

compile_bucket(ModName, Map) ->
    ModNameB = atom_to_binary(ModName, utf8),

    FileName = atom_to_list(ModName) ++ ".S",
    {ok, File} = file:open(FileName, write),

    Asm = asm_bucket_template(ModNameB, Map),
    ?timed(file, file:write(File, Asm)),
    ok = file:close(File),

    Compile = compile:file(FileName, ?COMPILER_OPTS ++ [from_asm]),

    case Compile of
        {ok, Module, Bin} ->
            code:soft_purge(Module),
            F = atom_to_list(ModName) ++ ".erl",
            {module, Module} = code:load_binary(Module, F, Bin),
            io:format("Compiled ~p~n", [Module]),

            file:delete(FileName);

        Error ->
            io:format(user, "Error ~p~n", [Error]),
            Error
    end.

-spec asm_template(binary(), binary()) -> binary().
asm_template(ModName, BinaryVal) ->
    Template = [
        %% Header
        <<"{module, ", ModName/binary, "}.  %% version = 0\n">>,
        <<"{exports, [{get,0},{info,0},{module_info,0},{module_info,1}]}.\n">>,
        <<"{attributes, []}.\n">>,
        <<"{labels, 7}.\n">>,

        %% Get
        <<"{function, get, 0, 2}.\n">>,
        <<"{label, 1}.\n">>,
        <<"{func_info,{atom,", ModName/binary, "},{atom,get},0}.\n">>,
        <<"{label, 2}.\n">>,
        [<<"{move,{literal,">>, BinaryVal, <<"},{x,0}}.\n">>],
        <<"return.\n">>,

        %% Module Info
        <<"{function, module_info, 0, 6}.\n">>,
        <<"{label,3}.\n">>,
        <<"{func_info,{atom,", ModName/binary, "},{atom,module_info},0}.\n">>,
        <<"{label,4}.\n">>,
        <<"{move,{atom,", ModName/binary, "},{x,0}}.\n">>,
        <<"{call_ext_only,1,{extfunc,erlang,get_module_info,1}}.\n">>,

        <<"{function, module_info, 1, 8}.\n">>,
        <<"{label,5}.\n">>,
        <<"{func_info,{atom,", ModName/binary, "},{atom,module_info},1}.\n">>,
        <<"{label,6}.\n">>,
        <<"{move,{x,0},{x,1}}.\n">>,
        <<"{move,{atom,", ModName/binary, "},{x,0}}.\n">>,
        <<"{call_ext_only,2,{extfunc,erlang,get_module_info,2}}.\n">>
    ],

    iolist_to_binary(Template).

asm_bucket_template(ModName, Map) ->
    MapSize = maps:size(Map),
    NumLabels = MapSize + 1
                + 3
                + 4,

    {KeySelects, _} = lists:foldl(
        fun(K, {IoList, I}) ->
            NewList = [IoList,
                       <<"{atom,">>, atom_to_binary(K, utf8), <<"},">>,
                       <<"{f,">>, integer_to_binary(3 + I), <<"}">>,
                       case I of
                           X when X =:= MapSize - 1 ->
                               <<>>;
                           _ ->
                               <<",">>
                       end
                ],
            {NewList, I + 1}
        end, {[], 0}, maps:keys(Map)),

    {KeyValueLabels, _} = maps:fold(
        fun(_, V, {IoList, I}) ->
            B = io_lib:format("~p", [V]),
            NewList = [IoList,
                       <<"{label,">>, integer_to_binary(3 + I), <<"}.\n">>,
                       [<<"{move,{literal,">>, B, <<"},{x,0}}.\n">>],
                       <<"return.\n">>],

            {NewList, I + 1}
        end, {[], 0}, Map),

    Template = [
        %% Header
        <<"{module, ", ModName/binary, "}.  %% version = 0\n">>,
        <<"{exports, [{get,1},{module_info,0},{module_info,1}]}.\n">>,
        <<"{attributes, []}.\n">>,
        [<<"{labels,">>, integer_to_binary(NumLabels), <<"}.\n">>],

        %% Get
        <<"{function, get, 1, 2}.\n">>,
        <<"{label, 1}.\n">>,
        <<"{func_info,{atom,", ModName/binary, "},{atom,get},1}.\n">>,
        <<"{label, 2}.\n">>,
        [<<"{test, is_atom, {f, ">>, integer_to_binary(3 + MapSize), <<"}, [{x, 0}]}.\n">>],
        [<<"{select_val,{x,0},{f, ">>, integer_to_binary(3 + MapSize), <<"},{list,[">>],
        KeySelects,
        <<"]}}.\n">>,

        KeyValueLabels,
        <<"{label,">>, integer_to_binary(3 + MapSize), <<"}.\n">>,
        <<"{move,{atom,key_not_found},{x,0}}.\n">>,
        <<"return.\n">>,

        %% Module Info
        <<"{function, module_info, 0, 6}.\n">>,
        <<"{label,">>, integer_to_binary(3 + MapSize + 1), <<"}.\n">>,
        <<"{func_info,{atom,", ModName/binary, "},{atom,module_info},0}.\n">>,
        <<"{label,">>, integer_to_binary(3 + MapSize + 2), <<"}.\n">>,
        <<"{move,{atom,", ModName/binary, "},{x,0}}.\n">>,
        <<"{call_ext_only,1,{extfunc,erlang,get_module_info,1}}.\n">>,

        <<"{function, module_info, 1, 8}.\n">>,
        <<"{label,">>, integer_to_binary(3 + MapSize + 3), <<"}.\n">>,
        <<"{func_info,{atom,", ModName/binary, "},{atom,module_info},1}.\n">>,
        <<"{label,">>, integer_to_binary(3 + MapSize + 4), <<"}.\n">>,
        <<"{move,{x,0},{x,1}}.\n">>,
        <<"{move,{atom,", ModName/binary, "},{x,0}}.\n">>,
        <<"{call_ext_only,2,{extfunc,erlang,get_module_info,2}}.\n">>
    ],

    iolist_to_binary(Template).