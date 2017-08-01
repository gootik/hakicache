%%%-------------------------------------------------------------------
%%% @doc NOTE: Super hacky (but fast) at the moment. If you want to use
%%%      this compiler you have to force it by forcing it. For example:
%%%      haki:cache(K, V, haki_beam_compiler).
%%%
%%%      TODO: Explain what's going on.
%%% @end
%%%-------------------------------------------------------------------
-module(haki_beam_compiler).

-behavior(haki_compiler).

-include("internal.hrl").
-include("types.hrl").

-export([
    compile/2,
    compile_bucket/2
]).


-spec compile(cache_module_name(), cache_value()) -> compile_ret().
compile(ModName, Val) ->
    Compile = beam_asm(ModName, Val),

    case Compile of
        {ok, Bin} ->
            code:soft_purge(ModName),
            F = atom_to_list(ModName) ++ ".erl",
            {module, ModName} = code:load_binary(ModName, F, Bin),

            {ok, Bin};
        Error ->
            error_logger:error_msg("[hakicache] - Could not build module: ~p", [Error]),

            Error
    end.

compile_bucket(ModName, Val) ->
    Compile = beam_asm_bucket(ModName, Val),

    case Compile of
        {ok, Bin} ->
            code:soft_purge(ModName),
            F = atom_to_list(ModName) ++ ".erl",
            {module, ModName} = code:load_binary(ModName, F, Bin),

            {ok, Bin};
        Error ->
            error_logger:error_msg("[hakicache] - Could not build module: ~p", [Error]),

            Error
    end.


-spec beam_asm(cache_module_name(), cache_value()) -> {ok, binary()} | error.
beam_asm(ModName, Val) ->
    Code = {ModName,
            [{get, 0}, {module_info, 0}, {module_info, 1}],
            [],
            [{function, get, 0, 2, [
                {label, 1},
                {func_info, {atom, ModName}, {atom, get}, 0},
                {label, 2},
                {move,
                 {literal, Val},
                 {x,0}},
                return]},
             {function,module_info,0,4,
              [{label,3},
               {func_info,{atom,ModName},{atom,module_info},0},
               {label,4},
               {move,{atom,ModName},{x,0}},
               {call_ext_only,1,{extfunc,erlang,get_module_info,1}}]},
             {function,module_info,1,6,
              [{label,5},
               {func_info,{atom,ModName},{atom,module_info},1},
               {label,6},
               {move,{x,0},{x,1}},
               {move,{atom,ModName},{x,0}},
               {call_ext_only,2,{extfunc,erlang,get_module_info,2}}]}],
            7},
    Chunks = [],
    Source = atom_to_list(ModName) ++ ".S",
    Opts = ?COMPILER_OPTS,
    CompilerOpts = ?COMPILER_OPTS,

    beam_asm:module(Code, Chunks, Source, Opts, CompilerOpts).

beam_asm_bucket(ModName, Map) ->
    NumKeys = maps:size(Map),

    {SelectList, _} = maps:fold(
        fun(Key, _, {L, I}) ->
            NewList = L ++ [{atom, Key}, {f, I + 3}],
            {NewList, I + 1}
        end, {[], 0}, Map),

    {KeyLabels, _} = maps:fold(
        fun(_, Val, {Labels, I}) ->
            NewLabels = Labels ++ [
                {label, I + 3},
                {move, {literal, Val}, {x, 0}},
                return],

            {NewLabels, I + 1}
        end, {[], 0}, Map),

    UnknownKeyLabel = [
        {label, NumKeys + 3},
        {move, {atom, bad_key}, {x, 0}},
        return
    ],

    Code = {ModName,
            [{get, 1}, {module_info, 0}, {module_info, 1}],
            [],
            [{function, get, 1, 2, [
                {label, 1},
                {func_info, {atom, ModName}, {atom, get}, 1},
                {label, 2},
                {test, is_atom, {f, NumKeys + 3}, [{x, 0}]},
                {select_val,
                 {x,0},
                 {f, NumKeys + 3},
                 {list, SelectList}}
                ] ++ KeyLabels ++ UnknownKeyLabel},
             {function,module_info,0,4,
              [{label, NumKeys + 3 + 1},
               {func_info,{atom,ModName},{atom,module_info},0},
               {label, NumKeys + 3 + 2},
               {move,{atom,ModName},{x,0}},
               {call_ext_only,1,{extfunc,erlang,get_module_info,1}}]},
             {function,module_info,1,6,
              [{label,NumKeys + 3 + 3},
               {func_info,{atom,ModName},{atom,module_info},1},
               {label,NumKeys + 3 + 4},
               {move,{x,0},{x,1}},
               {move,{atom,ModName},{x,0}},
               {call_ext_only,2,{extfunc,erlang,get_module_info,2}}]}],
            NumKeys + 3 + 5},

    Chunks = [],
    Source = atom_to_list(ModName) ++ ".S",
    Opts = ?COMPILER_OPTS,
    CompilerOpts = ?COMPILER_OPTS,

    beam_asm:module(Code, Chunks, Source, Opts, CompilerOpts).