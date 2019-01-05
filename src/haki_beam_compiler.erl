%%%-------------------------------------------------------------------
%%% @doc NOTE: Super hacky (but fast) at the moment. If you want to use
%%%      this compiler you have to force it by setting the compiler in
%%%      the options. For example:
%%%      haki:cache(K, V, #{compiler => haki_beam_compiler}).
%%%
%%%      TODO: Explain what's going on.
%%% @end
%%%-------------------------------------------------------------------
-module(haki_beam_compiler).

-behavior(haki_compiler).

-include("internal.hrl").
-include("types.hrl").

-export([
    compile/3,
    compile_bucket/3
]).

-spec compile(cache_module_name(), cache_value(), cache_options()) -> compile_ret().
compile(ModName, Val, _Options) ->
    Compile = beam_asm(ModName, Val),

    case Compile of
        {ok, Bin} ->
            code:soft_purge(ModName),

            F = atom_to_list(ModName) ++ ".erl",
            {module, ModName} = code:load_binary(ModName, F, Bin),

            {ok, Bin};
        Error ->
            error_logger:error_msg("[hakicache_beam_compiler] - Could not build module: ~p", [Error]),

            Error
    end.

-spec compile_bucket(cache_module_name(), cache_bucket_value(), cache_options()) -> compile_ret().
compile_bucket(ModName, Val, _Options) ->
    Compile = beam_asm_bucket(ModName, Val),

    case Compile of
        {ok, Bin} ->
            code:soft_purge(ModName),

            F = atom_to_list(ModName) ++ ".erl",
            {module, ModName} = code:load_binary(ModName, F, Bin),

            {ok, Bin};
        Error ->
            error_logger:error_msg("[hakicache_beam_compiler] - Could not build module: ~p", [Error]),

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
    Opts = ?COMPILER_OPTS,
    CompilerOpts = ?COMPILER_OPTS,

    call_asm_module(ModName, Code, Chunks, Opts, CompilerOpts).

-spec beam_asm_bucket(cache_module_name(), cache_bucket_value()) -> {ok, binary()} | error.
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

    UnknownKeyLabel = [{label, NumKeys + 3},
                       {move, {atom, bad_key}, {x, 0}},
                       return],

    GetFunction = case NumKeys of
        0 ->
          [{move,{atom,bad_key},{x,0}},
           return];
        _ ->
          [{test, is_atom, {f, NumKeys + 3}, [{x, 0}]},
           {select_val,
             {x,0},
             {f, NumKeys + 3},
             {list, SelectList}}] ++ KeyLabels ++ UnknownKeyLabel
    end,

    ModuleInfoPadding = case NumKeys of
        0 -> 0;
        _ -> 1
    end,

    Code = {ModName,
            [{get, 1}, {module_info, 0}, {module_info, 1}],
            [],
            [{function, get, 1, 2, [
                {label, 1},
                {func_info, {atom, ModName}, {atom, get}, 1},
                {label, 2}] ++ GetFunction},
             {function,module_info, 0, NumKeys + 3 + ModuleInfoPadding + 1,
              [{label, NumKeys + 3 + ModuleInfoPadding},
               {func_info,{atom,ModName},{atom,module_info},0},
               {label, NumKeys + 3 + ModuleInfoPadding + 1},
               {move,{atom,ModName},{x,0}},
               {call_ext_only,1,{extfunc,erlang,get_module_info,1}}]},
             {function,module_info, 1, NumKeys + 3 + ModuleInfoPadding + 3,
              [{label, NumKeys + 3 + ModuleInfoPadding + 2},
               {func_info,{atom,ModName},{atom,module_info},1},
               {label, NumKeys + 3 + ModuleInfoPadding + 3},
               {move, {x,0},{x,1}},
               {move, {atom,ModName},{x,0}},
               {call_ext_only,2,{extfunc,erlang,get_module_info,2}}]}],
            NumKeys + 3 + ModuleInfoPadding + 4},

    Chunks = [],
    Opts = ?COMPILER_OPTS,
    CompilerOpts = ?COMPILER_OPTS,

    call_asm_module(ModName, Code, Chunks, Opts, CompilerOpts).

-ifdef('OTP_21_PLUS').
call_asm_module(_ModName, Code, Chunks, Opts, CompilerOpts) ->
    beam_asm:module(Code, Chunks, Opts, CompilerOpts).
-else.
call_asm_module(ModName, Code, Chunks, Opts, CompilerOpts) ->
    Source = atom_to_list(ModName) ++ ".S",
    beam_asm:module(Code, Chunks, Source, Opts, CompilerOpts).
-endif.
